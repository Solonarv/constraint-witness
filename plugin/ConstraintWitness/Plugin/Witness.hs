{-# LANGUAGE
    TupleSections,
    RecordWildCards,
    ViewPatterns
    #-}

module ConstraintWitness.Plugin.Witness (
    witnessPlugin
    ) where

-- external
import GHC.TcPluginM.Extra  (evByFiat, lookupModule, lookupName, newWanted)
import Data.Maybe           (mapMaybe)
import Data.Either          (partitionEithers)
import Data.Bifunctor       (bimap, second)
import Data.IORef           (writeIORef)
import Control.Monad        (guard)

-- GHC API
import GhcPlugins   (Plugin(..), defaultPlugin)
import TcRnTypes    (Ct(..), TcPlugin(..), TcPluginResult(..), ctEvidence, ctEvPred, CtEvidence(..), CtLoc, ctLoc)
import FastString   (fsLit)
import OccName      (mkTcOcc, mkOccName, dataName, occNameString)
import Name         (nameOccName, nameUnique)
import Module       (mkModuleName)
import TcPluginM    (TcPluginM, tcLookupTyCon, tcLookupDataCon, tcPluginIO)
import TcEvidence   (EvTerm)
import TyCon        (TyCon, tyConDataCons, tyConRoles, mkPromotedDataCon)
import Type         (EqRel (NomEq), PredTree (..), classifyPredType, mkEqPred, typeKind)
import TypeRep      (Type(..), PredType)
import Kind         (constraintKind, liftedTypeKind)
import DataCon      (dataConTyCon, dataConName, DataCon)
import StaticFlags  (v_opt_C_ready) -- THIS IS A HORRIBLE HACK BECAUSE THE API SUCKS

witnessPlugin :: TcPlugin
witnessPlugin = TcPlugin {
    tcPluginInit = findTyCons,
    tcPluginSolve = solveWitness,
    tcPluginStop = const (return ())
    }

-- Holds all the type constructors, values etc. that are needed in this plugin.
-- Mostly used with wildcard syntax; that way, we can sort-of treat the fields as top-level constants.
data State = State { witness    :: TyCon -- ^ constraint-witness:ConstraintWitness.Internal (Witness). The magical type family.
                   , eq         :: TyCon -- ^ base:Data.Type.Equality ((:~:)), the witness datatype for equality constraints.
                   , hlist      :: TyCon -- ^ HList:Data.HList.HList (HList), a strongly-typed heterogenous list. The witness for tupled constraints.
                   , list       :: TyCon -- ^ ghc-prim:GHC.Types ([]), the standard list type.
                   , cons       :: TyCon -- ^ ghc-prim:GHC.Types (:), lifted.
                   , nil        :: Type  -- ^ ghc-prim:GHC.Types ([]), the lifted constructor (NOT the type!)
                   }

-- | Find the type family constructors we need.
findTyCons :: TcPluginM State
findTyCons = do
    -- This is an utterly horrible hack. I know. But there's no reinitializeGlobals equivalent for TcPluginM.
    tcPluginIO $ writeIORef v_opt_C_ready True
    witness     <- summonTc "constraint-witness"    "ConstraintWitness.Internal"    "Witness"
    eq          <- summonTc "base"                  "Data.Type.Equality"            ":~:"
    hlist       <- summonTc "HList"                 "Data.HList.HList"              "HList"
    list        <- summonTc "ghc-prim"              "GHC.Types"                     "[]"
    let promote      :: DataCon -> TyCon
        promote con = let name   = dataConName con
                          unique = nameUnique name
                          kind   = TyConApp list [liftedTypeKind]
                          roles  = tyConRoles (dataConTyCon con)
                      in mkPromotedDataCon con name unique kind roles
        consts = tyConDataCons list
        -- This type is wired-in, so it's fine to hardfail the pattern matches
    let [flip TyConApp [] . promote ->
            nil]  = filter ((== "[]") . occNameString . nameOccName . dataConName) consts
        [promote ->
            cons] = filter ((== ":" ) . occNameString . nameOccName . dataConName) consts
    return $ State {..}
  where
    summonTc :: String -> String -> String -> TcPluginM TyCon
    summonTc pack mod name = tcLookupTyCon =<< flip lookupName (mkTcOcc name) =<< lookupModule (mkModuleName mod) (fsLit pack)

-- | The actual constraint solver
solveWitness :: State -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveWitness _ givens deriveds [] = return (TcPluginOk [] [])
solveWitness (State{..}) givens deriveds wanteds = case failed of
    [] -> TcPluginOk solved <$> traverse (fmap CNonCanonical . uncurry newWanted) new
    _  -> return $ TcPluginContradiction failed
  where
    failed :: [Ct]
    new    :: [(CtLoc, PredType)]
    solved :: [(EvTerm, Ct)]
    (failed, (new, solved)) = second unzip $ partitionEithers $ map (toWitnessEquality $ State{..}) wanteds

-- | Check if a constraint is an equality constraint with (Witness a) on one or both sides
--   @'toWitnessEquality' s ct@ is either:
--     - @'Right' (ct, (equ, fiat))@ when ct is an equality constraint where one or both sides are of the form @'Witness' cxt@.
--       In that case, @equ@ is the original equality constraint with values of the form @'Witness' cxt@ replaced by the witness for cxt.
--       @fiat@ is an evidence term that basically says "Magic!" and that's it.
--     - @'Left' ct@, when the above isn't the case.
toWitnessEquality :: State -- ^ The type constructors we need
                  -> Ct    -- ^ The constraint to pattern-match on
                  -> Either Ct ((CtLoc, PredType), (EvTerm, Ct))
toWitnessEquality (State{..}) ct = 
    case classifyPredType $ ctEvPred $ ctEvidence ct of
        EqPred NomEq lhs rhs ->
          let
            mbNewEq = do -- Maybe monad
                lhs' <- case lhs of
                    TyConApp con [arg] | con == witness -> constructWitness (State{..}) arg
                    _                                   -> Just lhs
                rhs' <- case rhs of
                    TyConApp con [arg] | con == witness -> constructWitness (State{..}) arg
                    _                                   -> Just rhs
                guard $ lhs' /= lhs || rhs' /= rhs
                Just ((ctLoc ct, mkEqPred lhs rhs), (evByFiat "magical-type-family:Witness" lhs rhs, ct))
          in
            case mbNewEq of
                Nothing -> Left ct
                Just result -> Right result
        _ -> Left ct

constructWitness :: State -- ^ The type constructors we need
                 -> Type  -- ^ The type to make a witness of. Must have kind Constraint.
                 -> Maybe Type
constructWitness (State{..}) arg
    | typeKind arg == constraintKind =
        case classifyPredType arg of
            ClassPred clas args -> Nothing -- TODO dictionary
            EqPred NomEq l r    -> Just $ TyConApp eq [l, r]
            TuplePred tys       -> Just $ TyConApp hlist [promotedList tys]
    | otherwise = Nothing
  where
    promotedList :: [Type] -> Type
    promotedList = foldr (\head spine -> TyConApp cons [head, spine]) nil