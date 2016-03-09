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
import Data.STRef           (readSTRef, newSTRef, writeSTRef)
import Control.Monad.ST     (runST)
import System.IO.Unsafe     (unsafePerformIO)

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
import Type         (EqRel(..), PredTree (..), classifyPredType, mkEqPred, typeKind)
import TypeRep      (Type(..), PredType)
import Kind         (constraintKind, liftedTypeKind)
import DataCon      (dataConTyCon, dataConName, DataCon)
import StaticFlags  (v_opt_C_ready) -- THIS IS A HORRIBLE HACK BECAUSE THE API SUCKS
import Outputable   (showPpr)
import DynFlags     (unsafeGlobalDynFlags)

witnessPlugin :: TcPlugin
witnessPlugin = TcPlugin {
    tcPluginInit = findTyCons,
    tcPluginSolve = solveWitness,
    tcPluginStop = const (return ())
    }

-- Holds all the type constructors, values etc. that are needed in this plugin.
-- Mostly used with wildcard syntax; that way, we can sort-of treat the fields as top-level constants.
data State = State { witness    :: TyCon -- ^ constraint-witness:ConstraintWitness.Internal (Witness). The magical type family.
                   -- , iso        :: TyCon -- ^ constraint-witness:ConstraintWitness.Internal  (Isomorphism)
                   , eq         :: TyCon -- ^ base:Data.Type.Equality ((:~:)), the witness datatype for equality constraints.
                   , hlist      :: TyCon -- ^ HList:Data.HList.HList (HList), a strongly-typed heterogenous list. The witness for tupled constraints.
                   , list       :: TyCon -- ^ ghc-prim:GHC.Types ([]), the standard list type.
                   , cons       :: TyCon -- ^ ghc-prim:GHC.Types ((:)), lifted.
                   , nil        :: Type  -- ^ ghc-prim:GHC.Types ([]), the lifted constructor (NOT the type!)
                   , unit       :: Type  -- ^ ghc-prim:GHC.Tuple (()), the unit tuple
                   }

data WitnessEquality = WitnessEquality {
    we_loc     :: CtLoc,
    we_precond :: (Type, Type),
    we_ev      :: EvTerm,
    we_proven :: Ct }

-- | Find the type family constructors we need.
findTyCons :: TcPluginM State
findTyCons = do
    -- This is an utterly horrible hack. I know. But there's no reinitializeGlobals equivalent for TcPluginM.
    tcPluginIO $ writeIORef v_opt_C_ready True
    witness     <- summonTc "constraint-witness"    "ConstraintWitness.Internal"    "Witness"
    iso         <- summonTc "constraint-witness"    "ConstraintWitness.Internal"    "Isomorphism"
    eq          <- summonTc "base"                  "Data.Type.Equality"            ":~:"
    hlist       <- summonTc "HList"                 "Data.HList.HList"              "HList"
    list        <- summonTc "ghc-prim"              "GHC.Types"                     "[]"
    unit'       <- summonTc "ghc-prim"              "GHC.Tuple"                     "()"
    let unit = TyConApp unit' []
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

-- | The actual constraint solver. Well, it's mostly just an interface to toWitnessEquality
solveWitness :: State -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveWitness _ givens deriveds [] = return (TcPluginOk [] [])
solveWitness (State{..}) givens deriveds wanteds = TcPluginOk solved <$> reduced
  where
    touched   :: [WitnessEquality]
    touched = mapMaybe (toWitnessEquality State{..}) wanteds
    solved :: [(EvTerm, Ct)]
    solved = [(we_ev, we_proven) | WitnessEquality{..} <- touched]
    wanted :: [(CtLoc, (Type, Type))]
    wanted = [(we_loc, we_precond) | WitnessEquality{..} <- touched]
    reduced :: TcPluginM [Ct]
    reduced = sequence $ mapMaybe satisfyPrecond wanted
    satisfyPrecond :: (CtLoc, (Type, Type)) -> Maybe (TcPluginM Ct)
    satisfyPrecond (loc, (lhs, rhs)) =
        if foldr (\given found -> case classifyPredType $ ctEvPred $ ctEvidence given of
            EqPred NomEq l r -> found || (lhs == l && rhs == r) || (lhs == r && rhs == l)
            _                -> found) False givens
        then Nothing
        else Just (CNonCanonical <$> newWanted loc (mkEqPred lhs rhs))

-- | Check if a constraint is an equality constraint with (Witness a) on one or both sides
--   @'toWitnessEquality' s ct@ is either:
--     - @'Right' (ct, (equ, fiat))@ when ct is an equality constraint where one or both sides are of the form @'Witness' cxt@.
--       In that case, @equ@ is the original equality constraint with values of the form @'Witness' cxt@ replaced by the witness for cxt.
--       @fiat@ is an evidence term that basically says "Magic!" and that's it.
--     - @'Left' ct@, when the above isn't the case.
toWitnessEquality :: State -- ^ The type constructors we need
                  -> Ct    -- ^ The constraint to pattern-match on
                  -> Maybe WitnessEquality
toWitnessEquality (State{..}) ct = 
    case classifyPredType $ ctEvPred $ ctEvidence ct of
        EqPred NomEq lhs rhs ->
            runST $ do
                pass <- newSTRef True
                lhs' <- case lhs of
                    TyConApp con [arg] | con == witness -> writeSTRef pass False >> return (constructWitness State{..} arg)
                    _                                   -> return lhs
                rhs' <- case rhs of
                    TyConApp con [arg] | con == witness -> writeSTRef pass False >> return (constructWitness State{..} arg)
                    _                                   -> return rhs
                readSTRef pass >>= \f -> if f
                    then log__ "toWitnessEquality found no Witness constructors" $ return Nothing
                    else log__ "toWitnessEquality found Witness constructor(s)"  $ return $ Just WitnessEquality {
                        we_proven  = ct,
                        we_ev      = evByFiat "magical type family: Witness" lhs rhs,
                        we_loc     = ctLoc ct,
                        we_precond = (lhs', rhs')}
        _ -> Nothing

constructWitness :: State -- ^ The type constructors we need
                 -> Type  -- ^ The type to make a witness of. Must have kind Constraint.
                 -> Type
constructWitness (State{..}) arg
    | typeKind arg == constraintKind =
        case classifyPredType arg of
            ClassPred clas args -> error "TODO: implement dictionary thingy"
            EqPred NomEq l r    -> log__ "transforming (a ~ b) to (a :~: b)" $ TyConApp eq [l, r]
            TuplePred []        -> log__ "transforming an empty tuple constraint" $ unit
            TuplePred tys       -> log__ "transforming a non-empty tuple constraint" $ TyConApp hlist [promotedList $ map (constructWitness State{..}) tys]
    | otherwise = error "argument is not a constraint"
  where
    promotedList :: [Type] -> Type
    promotedList = foldr (\head spine -> TyConApp cons [head, spine]) nil

log__ :: String -> a -> a
log__ s v = unsafePerformIO (putStrLn s) `seq` v