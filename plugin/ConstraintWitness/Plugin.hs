{-# LANGUAGE
    TupleSections,
    RecordWildCards
    #-}

module ConstraintWitness.Plugin where

-- external
import GHC.TcPluginM.Extra (evByFiat, lookupModule, lookupName)
import Data.Maybe (mapMaybe)
import Data.Either (partitionEithers)
import Data.Bifunctor (bimap)
import Control.Monad (guard)

-- GHC API
import GhcPlugins   (Plugin(..), defaultPlugin)
import TcRnTypes    (Ct, TcPlugin(..), TcPluginResult(..), ctEvidence, ctEvPred)
import FastString   (fsLit)
import OccName      (mkTcOcc)
import Module       (mkModuleName)
import TcPluginM    (TcPluginM, tcLookupTyCon)
import TyCon        (TyCon)
import Type         (EqRel (NomEq), PredTree (EqPred), classifyPredType, mkEqPred, typeKind)
import TypeRep      (Type(..))

-- The actual plugin we're exporting
plugin :: Plugin
plugin = defaultPlugin {
    tcPlugin = const $ Just TcPlugin {
        tcPluginInit = findTyCons,
        tcPluginSolve = solveWitness,
        tcPluginStop = const (return ())
        }
    }

-- Holds all the type constructors, values etc. that are needed in this plugin.
-- Mostly used with wildcard syntax; that way, we can sort-of treat the fields as top-level constants.
data State = State { witness    :: TyCon -- ^ constraint-witness:ConstraintWitness.Internal (Witness). The magical type family.
                   , eq         :: TyCon -- ^ base:Data.Type.Equality ((:~:)), the witness datatype for equality constraints.
                   , constraint :: TyCon -- ^ ghc-prim:GHC.Prim (Constraint), the kind of constraints.
                   , hlist      :: TyCon -- ^ HList:Data.HList.HList (HList), a strongly-typed heterogenous list. The witness for tupled constraints.
                   , list       :: TyCon -- ^ ghc-prim:GHC.Types ([]), the standard list type.
                   }

-- | Find the type family constructors we need.
findTyCons :: TcPluginM State
findTyCons = do
    witness     <- summonTc "constraint-witness"    "ConstraintWitness.Internal"    "Witness"
    eq          <- summonTc "base"                  "Data.Type.Equality"            ":~:"
    constraint  <- summonTc "ghc-prim"              "GHC.Prim"                      "Constraint"
    hlist       <- summonTc "HList"                 "Data.HList.HList"              "HList"
    list        <- summonTc "ghc-prim"              "GHC.Types"                     "[]"
    return $ State {..}
  where
    summonTc :: String -> String -> String -> TcPluginM TyCon
    summonTc pack mod name = tcLookupTyCon <$> flip lookupName (mkTcOcc name) <$> lookupModule (mkModuleName mod) (fsLit pack)

-- | The actual constraint solver
solveWitness :: State -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveWitness _ givens deriveds [] = return (TcPluginOk [] [])
solveWitness (State{..}) givens deriveds wanteds = return $ case failed of
    [] -> TcPluginOk (mapMaybe (\c -> (,c) <$> evMagic c) solved) new
    _  -> TcPluginContradiction failed
  where
    failed, solved, new :: [Ct]
    (failed, (solved, new)) = second unzip $ partitionEithers $ map (toWitnessEquality witness eq) wanteds

-- | Check if a constraint is an equality constraint with (Witness a) on one or both sides
--   @'toWitnessEquality' s ct@ is either:
--     - @'Right' (ct, equ)@ when ct is an equality constraint where one or both sides are of the form @'Witness' cxt@.
--       In that case, @equ@ is the original equality constraint with values of the form @'Witness' cxt@ replaced by the witness for cxt.
--     - @'Left' ct@, when the above isn't the case.
toWitnessEquality :: State -- ^ The type constructors we need
                  -> Ct    -- ^ The constraint to pattern-match on
                  -> Either Ct (Ct, Ct)
toWitnessEquality (State{..}) ct = 
    case classifyPredType $ ctEvPred $ ctEvidence ct of
        EqPred NomEq lhs rhs ->
          let
            mbNewEq = do -- Maybe monad
                lhs' <- case lhs of
                    TyConApp con arg | con == witness -> constructWitness witness eq arg
                    _                                 -> Just lhs
                rhs' <- case rhs of
                    TyConApp con arg | con == witness -> constructWitness witness eq arg
                    _                                 -> Just rhs
                guard $ lhs' /= lhs || rhs' /= rhs
                Just $ lhs' `mkEqPred` rhs'
          in
            case mbNewEq of
                Nothing -> Left ct
                Just newEq -> Right (ct, newEq)
        _ -> Left ct

constructWitness :: State -- ^ The type constructors we need
                 -> Type  -- ^ The type to make a witness of. Must have kind Constraint.
                 -> Maybe Type
constructWitness (State{..})) arg = case typeKind arg of
    TyConApp con [] | con == constraint ->
        case classifyPredType arg of
            ClassPred clas args -> Nothing -- TODO dictionary
            EqPred NomEq l r    -> Just $ TyConApp eq [l, r]
            TuplePred tys       -> -- TODO build a HList
    _ -> Nothing