{-# LANGUAGE
    ExistentialQuantification,
    RecordWildCards,
    TupleSections
    #-}

module ConstraintWitness.TcPlugin.Simple (
    TyConLoc (..),
    SimpleTcPlugin (..),
    toTcPlugin
    ) where

-- external
import ConstraintWitness.TcPlugin.Instances
import GHC.TcPluginM.Extra  (lookupModule, lookupName, newWanted, evByFiat)
import Data.Traversable     (traverse)
import Data.Either          (partitionEithers)
import Data.Bifunctor       (bimap, second)

-- GHC
import GhcPlugins   (Plugin(..), defaultPlugin)
import OccName      (mkTcOcc)
import TcPluginM    (TcPluginM, tcLookupTyCon)
import TcEvidence   (EvTerm)
import TcRnTypes    (TcPlugin(..), TcPluginResult(..), CtEvidence(..), Ct(..), CtLoc, ctEvPred, ctEvidence, ctLoc)
import TyCon        (TyCon)
import Type         (EqRel (NomEq), PredTree (..), classifyPredType, mkEqPred)
import TypeRep      (Type, PredType)
import FastString   (fsLit)
import Module       (mkModuleName)

data TyConLoc = TyConLoc {
    tcl_package :: String,
    tcl_module  :: String,
    tcl_name    :: String
    }

data SimpleTcPlugin = forall s. SimpleTcPlugin {
    stp_tycons  :: [TyConLoc],
    stp_wraptcs :: [(TyConLoc, TyCon)] -> s,
    stp_mapping :: s -> [Type -> Type]
    }

toTcPlugin :: SimpleTcPlugin -> TcPlugin
toTcPlugin SimpleTcPlugin{..} = TcPlugin (fmap stp_wraptcs $ lookupCons stp_tycons)
                                         (useMapping . stp_mapping)
                                         (const $ return ())

lookupCons :: [TyConLoc] -> TcPluginM [(TyConLoc, TyCon)]
lookupCons conlocs = mapM lookupCon conlocs
  where
    lookupCon loc@TyConLoc{..} = do
        let pack = fsLit        tcl_package
        let mod  = mkModuleName tcl_module
        let name = mkTcOcc      tcl_name
        fmap (loc,) $ tcLookupTyCon =<< flip lookupName name =<< lookupModule mod pack

useMapping :: [Type -> Type] -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
useMapping mapping g d w = fmap mconcat $ traverse (solver g d w) mapping where
    solver givens deriveds []      f = return $ TcPluginOk [] []
    solver givens deriveds wanteds f = do
        case failed of
            [] -> TcPluginOk solved <$> traverse (fmap CNonCanonical . uncurry newWanted) new
            _  -> return $ TcPluginContradiction failed
      where
        failed :: [Ct]
        new    :: [(CtLoc, PredType)]
        solved :: [(EvTerm, Ct)]
        (failed, (new, solved)) = second unzip $ partitionEithers $ map (toMappedEquality f) wanteds

toMappedEquality :: (Type -> Type) -> Ct -> Either Ct ((CtLoc, PredType), (EvTerm, Ct))
toMappedEquality f ct = case classifyPredType $ ctEvPred $ ctEvidence ct of
    EqPred NomEq lhs rhs -> 
        let lhs' = f lhs
            rhs' = f rhs
        in  if lhs' /= lhs || rhs' /= rhs
            then Right ((ctLoc ct, mkEqPred lhs rhs), (evByFiat "tc-plugin:magical-type-conversion" lhs' rhs', ct))
            else Left ct