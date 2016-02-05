{-# LANGUAGE
    TupleSections
    #-}

module ConstraintWitness.Plugin where

-- external
import GHC.TcPluginM.Extra (evByFiat, lookupModule, lookupName)
import Data.Maybe (mapMaybe)

-- GHC API
import GhcPlugins   (Plugin(..), defaultPlugin)
import TcRnTypes    (Ct, TcPlugin(..), TcPluginResult(..), ctEvidence, ctEvPred)
import FastString   (fsLit)
import OccName      (mkTcOcc)
import Module       (mkModuleName)
import TcPluginM    (TcPluginM, tcLookupTyCon)

-- The actual plugin we're exporting
plugin :: Plugin
plugin = defaultPlugin {
    tcPlugin = const $ Just TcPlugin {
        tcPluginInit = findWitnessTypeFamily,
        tcPluginSolve = solveWitness,
        tcPluginStop = const (return ())
        }
    }

-- | Find the type family constructor for Witness, and find Data.Type.Equality ((:~:), Refl)
findWitnessTypeFamily :: TcPluginM (TyCon, TyCon)
findWitnessTypeFamily = do
    md <- lookupModule witnessModule witnessPackage
    witnessTcNm <- lookupName md (mkTcOcc "Witness")
    witness <- tcLookupTyCon witnessTcNm
    dte <- lookupModule dteModule basePackage
    eqTcNm <- lookupName dte (mkTcOcc ":~:")
    eq <- tcLookupTyCon eqTcNm
    return (witness, eq)
  where
    witnessModule = mkModuleName "ConstraintWitness.Internal"
    witnessPackage = fsLit "constraint-witness"
    dteModule = mkModuleName "Data.Type.Equality"
    basePackage = fsLit "base"
    

-- | The actual constraint solver
solve :: (TyCon, TyCon) -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solve _ givens deriveds [] = return (TcPluginOk [] [])
solve (witness, eq) givens deriveds wanteds = do
    
    
    