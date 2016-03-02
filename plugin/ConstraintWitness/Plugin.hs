module ConstraintWitness.Plugin where

import ConstraintWitness.Plugin.Witness (witnessPlugin)
import ConstraintWitness.TcPlugin.Instances

import GhcPlugins (Plugin(..), defaultPlugin, CommandLineOption, CoreToDo, CoreM, reinitializeGlobals)

-- The actual plugin we're exporting
plugin :: Plugin
plugin = defaultPlugin {
    installCoreToDos = reinit,
    tcPlugin = const $ Just $ witnessPlugin
    }

reinit :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
reinit _ todos = reinitializeGlobals >> return todos