module ConstraintWitness.Plugin where

import ConstraintWitness.Plugin.Witness (witnessPlugin)
import ConstraintWitness.Plugin.Instances

import GhcPlugins (Plugin(..), defaultPlugin)

-- The actual plugin we're exporting
plugin :: Plugin
plugin = defaultPlugin {
    tcPlugin = const $ Just $ witnessPlugin
    }

