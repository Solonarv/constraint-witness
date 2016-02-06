{-# LANGUAGE
    ExistentialQuantification,
    RankNTypes,
    ImpredicativeTypes
    #-}

module ConstraintWitness.Plugin.Instances where

import Prelude hiding (init)

import Data.Monoid
import Data.List (union)
import Data.Traversable (traverse)
import Control.Monad (sequence, void)

import GhcPlugins ()
import TcRnTypes (TcPlugin(..), TcPluginResult(..))

-- | Straightforward plugin-result composition
instance Monoid TcPluginResult where
    mempty = TcPluginOk [] []
    mappend (TcPluginOk solved1 new1)    (TcPluginOk solved2 new2   ) = TcPluginOk (solved1 ++ solved2) (new1 ++ new2)
    mappend (TcPluginContradiction bad1) (TcPluginOk _       _      ) = TcPluginContradiction bad1
    mappend (TcPluginOk _       _)       (TcPluginContradiction bad2) = TcPluginContradiction bad2
    mappend (TcPluginContradiction bad1) (TcPluginContradiction bad2) = TcPluginContradiction (bad1 ++ bad2)

-- | This makes plugins composable.
--   plug1 <> plug2 will run both plugins at the same time *not* one after the other.
--   If either finds a contradiction, the composed plugin will also result in a contradiction.
--   If they both find a contradiction, the result is a contradiction where the bad constraints
--   consist of the bad ones for *both* plugins.
instance Monoid TcPlugin where
    mempty =
        TcPlugin (return ())
                 (\state given derived wanted -> return $ TcPluginOk [] [])
                 (\() -> return ())
    mappend (TcPlugin init1 solve1 stop1) (TcPlugin init2 solve2 stop2) =
        TcPlugin ((,) <$> (init1) <*> (init2))
                 (\(s1, s2) given derived wanted ->
                     mappend <$> solve1 s1 given derived wanted
                             <*> solve2 s2 given derived wanted)
                 (\(s1, s2) -> stop1 s1 >> stop2 s2)