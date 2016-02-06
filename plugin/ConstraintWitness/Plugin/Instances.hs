module ConstraintWitness.Plugin.Util where

import Data.Monoid
import Data.List (union)
import Control.Monad (sequence, traverse)

import GhcPlugins
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
        TcPlugin { init  = return ()
                 , stop  = \() -> return ()
                 , solve = \state given derived wanted -> return TcPluginOk [] [] }
    mappend plug1 plug2 =
        TcPlugin { init  = (,) <$> (init plug1) <*> (init plug2)
                 , stop  = \(s1, s2) -> stop plug1 s1 >> stop plug2 s2
                 , solve = \(s1, s2) given derived wanted ->
                     mappend <$> solve plug1 s1 given derived wanted
                             <*> solve plug2 s2 given derived wanted }
    -- | This implementation uses specialised folds "deeper down" in the plugin structure,
    --   so it's more efficient than the default @'foldr' 'mappend' 'mempty'@.
    --   It's not literally the same as the default, because it uses lists rather than nested tuples;
    --   but that's not exposed, it's hidden by the forall in TcPlugin's type. And it's more efficient.
    mconcat [] = mempty
    mconcat plugins =
        TcPlugin { init  = traverse init plugins
                 , stop  = sequence . zipWith stop plugins
                 , solve = \states given derived wanted ->
                     mconcat <$> sequence $ zipWith (\state plug -> solve plug given derived wanted) states plugins }