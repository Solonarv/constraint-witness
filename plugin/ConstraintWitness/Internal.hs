{-# LANGUAGE
    TypeFamilies,
    KindSignatures,
    ConstraintKinds,
    ExplicitNamespaces,
    GADTs,
    TypeOperators,
    DataKinds
    #-}

module ConstraintWitness.Internal (
    (:~:)(..),
    Witness
    ) where

import Data.Type.Equality ((:~:)(..))

import GHC.Prim (Constraint)

-- | Magical type family that turns a constraint into a (value-level) witness.
--   The rules are: 
--   Witness (a ~ b) -> a :~: b
--   Witness <a typeclass constraint> -> <a dictionary for that typeclass>
--   Witness (?p :: a) -> a
--   Witness (x, y, ..., z) -> (Witness x, Witness y, .., Witness z)
type family Witness (ct :: Constraint) :: * where
    Witness c = () -- Bogus equation so GHC doesn't choke

class HasDict (ct :: Constraint) where
    type Dict ct
    dict :: proxy ct -> Dict ct