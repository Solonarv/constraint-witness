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
import Data.HList

import GHC.Prim (Constraint)

-- | Magical type family that turns a constraint into a (value-level) witness.
--   The rules are: 
--   Witness (a ~ b) -> a :~: b
--   Witness <a typeclass constraint> -> <a dictionary for that typeclass>
--   Witness (?p :: a) -> a
--   Witness (x, y, ..., z) -> (Witness x, Witness y, .., Witness z)
type family Witness (ct :: Constraint) :: * where
    Witness c = () -- Bogus equation so GHC doesn't choke

class IsTypeClass (ct :: Constraint) where
    type Dict ct :: *

class HasClasses (cts :: [Constraint]) where
    type Dicts cts :: [*]

instance HasClasses '[] where type Dicts '[] = '[]

instance {-# OVERLAPS #-} (IsTypeClass ct, HasClasses cts) => HasClasses (ct ': cts) where
    type Dicts (ct ': cts) = Dict ct ': Dicts cts

instance HasClasses cts => HasClasses (ct ': cts) where type Dicts (ct ': cts) = Dicts cts

-- | Tries to provide a canonical witness for the given constraint. This is Refl for equality constraints, the dictionary for typeclass constraints, the implicit parameter's value for ImplicitParams, and a HList of the component constraints' canonical witnesses for conjoined constrints.
canonicalWitness :: forall (ct :: Constraint). ct => Witness ct
canonicalWitness = undefined -- implemented by compiler plug in
{-# NOINLINE canonicalWitness #-}
