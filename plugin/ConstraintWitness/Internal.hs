{-# LANGUAGE
    TypeFamilies,
    KindSignatures,
    ConstraintKinds,
    ExplicitNamespaces,
    GADTs,
    TypeOperators,
    DataKinds,
    RankNTypes,
    AllowAmbiguousTypes,
    RecordWildCards
    #-}

module ConstraintWitness.Internal (
    (:~:)(..),
    Witness,
    canonicalWitness,
    expose, useWitness
    ) where

import Data.Type.Equality ((:~:)(..))
import Data.HList

import GHC.Prim (Constraint)

-- | Magical type family that turns a constraint into a (value-level) witness.
--   The rules are:
--   Witness () -> ()
--   Witness (a ~ b) -> a :~: b
--   Witness <a typeclass constraint> -> <a dictionary for that typeclass>
--   Witness (?p :: a) -> a
--   Witness (x, y, ..., z) -> HList '[Witness x, Witness y, .., Witness z]
type family Witness (ct :: Constraint) :: * where
    Witness () = () -- Bogus equation so GHC doesn't choke

-- class IsTypeClass (ct :: Constraint) where
--     type Dict ct :: *
-- 
-- class HasClasses (cts :: [Constraint]) where
--     type Dicts cts :: [*]
-- 
-- instance HasClasses '[] where type Dicts '[] = '[]
-- 
-- instance {-# OVERLAPS #-} (IsTypeClass ct, HasClasses cts) => HasClasses (ct ': cts) where
--     type Dicts (ct ': cts) = Dict ct ': Dicts cts
-- 
-- instance HasClasses cts => HasClasses (ct ': cts) where type Dicts (ct ': cts) = Dicts cts

-- | Tries to provide a canonical witness for the given constraint. This is:
--    - () for empty constraints
--    - Refl for equality constraints
--    - the dictionary for typeclass constraints
--    - the implicit parameter's value for ImplicitParams
--    - a HList of the component constraints' canonical witnesses for conjoined constraints.
--   It's implemented by CoreToCore magic, so we leave it as `undefined` here and make sure
--   it won't be inlined so that we can still find it in the CoreToCore passes.
canonicalWitness :: forall (ct :: Constraint). ct => Witness ct
canonicalWitness = undefined -- implemented by compiler plug in
{-# NOINLINE canonicalWitness #-}

-- | Transforms a constraint into a witness-value argument, *exposing* it.
expose :: forall (ct :: Constraint) a. (ct => a) -> (Witness ct -> a)
expose thing witness = undefined

-- | Alias of expose, with the arguments flipped. This is mostly useful as an argument to higher-order functions.
useWitness :: forall (ct :: Constraint) a. Witness ct -> (ct => a) -> a
useWitness witness thing = undefined

data Isomorphism a b = Iso {appIso :: a -> b, appRevIso :: b -> a}

mkIso :: (a -> b) -> (b -> a) -> Isomorphism a b
mkIso fwd bwd = Iso {appIso = fwd, appRevIso = bwd}

revIso :: Isomorphism a b -> Isomorphism b a
revIso Iso{..} = Iso {appIso = appRevIso, appRevIso = appIso}