{-# LANGUAGE
    GADTs,
    KindSignatures,
    ConstraintKinds,
    ExplicitForAll
    #-}

module ConstraintWitness.Crude where

import GHC.Prim (Constraint)
import Data.Typeable

data Witness (ct :: Constraint) where
    Fulfilled :: ct => Witness ct

instance Typeable ct => Show (Witness ct) where
    show witness = "<Fulfilled witnessing " ++ show (typeRep witness) ++ ">"

conj :: forall (a :: Constraint) (b :: Constraint). Witness a -> Witness b -> Witness (a, b)
conj Fulfilled Fulfilled = Fulfilled

unconj :: forall (a :: Constraint) (b :: Constraint). Witness (a, b) -> (Witness a, Witness b)
unconj Fulfilled = (Fulfilled, Fulfilled)

show' :: forall a. Witness (Show a) -> a -> String
show' Fulfilled it = show it