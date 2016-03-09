{-# OPTIONS_GHC -fplugin ConstraintWitness.Plugin #-}

{-# LANGUAGE
    TypeOperators,
    DataKinds
    #-}

module ConstraintWitness.Test where

import ConstraintWitness.Internal
import Data.Type.Equality ((:~:)(..))
import Data.HList (HList)

test1 :: Witness (Int ~ a) -> Int :~: a
test1 = id

test2 :: Witness (Int ~ Int) -> Int :~: Int
test2 = id

test3 :: Witness (()) -> ()
test3 = id

-- test4 :: Witness (((), ())) -> HList [(), ()]
-- test4 = id