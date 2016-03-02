module ConstraintWitness.Test where

import ConstraintWitness.Internal
import Data.Type.Equality ((:~:)(..))

test1 :: Witness (Int ~ a) -> Int :~: a
test1 = id