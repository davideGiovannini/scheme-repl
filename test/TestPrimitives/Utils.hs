module TestPrimitives.Utils
where

import Data(ThrowsError, LispVal)
import Control.Monad.Except(runExcept)

(===>) :: ThrowsError LispVal -> LispVal -> Bool
action ===> value = runExcept action == Right value
