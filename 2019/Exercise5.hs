module Exercise5 where

import Util
import IntCode
import Control.Monad.State

--- Step: Just if next state, Nothing is finished. 
runProgram :: Memo -> Int
runProgram = head . outps . last . unfoldMb execStep . initVM where 
  initVM memo = IntVM 0 memo [5] []

test1 :: Memo
test1 = undefined

input1 :: Memo
input1 = undefined
