module CheckedStatefulMonad where

import Prelude hiding (LT, GT, EQ, id)
import Base
import Data.Maybe
import CheckedStateful
import Control.Monad



data CheckedStateful t = CST (Memory -> (Checked t, Memory))

instance Functor CheckedStateful where
  fmap  = liftM

instance Applicative CheckedStateful where
  pure val = CST (\m -> (Good val, m))
  (<*>) = ap 

instance Monad CheckedStateful where
  return = pure
  (CST c) >>= f = 
    CST (\m -> 
      let (val, m') = c m in
        case val of 
          Error v -> (val, m')
          Good v -> let CST f' = f v in 
                                  f' m'
      )
        
toStateful :: Checked Value -> CheckedStateful Value
toStateful  cv = CST(\m -> (cv, m))

myError msg = CST(\m -> (Error msg, m))

evaluate :: Exp -> Env -> CheckedStateful Value
-- basic operations
evaluate (Literal v) env = return v
evaluate (Unary op a) env = do
  av <- evaluate a env
  toStateful (checked_unary op av)
evaluate (Binary op a b) env = do
  av <- evaluate a env
  bv <- evaluate b env
  toStateful (checked_binary op av bv)
evaluate (If a b c) env = do
  av <- evaluate a env
  case av of
    BoolV cond -> evaluate (if cond then b else c) env
    _ -> myError ("Expected boolean but found " ++ show av)

-- variables and declarations
evaluate (Declare x e body) env = do    -- non-recursive case
  ev <- evaluate e env
  let newEnv = (x, ev) : env
  evaluate body newEnv
evaluate (Variable x) env    =
  case lookup x env of
    Nothing -> myError ("Variable " ++ x ++ " undefined")
    Just v  -> return v

-- first-class functions
evaluate (Function x body) env = return (ClosureV  x body env)
evaluate (Call fun arg) env = do
  funv <- evaluate fun env
  case funv of
    ClosureV x body closeEnv -> do
      argv <- evaluate arg env
      let newEnv = (x, argv) : closeEnv
      evaluate body newEnv
    _ ->  myError ("Expected function but found " ++ show funv)

-- mutation operations
evaluate (Seq a b) env = do
  evaluate a env
  evaluate b env
evaluate (Mutable e) env = do
  ev <- evaluate e env
  newMemory ev        
evaluate (Access a) env = do
  AddressV i <- evaluate a env
  readMemory i
evaluate (Assign a e) env = do
  AddressV i <- evaluate a env
  ev <- evaluate e env
  updateMemory ev i

newMemory val = CST (\mem-> (Good (AddressV (length mem)), mem ++ [val]))

readMemory i = CST (\mem-> (Good (access i mem), mem))

updateMemory val i = CST (\mem-> (Good val, update i val mem))

runStateful (CST c) = 
   let (val, mem) = c [] in val



  
