import Prelude hiding (LT, GT, EQ, id)
import CheckedStatefulMonad
import Base
import CheckedStatefulParse

execute exp = show (runStateful (evaluate exp []))

main = testMain parseExp execute
      
  
