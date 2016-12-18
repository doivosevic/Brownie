import TestUtilities.TestUtilities
import Language.Interpreter
import Language.Types
import Data.Map.Strict

main :: IO ()
main = do
  test1
  test2
  test3
  test4

test1 :: IO ()
test1 = runExecutionTest "$a=1;" (fromList [("a",VDouble 1.0)], "")

test2 :: IO ()
test2 = runExecutionTest "$a=1;" (fromList [], "")

test3 :: IO ()
test3 = runBindingTest "$a=1;" [Assignment "a" (Val (VDouble 1.0))]

test4 :: IO ()
test4 = runBindingTest "$a=1;" []
