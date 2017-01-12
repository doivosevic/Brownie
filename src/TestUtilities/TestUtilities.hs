module TestUtilities.TestUtilities where

import Base.Types
import Interpreting.Interpreter
import qualified Data.Map.Strict as M
import Text.Parsec hiding (parseTest)
import Parsing.Parser

type TestSourceText = String

executionTest :: TestSourceText -> IO (Either ParseError InterpreterState)
executionTest source = do
  let state = (M.empty, "")
  either (return . Left) ((Right <$>) . run state) . parse (many1 statement) "error" $ source

bindTest :: TestSourceText -> Either ParseError [Statement]
bindTest = parse (many1 statement) "error"

evaluateTest :: (Eq a, Show a) => Either ParseError a -> a -> String
evaluateTest (Left parseError) _ = show parseError
evaluateTest (Right evaluationResult) expectedResult = if evaluationResult == expectedResult
                                                  then "test passed"
                                                  else "test failed\nexpected:\n"
                                                    ++ show expectedResult
                                                    ++ "\nreceived:\n"
                                                    ++ show evaluationResult

runExecutionTest :: TestSourceText -> InterpreterState -> IO ()
runExecutionTest source expectedState = do
  result <- executionTest source
  putStrLn $ evaluateTest result expectedState

runBindingTest :: TestSourceText -> [Statement]-> IO ()
runBindingTest source expectedStatements = do
  let result = bindTest source
  putStrLn $ evaluateTest result expectedStatements

--Either ParseError InterpreterState
