module Hash(runScript, runInteractive) where

import System.FilePath.Posix
import System.Environment
import System.IO
import qualified Data.Map.Strict as M

import Parsing.Parser
import Language.Interpreter
import Language.Types
import Language.Commands

-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.
-- Runs a .hash script
runScript :: FilePath -> IO ()
runScript fp =
  readFile fp >>= interpret (M.empty, fp) >> return ()

-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
runInteractive = getExecutablePath >>= \path
  -> mainLoop (M.empty, takeDirectory path)

mainLoop :: InterpreterState -> IO()
mainLoop state = do
  putStr (snd state) >> putStr " > " >> hFlush stdout
  line <- getLine
  case line of
    ""        -> mainLoop state
    "*quit"   -> return ()
    _         -> do
      interpreted <- interpret state line
      case interpreted of
        Left err -> print err >> mainLoop state
        Right newState@(varTable, cwd) -> print varTable >> mainLoop newState
