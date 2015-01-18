module Hash(runScript, runInteractive) where

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
				-> mainLoop (M.empty, path)

mainLoop :: InterpreterState -> IO()
mainLoop state = do
	putStr (snd state) >> putStr " > " >> hFlush stdout
	line <- getLine
	newState@(varTable, cwd) <- interpret state line
	print $ varTable
	mainLoop newState