module Interpreting.Commands where

import Data.Char
import Data.List
import Control.Monad
import Control.Applicative
import Base.Types
import System.FilePath.Posix
import System.Directory
import Data.Maybe
import qualified Data.Map.Strict as M

type Args = [String]

execute :: InterpreterState -> String -> Args -> IO (Value, CWD)
execute (vt, cwd) cmdName cmdArgs = case map toUpper cmdName of
  "CD"  -> cmdCD cmdArgs cwd
  "LS"  -> cmdLS cmdArgs cwd
  "MV"  -> cmdMV cmdArgs cwd
  "CP"  -> cmdCP cmdArgs cwd
  "RM"  -> cmdRM cmdArgs cwd
  "CREATE" -> cmdCREATAE cmdArgs cwd
  "TOTEST" -> cmdCD ["/home/dito/test/"] cwd
  "ECHO" -> cmdECHO cmdArgs vt cwd
  _     -> return (VDouble 1.0, cwd)

cmdCD :: Args -> CWD -> IO (Value, CWD)
cmdCD [] cwd = return (VBool True, "/")
cmdCD [path] cwd = return (VBool True, toAbsolute path cwd)
cmdCD _ cwd = return (VBool False, cwd)

cmdLS :: Args -> CWD -> IO (Value, CWD)
cmdLS [] cwd = getDirectoryContents cwd >>= putStrLn . unlines . filter ((/= '.') . head) >> return (VBool True, cwd)
cmdLS [path] cwd = getDirectoryContents (toAbsolute path cwd) >>= putStrLn . unlines . filter ((/= '.') . head) >> return (VBool True, cwd)
cmdLS _ cwd = return (VBool False, cwd)

toAbsolute :: FilePath -> CWD -> FilePath
toAbsolute fp cwd = if isAbsolute fp then fp else combine cwd fp

cmdMV :: Args -> CWD -> IO (Value, CWD)
cmdMV [] cwd = return (VBool False, cwd)
cmdMV paths cwd = forM_ (init paths) (flip renameFile target . flip toAbsolute cwd)
  >> return (VBool True, cwd)
  where target = toAbsolute (last paths) cwd

cmdCP :: Args -> CWD -> IO (Value, CWD)
cmdCP [] cwd = return (VBool False, cwd)
cmdCP [path] cwd = cmdCP [] cwd
cmdCP [source, target] cwd = print (toAbsolute source cwd) >> print (toAbsolute target cwd) >>
  copyFile (toAbsolute source cwd) (toAbsolute target cwd)
  >> return (VBool True, cwd)
cmdCP list cwd =
  forM_ (init list) (flip cmdCP (toAbsolute (last list) cwd) . return)
  >> return (VBool True, cwd)

cmdRM :: Args -> CWD -> IO (Value, CWD)
cmdRM [] cwd = return (VBool False, cwd)
cmdRM list cwd = forM_ list (removeFile . flip toAbsolute cwd)
  >> return (VBool True, cwd)

------
cmdCREATAE :: Args -> CWD -> IO (Value, CWD)
cmdCREATAE [] cwd = return (VBool False, cwd)
cmdCREATAE list cwd = forM_ list (createDirectory . flip toAbsolute cwd)
  >> return (VBool True, cwd)

cmdECHO :: Args -> VarTable -> CWD -> IO (Value, CWD)
cmdECHO [str] vt cwd = print (unwords v2) >> return (VDouble 1, cwd)
  where
  str2 = if length str > 3
         then init . tail $ str
         else str
  v2 = map envar $ words str2
  envar (w:wrd) = if w == '$'
                  then extract (fromJust (M.lookup wrd vt))
                  else w:wrd
  envar wrd = wrd
  extract (VDouble a) = show a
  extract (VBool a) = show a
  extract (VString a) = a
