

execute :: InterpreterState -> String -> [String] -> IO (Value, CWD)
execute (vt, cwd) cmdName cmdArgs = case map toUpper cmdName of
  "CD"  -> cmdCD cmdArgs cwd
  _     -> return (VDouble 1.0, cwd)

cmdCD cmdArgs cwd = case head cmdArgs of
  ".." -> case elemIndices '/' cwd of
            []  -> return (VDouble 0, cwd)
            arr -> return (VDouble 1, take (last arr) cwd)
  _    -> return (VDouble 1, cwd)