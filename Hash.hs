-- The top-level module. Connects parsing to execution and adds interaction
-- with the user / reading from file.
-- Runs a .hash script
runScript :: FilePath -> IO ()

-- Communicates with the user and performs hash commands line by line
runInteractive :: IO ()
