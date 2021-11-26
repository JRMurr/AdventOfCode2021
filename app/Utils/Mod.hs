module Utils.Mod where

import System.FilePath (combine, takeDirectory)

readInput :: FilePath -> IO [String]
readInput filename = lines <$> readFile filename

-- | Given a source file path, get the `in` file in that directory
getInputFile :: FilePath -> FilePath
getInputFile sourceFilePath = combine (takeDirectory sourceFilePath) "in"

-- | Given a source file path, get the `in.example` file in that directory
getExampleInputFile :: FilePath -> FilePath
getExampleInputFile sourceFilePath = combine (takeDirectory sourceFilePath) "in.example"