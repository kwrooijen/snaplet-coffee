module Snap.Snaplet.Coffee.Utils
       ( CompileMode(..)
       , CoffeeScript(..)
       , allCoffeeFiles
       , destDir
       , srcDir
       , createDirUnlessExists
       , getCompilerMode
       , getDestDir
       , requestedCoffeeFile
       ) where

import Control.Monad
import System.Directory
import System.FilePath

-- | Simple data structure for CompileMode
data CompileMode = Development | Production deriving (Show, Eq)

-- | The CoffeeScript Snaplet Configuration
data CoffeeScript = CoffeeScript
    { snapletFilePath :: FilePath
    , compiler        :: Maybe String
    , compileMode     :: CompileMode
    , destinationDir  :: FilePath
    } deriving (Show)

-- | Gets all the .coffee files in the source directory
allCoffeeFiles :: CoffeeScript -> IO [FilePath]
allCoffeeFiles cfg = do
    let p = srcDir cfg
    a <- getDirectoryContents p
    return $ map (</> srcDir cfg) $ filterCoffee a

-- | Simple reverse taker
takeReverse :: Int -> [a] -> [a]
takeReverse i = reverse . take i . reverse

-- | Filters the .coffee files
filterCoffee :: [String] -> [String]
filterCoffee = filter ((==) ".coffee" . takeReverse 7)

-- | Source directory
srcDir :: CoffeeScript -> FilePath
srcDir = (</> "coffee") . snapletFilePath

-- | Destination directory
destDir :: CoffeeScript -> FilePath
destDir = (</> "js") . snapletFilePath

-- | Function to create directories if they don't exist
createDirUnlessExists :: FilePath -> IO ()
createDirUnlessExists fp = do
    dirExists <- doesDirectoryExist fp
    unless dirExists $ createDirectory fp

-- | Gets the CompileMode
getCompilerMode :: Maybe String -> CompileMode
getCompilerMode Nothing              = Production
getCompilerMode (Just "Development") = Development
getCompilerMode (Just "Production")  = Production
getCompilerMode (Just x)             = error $ x ++ " is not a valid Compiler mode for snaplet-coffeescript. -- devel.cfg"

-- | Gets the destination directory
getDestDir :: Maybe String -> CoffeeScript -> FilePath
getDestDir Nothing   c = destDir c
getDestDir (Just "") c = destDir c
getDestDir (Just x)  _ = x

-- | Converts path/to/file.js to file.coffee
requestedCoffeeFile :: String -> FilePath
requestedCoffeeFile =
     ("/"++) . (++"coffee") . reverse .dropWhile con2 . takeWhile con1 . reverse
  where con1 = (/= '/')
        con2 = (/= '.')