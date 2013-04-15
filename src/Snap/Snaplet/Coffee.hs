{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Coffee( CoffeeScript(..)
                          , initCoffee
                          , coffeeServe
                          ) where

import qualified Data.Configurator as C (lookup)
import qualified Data.ByteString.Char8 as BS (unpack)
import           Snap.Snaplet
import           Snap.Core (rqURI, modifyResponse, getRequest, setContentType)
import           Snap.Util.FileServe (serveDirectory)
import           Snap.Snaplet.Coffee.Utils
import           Control.Monad.Reader (liftIO, when, MonadIO)
import           Control.Monad.State.Class (get)
import           Control.Applicative ((<$>))
import           System.Exit (ExitCode(..))
import           Coffee.Bindings (Coffee(..), coffeeCompile)
import           Control.Monad (liftM)
import           Paths_snaplet_coffee

-- | Snaplet-Coffee initializer
initCoffee :: SnapletInit c CoffeeScript
initCoffee = makeSnaplet "coffee" "description" dataDir $ do
    config <- getSnapletUserConfig
    fp     <- getSnapletFilePath
    [comp, dev, dDir] <- liftIO $ mapM (C.lookup config) configOptions

    let coffee = CoffeeScript fp comp (getCompilerMode dev) (getDestDir dDir coffee)
    liftIO $ mapM_ createDirUnlessExists [fp, srcDir coffee, destDir coffee]

    allCoffees <- liftIO $ allCoffeeFiles coffee

    when (Production == compileMode coffee) $ liftIO $ compileFiles coffee allCoffees
    return coffee
  where dataDir = Just $ liftM (++ "/resources") getDataDir
        configOptions = ["compilerPath", "compilerMode", "destinationPath"]

-- | Serves the compiled CoffeeScript files
coffeeServe :: Handler b CoffeeScript ()
coffeeServe = do
    modifyResponse . setContentType $ "text/javascript;charset=utf-8"
    cfg <- get
    requestedFile <- (srcDir cfg ++) . requestedCoffeeFile .
                     BS.unpack . rqURI <$> getRequest
    when (Development == compileMode cfg) $ liftIO $ compileFiles cfg [requestedFile]
    serveDirectory $ destinationDir cfg

-- | Compiles the files that are being served, but ignores all other files.
compileFiles :: MonadIO m => CoffeeScript -> [FilePath] -> m ()
compileFiles cfg fp = do
    let coffeeStruct = Coffee (compiler cfg) False
    result    <- liftIO $ coffeeCompile fp
                 (Just (destinationDir cfg)) coffeeStruct
    case result of
        ExitSuccess   -> return ()
        ExitFailure x -> error $ show x ++ errMsg
  where errMsg = unlines
                 [ " - Error while compiling CoffeeScript"
                 , "Does this file really exist?"
                 , "Is your /snaplets/coffee/devel.cfg correct?"
                 , "You might need to restart the server after fixing the issue."
                 ]
