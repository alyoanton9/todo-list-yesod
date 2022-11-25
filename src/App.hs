{-# LANGUAGE OverloadedStrings #-}

module App
  ( runTaskList
  ) where

import Database.Persist.Postgresql (runMigration, runSqlPool)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Yesod.Core.Dispatch (toWaiAppPlain)

import Config
import Controllers
import Entity
import Logger

runTaskList :: IO ()
runTaskList = do
  withConfig $ \config -> do
    app <- initialize config
    run (configPort config) app

initialize :: Config -> IO Application
initialize config = do
  let logger = katipLogger $ configLogEnv config
  runSqlPool (runMigration migrateAll) (configPool config)
  -- Make plain WAI application
  app <- toWaiAppPlain $ TaskList config
  -- Add logging middleware
  return . logger $ app

withConfig :: (Config -> IO a) -> IO a
withConfig action = do
  logEnv <- defaultLogEnv
  pool <- makePool logEnv
  let config = Config
        { configPool   = pool
        , configLogEnv = logEnv
        , configPort   = defaultPort
        }
  action config
