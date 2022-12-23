{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql (
  runMigration, runSqlPool, withPostgresqlPool)
import Network.Wai.Handler.Warp (run)
import Yesod.Core.Dispatch (toWaiAppPlain)

import Config
import Controllers
import Entity
import Logger

main :: IO ()
main = runStderrLoggingT $
  withPostgresqlPool connectionStr connectionsNumber $ \pool ->
    liftIO $ do
      let config = Config
            { configPool = pool
            , configPort = defaultPort
            }

      runSqlPool (runMigration migrateAll) (configPool config)

      app <- toWaiAppPlain $ TaskList config

      -- apply logger middleware
      run (configPort config) . logger $ app
