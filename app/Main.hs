{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Logger (runStderrLoggingT)
import Database.Persist.Postgresql
import Yesod

import Controllers
import Entity

connectionStr :: ConnectionString
connectionStr = "host=localhost dbname=postgres user=postgres password=postgres port=5432"

connectionsNumber :: Int
connectionsNumber = 10

port :: Int
port = 3000

main :: IO ()
main = runStderrLoggingT $
  withPostgresqlPool connectionStr connectionsNumber $ \pool -> liftIO $ do
    flip runSqlPersistMPool pool $ do
      runMigration migrateAll
    warp port $ TaskList pool
