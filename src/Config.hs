{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Config
  ( Config(..)
  , makePool
  , katipLogger
  , defaultPort
  ) where

import Database.Persist.Postgresql (
  ConnectionPool, ConnectionString, createPostgresqlPool)
import Network.Wai.Handler.Warp (Port)

import Logger

data Config
  = Config
  { configPool   :: ConnectionPool
  , configLogEnv :: LogEnv
  , configPort   :: Port
  }

makePool :: LogEnv -> IO ConnectionPool
makePool logEnv = runKatipT logEnv $
  createPostgresqlPool connectionStr connectionsNumber

-- Config constants

connectionStr :: ConnectionString
connectionStr = "host=localhost dbname=postgres user=postgres password=postgres port=5432"

connectionsNumber :: Int
connectionsNumber = 10

defaultPort :: Int
defaultPort = 3000
