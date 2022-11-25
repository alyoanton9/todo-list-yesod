{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( convertLogMsg
  , defaultLogEnv

  -- * re-exports from @Katip@
  , logMsg
  , runKatipT
  , logStr
  , katipLogger
  , KatipT(..)
  , Katip(..)
  , LogEnv
  , Severity(..)
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger
import Data.List (singleton)
import Data.Maybe (fromMaybe)
import Katip
import Network.Wai (Middleware, rawPathInfo, requestHeaderUserAgent)
import System.IO (stdout)

defaultLogEnv :: IO LogEnv
defaultLogEnv = do
  let permitFunc = permitItem DebugS
  handleScribe <- mkHandleScribe ColorIfTerminal stdout permitFunc V2
  logEnv <- initLogEnv "todo-list-yesod" "production"
  registerScribe "stdout" handleScribe defaultScribeSettings logEnv

-- | Custom @Katip@ logger to add as a middleware.
katipLogger :: LogEnv -> Middleware
katipLogger env app request responseFunc = runKatipT env $ do
  let userAgent = fromMaybe "undefined" $ requestHeaderUserAgent request
  let requestedPath = rawPathInfo request
  let msg = "raw path=" <> requestedPath <> " user agent=" <> userAgent
  logMsg "middleware" InfoS $ logStr msg
  liftIO $ app request responseFunc

-- | Convert @Katip.logMsg@ to @Control.Monad.Logger.monadLoggerLog@.
convertLogMsg
  :: ToLogStr msg
  => (Namespace -> Severity -> Katip.LogStr -> m ())
  -> (Loc -> LogSource -> LogLevel -> msg -> m ())
convertLogMsg logMsg' _ logSrc logLvl msg =
  logMsg' namespace severity logStrMsg
  where
    namespace = Namespace $ singleton logSrc
    severity = logLvlToSeverity logLvl
    logStrMsg = logStr . fromLogStr . toLogStr $ msg

logLvlToSeverity :: LogLevel -> Severity
logLvlToSeverity = \case
  LevelDebug     -> DebugS
  LevelInfo      -> InfoS
  LevelWarn      -> WarningS
  LevelError     -> ErrorS
  (LevelOther _) -> NoticeS

-- Instances

-- Note: these instances are needed to make Katip and Postgres friends

-- | @MonadLogger@ instance required to define @MonadLoggerIO@ instance.
instance MonadIO m => MonadLogger (KatipT m) where
  monadLoggerLog = convertLogMsg logMsg

-- | @IO@ instance required to define @MonadLoggerIO@ instance.
instance Katip IO where
  getLogEnv = defaultLogEnv
  localLogEnv _ ioAction = ioAction

-- | @MonadLoggerIO@ instance required by @Database.Persist.Postgresql.createPostgresqlPool@.
instance MonadIO m => MonadLoggerIO (KatipT m) where
  askLoggerIO = KatipT . return $ convertLogMsg logMsg
