{-# LANGUAGE OverloadedStrings #-}

module Logger
  ( logger
  ) where

import Control.Monad.Logger (logInfoN, runStdoutLoggingT)
import Data.Text (intercalate)
import Network.Wai (Middleware, pathInfo)

-- | Custom logger function. Extracts URL path from request,
-- and logs it with @Info@ verbosity level.
logger :: Middleware
logger app request responseFunc = do
  let requestedPath = intercalate "/" $
        pathInfo request
  let msg = "url-path=" <> requestedPath
  runStdoutLoggingT $ logInfoN msg
  app request responseFunc
