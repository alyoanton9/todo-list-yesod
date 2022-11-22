{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

-- TODO: rename to Model? Database?

module Entity
  ( Task (..)
  , TaskId
  , Entity (..)
  , migrateAll
  ) where

import Data.Aeson
import Database.Persist
import Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Task
    content String
    deriving Show
|]

instance ToJSON Task where
  toJSON task = object ["content" .= taskContent task]

instance FromJSON Task where
  parseJSON = withObject "Task" $ \v -> Task
    <$> v .: "content"

instance ToJSON (Entity Task) where
  toJSON (Entity taskId task) = object
    [ "id"   .= taskId
    , "content" .= taskContent task
    ]
