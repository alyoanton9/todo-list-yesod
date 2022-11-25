{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE OverloadedStrings    #-}

module Controllers
  ( TaskList (..)
  ) where

import Database.Persist.Postgresql
import Network.HTTP.Types.Status
import Yesod

import Config
import Entity

-- Routes

data TaskList = TaskList Config

mkYesod "TaskList" [parseRoutes|
/api/task         TaskListR GET POST
/api/task/#TaskId TaskR GET DELETE PUT
|]

getTaskListR :: Handler Value
getTaskListR = do
  tasks <- getEntityTasks
  sendStatusJSON ok200 $ toJSONList tasks

postTaskListR :: Handler Value
postTaskListR = do
  task :: Task <- requireCheckJsonBody
  entityTask <- createEntityTask task
  sendStatusJSON created201 $ toJSON entityTask

getTaskR :: TaskId -> Handler Value
getTaskR taskId = do
  entityTask <- getEntityTaskById taskId
  sendStatusJSON ok200 $ toJSON entityTask

deleteTaskR :: TaskId -> Handler Value
deleteTaskR taskId = do
  entityTask <- getEntityTaskById taskId
  deleteEntityTask taskId
  sendStatusJSON ok200 $ toJSON entityTask

putTaskR :: TaskId -> Handler Value
putTaskR taskId = do
  _ <- getEntityTaskById taskId
  task :: Task <- requireCheckJsonBody
  entityTask <- replaceEntityTask taskId task
  sendStatusJSON ok200 $ toJSON entityTask

-- Instances

instance Yesod TaskList

instance YesodPersist TaskList where
  type YesodPersistBackend TaskList = SqlBackend

  runDB action = do
    TaskList config <- getYesod
    runSqlPool action (configPool config)

-- DB helpers

getEntityTasks :: Handler [Entity Task]
getEntityTasks = runDB $ selectList [] []

getEntityTaskById :: TaskId -> Handler (Entity Task)
getEntityTaskById taskId = do
  task <- runDB $ get404 taskId
  return $ Entity taskId task

createEntityTask :: Task -> Handler (Entity Task)
createEntityTask task = do
  taskId <- runDB $ insert task
  return $ Entity taskId task

deleteEntityTask :: TaskId -> Handler ()
deleteEntityTask = runDB . delete

replaceEntityTask :: TaskId -> Task -> Handler (Entity Task)
replaceEntityTask taskId task = do
  runDB $ replace taskId task
  return $ Entity taskId task
