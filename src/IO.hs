{-# LANGUAGE OverloadedStrings #-}

module IO 
  ( createTaskIO
  , deleteTaskIO
  , deleteTaskByIdIO
  , getTasksIO
  , getTaskByIdIO
  , updateTaskIO
  ) where

import Control.Monad (void)
import Database.PostgreSQL.Simple
  ( Connection
  , Only(..)
  , execute
  , fromOnly
  , query
  , query_
  )
import Data.Maybe (listToMaybe)

import Models

createTaskIO :: Connection -> TaskFields -> IO TaskId
createTaskIO conn (TaskFields label completed) =
  createTaskDb conn label completed

createTaskDb :: Connection -> TaskLabel -> TaskCompleted -> IO TaskId
createTaskDb conn label completed =
  let q = "INSERT INTO tasks (label, completed) VALUES (?, ?) RETURNING id"
      params = (label, completed)
  in do
    result <- query conn q params
    return $ fromOnly . head $ result
  -- Sorry! Partial function, and I'm lazy right now

-- *****

getTasksIO :: Connection -> IO [Task]
getTasksIO conn =
  do
    rows <- getTasksDb conn
    return $ fmap rowToTask rows

getTasksDb :: Connection -> IO [(TaskId, TaskLabel, TaskCompleted)]
getTasksDb conn =
  let q = "SELECT id, label, completed FROM tasks;"
  in query_ conn q

-- *****

getTaskByIdIO :: Connection -> TaskId -> IO (Maybe Task)
getTaskByIdIO conn taskId =
  do
    maybeRow <- getTaskByIdDb conn taskId
    return $ fmap rowToTask maybeRow

getTaskByIdDb :: Connection -> TaskId -> IO (Maybe (TaskId, TaskLabel, TaskCompleted))
getTaskByIdDb conn taskId =
  let q = "SELECT id, label, completed FROM tasks WHERE id=?;"
      params = Only taskId
  in do
    result <- query conn q params
    return $ listToMaybe result

rowToTask :: (TaskId, TaskLabel, TaskCompleted) -> Task
rowToTask (taskId, taskLabel, taskCompleted) = 
  Task taskId (TaskFields taskLabel taskCompleted)

-- *****

updateTaskIO :: Connection -> Task -> IO ()
updateTaskIO conn (Task taskId (TaskFields taskLabel taskCompleted)) = 
  updateTaskDb conn taskId taskLabel taskCompleted

updateTaskDb :: Connection -> TaskId -> TaskLabel -> TaskCompleted -> IO ()
updateTaskDb conn taskId label completed =
  let q = "UPDATE tasks SET label=?, completed=? WHERE id=?;"
      params = (label, completed, taskId)
  in void $ execute conn q params

-- *****

deleteTaskIO :: Connection -> Task -> IO ()
deleteTaskIO conn (Task taskId _) =
  deleteTaskDb conn taskId

deleteTaskByIdIO :: Connection -> TaskId -> IO ()
deleteTaskByIdIO = deleteTaskDb 

deleteTaskDb :: Connection -> TaskId -> IO ()
deleteTaskDb conn taskId =
  let q = "DELETE FROM tasks WHERE id=?;"
      params = Only taskId
  in void $ execute conn q params
