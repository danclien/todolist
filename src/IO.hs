{-# LANGUAGE OverloadedStrings #-}

module IO 
  ( deleteTaskDb
  , createTaskDb
  , getTasksDb
  , getTaskByIdDb
  , updateTaskDb
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

createTaskDb :: Connection -> TaskLabel -> TaskCompleted -> IO TaskId
createTaskDb conn label completed =
  let q = "INSERT INTO tasks (label, completed) VALUES (?, ?) RETURNING id"
      params = (label, completed)
  in do
    result <- query conn q params
    return $ fromOnly . head $ result
  -- Sorry! Partial function, and I'm lazy right now

getTasksDb :: Connection -> IO [(TaskId, TaskLabel, TaskCompleted)]
getTasksDb conn =
  let q = "SELECT id, label, completed FROM tasks;"
  in query_ conn q

getTaskByIdDb :: Connection -> TaskId -> IO (Maybe (TaskId, TaskLabel, TaskCompleted))
getTaskByIdDb conn taskId =
  let q = "SELECT id, label, completed FROM tasks WHERE id=?;"
      params = Only taskId
  in do
    result <- query conn q params
    return $ listToMaybe result

updateTaskDb :: Connection -> TaskId -> TaskLabel -> TaskCompleted -> IO ()
updateTaskDb conn taskId label completed =
  let q = "UPDATE tasks SET label=?, completed=? WHERE id=?;"
      params = (label, completed, taskId)
  in void $ execute conn q params

deleteTaskDb :: Connection -> TaskId -> IO ()
deleteTaskDb conn taskId =
  let q = "DELETE FROM tasks WHERE id=?;"
      params = Only taskId
  in void $ execute conn q params
