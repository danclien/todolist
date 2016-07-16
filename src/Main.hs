{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Database.PostgreSQL.Simple
  ( Connection
  , ConnectInfo(..)
  , Only(..)
  , connect
  , execute
  , fromOnly
  , query
  , query_
  )
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Data.Maybe (listToMaybe)
import Data.String (IsString)
import Data.Text (Text)

main :: IO ()
main = do
  conn <- getConnection

  putStrLn ""
  putStrLn "Starting"
  startingTasks <- getTasksDb conn
  print startingTasks
  
  putStrLn ""
  putStrLn "Adding task"
  newTaskId <- createTaskDb conn "New task" (TaskCompleted False)
  putStrLn $ "New task ID: " ++ (show newTaskId)
  afterAddingTask <- getTasksDb conn
  print afterAddingTask
  
  putStrLn ""
  putStrLn "Updating task"
  _ <- updateTaskDb conn newTaskId "New task (updated)" (TaskCompleted True)
  afterUpdatingTask <- getTasksDb conn
  print afterUpdatingTask
  
  putStrLn ""
  putStrLn "Deleting task"
  _ <- deleteTaskDb conn newTaskId
  afterDeletingTask <- getTasksDb conn
  print afterDeletingTask

newtype TaskId = TaskId { unTaskId :: Int } deriving (Eq, Show, FromField, ToField)
newtype TaskLabel = TaskLabel { unTaskLabel :: Text } deriving (Eq, Show, IsString, FromField, ToField)
newtype TaskCompleted = TaskCompleted { unTaskCompleted :: Bool } deriving (Eq, Show, FromField, ToField)

getConnection :: IO Connection
getConnection = connect $
  ConnectInfo { connectHost="localhost"
              , connectPort=5432
              , connectUser="todoadmin"
              , connectPassword="a"
              , connectDatabase="todolist"
              }

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
