{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Database.PostgreSQL.Simple
import Data.Maybe
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
  newTaskId <- createTaskDb conn "New task" False
  putStrLn $ "New task ID: " ++ (show newTaskId)
  afterAddingTask <- getTasksDb conn
  print afterAddingTask
  
  putStrLn ""
  putStrLn "Updating task"
  _ <- updateTaskDb conn newTaskId "New task (updated)" True
  afterUpdatingTask <- getTasksDb conn
  print afterUpdatingTask
  
  putStrLn ""
  putStrLn "Deleting task"
  _ <- deleteTaskDb conn newTaskId
  afterDeletingTask <- getTasksDb conn
  print afterDeletingTask


getConnection :: IO Connection
getConnection = connect $
  ConnectInfo { connectHost="localhost"
              , connectPort=5432
              , connectUser="todoadmin"
              , connectPassword="a"
              , connectDatabase="todolist"
              }

createTaskDb :: Connection -> Text -> Bool -> IO Int
createTaskDb conn label completed =
  let q = "INSERT INTO tasks (label, completed) VALUES (?, ?) RETURNING id"
      params = (label, completed)
  in do
    result <- query conn q params
    return $ fromOnly $ head result
  -- Sorry! Partial function, and I'm lazy right now

getTasksDb :: Connection -> IO [(Int, Text, Bool)]
getTasksDb conn =
  let q = "SELECT id, label, completed FROM tasks;"
  in query_ conn q

getTaskByIdDb :: Connection -> Int -> IO (Maybe (Int, Text, Bool))
getTaskByIdDb conn taskId =
  let q = "SELECT id, label, completed FROM tasks WHERE id=?;"
      params = Only taskId
  in do
    result <- query conn q params
    return $ listToMaybe result

updateTaskDb :: Connection -> Int -> Text -> Bool -> IO ()
updateTaskDb conn taskId label completed =
  let q = "UPDATE tasks SET label=?, completed=? WHERE id=?;"
      params = (label, completed, taskId)
  in void $ execute conn q params

deleteTaskDb :: Connection -> Int -> IO ()
deleteTaskDb conn taskId =
  let q = "DELETE FROM tasks WHERE id=?;"
      params = Only taskId
  in void $ execute conn q params
