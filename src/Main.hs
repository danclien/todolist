{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
  ( Connection
  , ConnectInfo(..)
  , connect
  )

import IO 
  ( deleteTaskDb
  , createTaskDb
  , getTasksDb
  , updateTaskDb
  )
import Models
  ( TaskCompleted(..)
  )

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


getConnection :: IO Connection
getConnection = connect $
  ConnectInfo { connectHost="localhost"
              , connectPort=5432
              , connectUser="todoadmin"
              , connectPassword="a"
              , connectDatabase="todolist"
              }
