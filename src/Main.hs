{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database.PostgreSQL.Simple
  ( Connection
  , ConnectInfo(..)
  , connect
  )

import IO 
  ( createTaskIO
  , deleteTaskByIdIO
  , getTasksIO
  , updateTaskIO
  )
import Models
  ( Task(..)
  , TaskCompleted(..)
  , TaskFields(..)
  )

main :: IO ()
main = do
  conn <- getConnection

  putStrLn ""
  putStrLn "Starting"
  startingTasks <- getTasksIO conn
  print startingTasks
  
  putStrLn ""
  putStrLn "Adding task"
  let newTask = TaskFields "New task" (TaskCompleted False)
  newTaskId <- createTaskIO conn newTask
  putStrLn $ "New task ID: " ++ (show newTaskId)
  afterAddingTask <- getTasksIO conn
  print afterAddingTask
  
  putStrLn ""
  putStrLn "Updating task"
  let updatedTask = Task newTaskId (TaskFields "New task (updated)" (TaskCompleted True))
  _ <- updateTaskIO conn updatedTask
  afterUpdatingTask <- getTasksIO conn
  print afterUpdatingTask
  
  putStrLn ""
  putStrLn "Deleting task"
  _ <- deleteTaskByIdIO conn newTaskId
  afterDeletingTask <- getTasksIO conn
  print afterDeletingTask


getConnection :: IO Connection
getConnection = connect $
  ConnectInfo { connectHost="localhost"
              , connectPort=5432
              , connectUser="todoadmin"
              , connectPassword="a"
              , connectDatabase="todolist"
              }
