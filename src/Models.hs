{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Models 
  ( Task(..)
  , TaskCompleted(..)
  , TaskFields(..)
  , TaskId(..)
  , TaskLabel(..)
  ) where

import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import Data.String (IsString)
import Data.Text (Text)

newtype TaskId = TaskId { unTaskId :: Int } deriving (Eq, Show, FromField, ToField)
newtype TaskLabel = TaskLabel { unTaskLabel :: Text } deriving (Eq, Show, IsString, FromField, ToField)
newtype TaskCompleted = TaskCompleted { unTaskCompleted :: Bool } deriving (Eq, Show, FromField, ToField)

data Task = Task 
  { taskTaskId :: TaskId
  , taskTaskFields :: TaskFields
  } deriving (Eq, Show)

data TaskFields = TaskFields
  { taskTaskLabel :: TaskLabel
  , taskTaskCompleted :: TaskCompleted
  } deriving (Eq, Show)