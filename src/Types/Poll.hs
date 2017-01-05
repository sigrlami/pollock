{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric       #-}

module Types.Poll where

import Data.Text
import Data.Aeson
import Data.UUID
import Control.Applicative
import GHC.Generics
import Data.Time.Clock
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.FromRow
--import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Data.Default

--------------------------------------------------------------------------------

data Poll =
  Poll { pollId    :: Int
       , pollTitle :: Maybe Text
       , pollDesc  :: Maybe Text
       , pollStart :: Maybe UTCTime
       , pollEnd   :: Maybe UTCTime
       , pollOwner :: Maybe Int
       , pollChann :: Maybe Int  
       } deriving (Eq, Generic, Show)
          
instance FromJSON Poll
instance ToJSON   Poll

instance FromRow Poll
instance ToRow   Poll

instance Default Poll where
  def = Poll 0 Nothing Nothing Nothing Nothing Nothing Nothing
