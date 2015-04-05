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

--------------------------------------------------------------------------------

data Poll =
  Poll { pollId    :: Int
       , pollTitle :: Text
       , pollDesc  :: Text
       , pollStart :: UTCTime
       , pollEnd   :: UTCTime
       , pollOwner :: Int
       } deriving (Eq, Generic, Show)
          
-- For pulling from the database
-- instance FromRow Poll where
--     fromRow = Poll <$> field <*> field <*> field <*> field <*> field <*> field

instance FromJSON Poll
instance ToJSON   Poll

instance FromRow Poll
instance ToRow   Poll
