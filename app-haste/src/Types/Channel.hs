{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric       #-}

module Types.Channel where

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

data Channel =
  Channel { chId    :: Int
          , chName  :: Text  
          , chOwner :: Text
          } deriving (Eq, Generic, Show)

instance FromJSON Channel
instance ToJSON   Channel

instance FromRow Channel
instance ToRow   Channel
