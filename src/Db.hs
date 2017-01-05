{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DeriveAnyClass      #-}

module Db ( User(..)
          , Poll(..)
          , initialize  
          , savePoll
          , getPoll
          , getPolls  
          --, getPollsForUser
          , getPollsForRange
          , deletePoll
          ) where

import           Control.Applicative
import qualified Control.Exception as E
import           Control.Monad
import           Prelude hiding (id)
import           Data.Aeson
import qualified Data.ByteString.Char8 as BSC
import           Data.List
import qualified Data.Text as T
import           Data.Time (UTCTime)
import           Data.Typeable
import           Data.String (fromString)
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.Internal
import           Database.PostgreSQL.Simple.Ok
import           Database.PostgreSQL.Simple.ToField
import           GHC.Generics
import           Safe

import           Application
import           Types.Poll
import           Types.Channel

-----------------------------------------------------------------------------

data User = User Int T.Text deriving (Show)

-- Used to sort polls by time when getting a range of polls.
pollTimeOrder :: Poll -> Poll -> Ordering
pollTimeOrder e1 e2 = case pollStart e1 `compare` pollStart e2 of
    EQ -> pollEnd e1 `compare` pollEnd e2
    LT -> LT
    GT -> GT              

-----------------------------------------------------------------------------
data ConnectConfig = ConnectConfig
  { host     :: String
  , port     :: String
  , dbs      :: String
  , user     :: String
  , pass     :: String
  } deriving (Generic, Eq, Read, Show, Typeable, FromJSON, ToJSON)

-- | This is default parameters that
--   are in use if config wasn't supplied
defaultConnectConfig = ConnectConfig {
    host     = "db"
  , port     = "5432"
  , dbs      = "pollock"
  , user     = "pollock"
  , pass     = "pollock-starfish"
 }

mkConnInfo :: ConnectConfig -- ^ Internal configuration representation
           -> ConnectInfo   -- ^ Representation used by Postgresql.Simple
mkConnInfo config =
  defaultConnectInfo
  { connectHost     = host config 
  , connectPort     = fromInteger $ read $ port config
  , connectDatabase = dbs  config
  , connectUser     = user config
  , connectPassword = pass config
  }

--------------------------------------------------------------------------------

-- | Db data initialization function
-- 
initialize :: Connection -> IO ()
initialize conn = do
  queryS <- readFile "db/init.sql"
  let query' = fromString $ queryS  
  execute_ conn query'
  return ()

getPoll :: Connection -> Integer -> IO (Maybe Poll)
getPoll conn id = do
  let !query'  = "SELECT * FROM poll WHERE id=?"      
      !query'' = fromString $ query' :: Query
  (xss::[Poll]) <- query conn query'' (Only (id::Integer))
  return $ headMay xss

savePoll :: Connection -> Poll -> IO Integer
savePoll conn poll = do
  let query'' = fromString $ "INSERT INTO poll(title, desc, start, end, owner, channel_id ) VALUES (?, ?, ?, ?, ?, ?) returning id"
  (xs::[Only Integer]) <- query conn query'' poll
  case headMay xs of
    Nothing -> return $ 1
    Just x  -> return $ fromOnly x 

getPolls :: Connection -> IO [Poll]
getPolls conn = do
  let !query'  =  Data.List.unlines
                 [ "SELECT * FROM poll"
                 ]  
      !query'' = fromString $ query' :: Query
  (xss::[Poll]) <- query_ conn query''
  return xss

getPollsForRange :: Connection -> UTCTime -> UTCTime -> IO [Poll]
getPollsForRange conn start end = do
  let !query'  =  Data.List.unlines
                 [ "SELECT * FROM poll"
                 , "WHERE start=? AND end=?"  
                 ]  
      !query'' = fromString $ query' :: Query
      vals = [start, end] 
  (xss::[Poll]) <- query conn query'' vals
  return xss
  
deletePoll :: Connection -> IO ()
deletePoll conn = do
  let !query'  = "TRUNCATE TABLE poll RESTART IDENTITY" 
      !query'' = fromString $ query'  
  execute_ conn query''
  return ()   

