{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db (
    User(..)
  , Poll(..)
  , createTables
  , savePoll
  , getPoll
  , getPollsForUser
  , getPollsForRange
  , deletePoll)
    where

import           Control.Applicative
import           Control.Monad
import           Data.List
import qualified Data.Text as T
import           Data.Time (UTCTime)
import qualified Database.SQLite.Simple as S
import           Database.SQLite.Simple.FromField
import           Database.SQLite.Simple.ToField
import           Database.SQLite.Simple.Internal
import           Database.SQLite.Simple.Ok
import           Snap.Snaplet
import           Snap.Snaplet.SqliteSimple

import           Application

-----------------------------------------------------------------------------

data User = User Int T.Text deriving (Show)

data Poll = Poll
    { pollId    :: Int
    , pollTitle :: T.Text
    , pollDesc  :: T.Text
    , pollStart :: UTCTime
    , pollEnd   :: UTCTime
    , pollOwner :: Int
    } deriving (Show)
          
-- For pulling from the database
instance FromRow Poll where
    fromRow = Poll <$> field <*> field <*> field <*> field <*> field <*> field

-- Used to sort polls by time when getting a range of polls.
pollTimeOrder :: Poll -> Poll -> Ordering
pollTimeOrder e1 e2 = case pollStart e1 `compare` pollStart e2 of
    EQ -> pollEnd e1 `compare` pollEnd e2
    LT -> LT
    GT -> GT              
        
--------------------------------------------------------------------------------
-- Database creation
--
-- createTables is run at every start up. If it doesn't find the  pollstable,
-- it creates it. (Note that Auth handles the user table)
--------------------------------------------------------------------------------

tableExists :: S.Connection -> String -> IO Bool
tableExists conn tblName = do
    r <- S.query conn "SELECT name FROM sqlite_master WHERE type='table' AND name=?" (Only tblName)
    case r of
        [Only (_ :: String)] -> return True
        _ -> return False

createTables :: S.Connection -> IO ()
createTables conn = do
    -- Create `polls` table if it doesn't exist yet
    isPollTableCreated <- tableExists conn "polls"
    unless isPollTableCreated $
        S.execute_ conn
            (S.Query $
             T.concat [ "CREATE TABLE polls ("
                      , "id      INTEGER PRIMARY KEY,"
                      , "title   TEXT NOT NULL,"
                      , "desc    TEXT,"
                      , "start   TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,"
                      , "end     TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL,"
                      , "user_id INTEGER,"
                      , "deleted INTEGER DEFAULT 0,"
                      , "FOREIGN KEY(user_id) REFERENCES users(uid))"
                      ])

--------------------------------------------------------------------------------
-- Communication with the database
--
-- Functions to get polls from the database as well as saving to it.
--------------------------------------------------------------------------------

getPoll :: Maybe Int -> Handler Pollock Sqlite [Poll]
getPoll Nothing = return []
getPoll (Just eid) =
    query "SELECT id, title, desc, start, end, user_id FROM polls WHERE  deleted = 0 AND id = ?" (Only eid)

getPollsForUser :: User -> Handler Pollock Sqlite [Poll]
getPollsForUser (User user_id _) =
    query "SELECT id, title, desc, start, end, user_id FROM polls WHERE  deleted = 0 AND user_id = ?" (Only user_id)

-- Poll is in range if its end is after the start of the range AND its start
-- is before the end of the range. This gets all those partially in the range.
getPollsForRange :: UTCTime -> UTCTime -> Handler Pollock Sqlite [Poll]
getPollsForRange start end = do
    nr <- polls
    return $ sortBy pollTimeOrder nr
    where
        polls :: Handler Pollock Sqlite [Poll]
        polls = query "SELECT id, title, desc, start, end, user_id FROM   polls WHERE deleted = 0 AND start < ? AND end > ?" (end, start)
        
savePoll :: User -> Maybe (T.Text, T.Text, UTCTime, UTCTime) -> Handler Pollock Sqlite ()
savePoll (User uid _) (Just (title, desc, start, end)) = 
    execute "INSERT INTO polls (title, desc, start, end, user_id) VALUES (?,?,?,?,?)"
              ( title
              , desc
              , start
              , end
              , uid
              )
savePoll _ _ = return ()

-- Only deletes if user owns poll as well
-- TODO: check is already done when it is used. Maybe doesn't belong here.
deletePoll :: User -> Poll -> Handler Pollock Sqlite ()
deletePoll (User uid _) poll =
    execute "UPDATE polls SET deleted = 1 WHERE id = ? AND user_id = ?" (pollId poll, uid)
