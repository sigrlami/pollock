{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db ( User(..)
          , Poll(..)
            --, createTables
          , savePoll
          , getPoll
          , getPollsForUser
          , getPollsForRange
          , deletePoll
          ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString.Char8 as BSC
import           Data.List
import qualified Data.Text as T
import           Data.Time (UTCTime)
import qualified Database.PostgreSQL.Simple as S
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.Internal
import           Database.PostgreSQL.Simple.Ok
import           Database.PostgreSQL.Simple.ToField
import           Snap.Snaplet

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
        
--------------------------------------------------------------------------------
-- Database creation
--
-- createTables is run at every start up. If it doesn't find the  pollstable,
-- it creates it. (Note that Auth handles the user table)
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- Communication with the database
--
-- Functions to get polls from the database as well as saving to it.
--------------------------------------------------------------------------------

-- getPoll :: Maybe Int -> IO [Poll]
-- getPoll Nothing = return []
-- getPoll (Just eid) =
--     query "SELECT id, title, desc, start, end, user_id FROM polls WHERE  deleted = 0 AND id = ?" (Only eid)

-- getPollsForUser :: User -> IO [Poll]
-- getPollsForUser (User user_id _) =
--     query "SELECT id, title, desc, start, end, user_id FROM polls WHERE  deleted = 0 AND user_id = ?" (Only user_id)

-- -- Poll is in range if its end is after the start of the range AND its start
-- -- is before the end of the range. This gets all those partially in the range.
-- getPollsForRange :: UTCTime -> UTCTime ->IO [Poll]
-- getPollsForRange start end = do
--     nr <- polls
--     return $ sortBy pollTimeOrder nr
--     where
--         polls :: Handler Pollock Sqlite [Poll]
--         polls = query "SELECT id, title, desc, start, end, user_id FROM   polls WHERE deleted = 0 AND start < ? AND end > ?" (end, start)
        
-- savePoll :: User -> Maybe (T.Text, T.Text, UTCTime, UTCTime) -> Handler Pollock Sqlite ()
-- savePoll (User uid _) (Just (title, desc, start, end)) =
--     execute "INSERT INTO polls (title, desc, start, end, user_id) VALUES (?,?,?,?,?)"
--               ( title
--               , desc
--               , start
--               , end
--               , uid
--               )
-- savePoll _ _ = return ()

-- -- Only deletes if user owns poll as well
-- -- TODO: check is already done when it is used. Maybe doesn't belong here.
-- deletePoll :: User -> Poll -> Handler Pollock Sqlite ()
-- deletePoll (User uid _) poll =
--     execute "UPDATE polls SET deleted = 1 WHERE id = ? AND user_id = ?" (pollId poll, uid)

getPoll  = undefined
savePoll = undefined
getPollsForUser = undefined
getPollsForRange = undefined
deletePoll = undefined

