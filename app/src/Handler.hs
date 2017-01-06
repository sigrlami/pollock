{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE TemplateHaskell     #-}

module Handler where

import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson
import qualified Data.Aeson.Lens   as ALens
import qualified Data.Aeson.Parser as AP
import           Data.Aeson.Types
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Char8      as BSC
import qualified Data.ByteString.Lazy       as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.ByteString.UTF8       as BSU
import           Data.Char
import           Data.Function (on)
import qualified Data.HashMap.Strict as H
import           Data.List
import           Data.List (sortBy)
import           Data.List.Split
import           Data.Map.Syntax
import           Data.Maybe
import qualified Data.Set as Set
import           Data.String.Utils
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Format   as TF
import qualified Data.Text.Lazy     as TL
import           Data.Text.Lazy.Builder
import           Data.Time
import           Data.Time.Format (formatTime, defaultTimeLocale)
import           Data.Tuple.Select
import           Database.PostgreSQL.Simple
import           Heist
import qualified Heist.Interpreted as I
import qualified Network.HTTP.Client as HttpClient
import           Network.HTTP.Types.Status
import           Numeric
import           Safe
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session
import           System.IO.Unsafe
import           System.Random
import           Text.Read
import           Data.Default

import           Application
import           Db
import           Types.Poll
import           Types.Channel

--------------------------------------------------------------------------------

              
-- | The indexHandler will be invoked whenever someone 
--   accesses the root URL, "/".
handlerDashboard :: Handler Pollock (AuthManager  Pollock) ()
handlerDashboard = do
  let start = UTCTime (fromGregorian 2016 02 1) 0
  let end   = UTCTime (fromGregorian 2016 03 1) 0
  polls <-  liftPG $ \conn -> liftIO $ Db.getPollsForRange conn start end
  logError $ BSC.pack $ (show polls) 
  renderWithSplices "dashboard" $ do
    "polls" ## renderPolls polls

handlerIndex :: Handler Pollock Pollock ()
handlerIndex = render "index"

-- Used to output an error to the user where needed.
renderError :: Show a => a -> Handler Pollock (AuthManager Pollock) ()
renderError = renderError' . show
              
renderError' :: String -> Handler Pollock (AuthManager  Pollock) ()
renderError' s = renderWithSplices "_error" $
                 "errormsg" ## I.textSplice . T.pack $ s

-- Turn an event into splices. Needed for rendering.
renderPoll :: Monad m => Poll -> Splices (I.Splice m)
renderPoll poll = do
    "pollid"          ## I.textSplice . T.pack . show $ pollId    poll
    "polltitle"       ## I.textSplice . T.pack . show $ pollTitle poll
    "polldescription" ## I.textSplice . T.pack . show $ pollDesc  poll
    "pollstart"       ## I.textSplice . T.pack . show $ pollStart poll
    "pollend"         ## I.textSplice . T.pack . show $ pollEnd   poll
    "pollowner"       ## I.textSplice . T.pack . show $ pollOwner poll

renderPolls :: [Poll] -> SnapletISplice Pollock
renderPolls = I.mapSplices $ I.runChildrenWith . renderPoll

-- getParam returns Maybe BS.ByteString and we always want to convert it
readBSMaybe :: Read a => Maybe BS.ByteString -> Maybe a
readBSMaybe mbs = mbs >>= readBSMaybe'
readBSMaybe' :: Read a => BS.ByteString -> Maybe a
readBSMaybe' = readMaybe . T.unpack . T.decodeUtf8

-- We do this pretty often too
toTextSplice :: Show a => a -> SnapletISplice Pollock
toTextSplice = I.textSplice . T.pack . show

-- Triggers on the /signup page
-- Form is shown on GET requests, form is handled on POST requests.
-- This idea is used on every page with a form.
handlerSignup :: Handler Pollock (AuthManager Pollock) ()
handlerSignup = method GET handleForm <|> method POST handleFormSubmit
    where
        handleForm = render "signup"
        handleFormSubmit = registerUser "username" "password" >>= afterSignup

afterSignup :: Either AuthFailure AuthUser
            -> Handler Pollock (AuthManager Pollock) ()
afterSignup (Left af) = renderError af
afterSignup _         = render "_signupsuccess"

withLoggedInUser :: (Db.User -> Handler Pollock (AuthManager Pollock) ())
                 -> Handler Pollock (AuthManager Pollock) ()
withLoggedInUser action =
    currentUser >>= go
    where
        go :: Maybe AuthUser -> Handler Pollock (AuthManager Pollock) ()
        go Nothing = renderError' "You need to be logged in to access this page."
        go (Just u) = case userId u of
                           Just uid -> action (Db.User (read . T.unpack $ unUid uid) (userLogin u))
                           Nothing  -> renderError' "You don't have a userid?"

----------------------------------------------------------------------
-- Handler for Polls action

handlerPollNew :: Handler Pollock (AuthManager Pollock) ()
handlerPollNew = method GET (withLoggedInUser handleForm) <|> method POST (withLoggedInUser handleFormSubmit)
  where
    handleForm :: Db.User -> Handler Pollock (AuthManager Pollock) ()
    handleForm _ = render "polls/new"

    handleFormSubmit :: Db.User -> Handler Pollock (AuthManager Pollock) ()
    handleFormSubmit user = 
      do
        logError "r"
        parameters <- mapM getParam ["title", "description", "start", "end"]
        let params = fromMaybe [] $ sequence parameters
        logError $ BSC.pack $ (show params)
        let poll = def
            poll'= parseParameters' params
        liftPG $ \conn -> liftIO $ Db.savePoll conn poll
        redirect "/"
      
    parseParameters :: [BS.ByteString]
                    -> Maybe (T.Text, T.Text, UTCTime, UTCTime)
    parseParameters [title, desc, start, end] = do
      let title' = T.decodeUtf8 title
      let desc'  = T.decodeUtf8 desc
      start'     <- readBSMaybe' start
      end'       <- readBSMaybe' end
      return (title', desc', start', end')
    parseParameters _ = Nothing

    parseParameters' :: [BS.ByteString] -> Maybe Poll
    parseParameters' [title, desc, start, end] = do
      let title' = T.decodeUtf8 title
      let desc'  = T.decodeUtf8 desc
      start'     <- readBSMaybe' start
      end'       <- readBSMaybe' end
      -- let start'' = 
      --    end''   =   
      return $ Poll 0 (Just title') (Just desc') (Just start') (Just end') (Just 0) Nothing 
    parseParameters' _ = Nothing
    
    
-- Triggers on the /signin page
handlerLogin :: Handler Pollock (AuthManager Pollock) ()
handlerLogin = method GET handleForm <|> method POST handleFormSubmit
    where
        handleForm = render "login"
        handleFormSubmit = loginUser "username" "password" Nothing renderError (redirect "/app")

-- Triggers on the /signout page
handlerLogout :: Handler Pollock (AuthManager Pollock) ()
handlerLogout = logout >> redirect "/"
