{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Applicative
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Lens
import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.SqliteSimple
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Snaplet.SqliteSimple
import Snap.Util.FileServe
import Heist.SpliceAPI
import Application
import Db
import Data.Maybe
import           Data.Time
import           Text.Read
import qualified Heist.Interpreted as I
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

--------------------------------------------------------------------------------    

routes :: [(BS.ByteString, Handler Pollock Pollock ())]
routes = [ ("/signup"             , with auth handlerSignup)
         , ("/login"              , with auth handlerLogin)
         , ("/logout"             , with auth handlerLogout)
         , ("/polls/new"          , with auth handlerPollNew)
         --, ("/poll/view/:pollid"   , with auth handlerPollView)
         --, ("/poll/delete/:pollid", with auth handlerPollDelete)
         , ("static"              , serveDirectory "static")
         , (""                    , handlerIndex)
         ]           
              
-- | The indexHandler will be invoked whenever someone 
--   accesses the root URL, "/".
handlerIndex :: Handler Pollock Pollock ()
handlerIndex = do
  let start = UTCTime (fromGregorian 2016 02 1) 0
  let end   = UTCTime (fromGregorian 2016 03 1) 0
  polls <- withTop db $ getPollsForRange start end
  logError $ BSC.pack $ (show polls) 
  renderWithSplices "index" $ do
    "polls" ## renderPolls polls

-- Used to output an error to the user where needed.
renderError :: Show a => a -> Handler Pollock (AuthManager Pollock) ()
renderError = renderError' . show
              
renderError' :: String -> Handler Pollock (AuthManager  Pollock) ()
renderError' s = renderWithSplices "_error" $
                 "errormsg" ## I.textSplice . T.pack $ s

-- Turn an event into splices. Needed for rendering.
renderPoll :: Monad n => Poll -> Splices (I.Splice n)
renderPoll poll = do
    "pollid"          ## I.textSplice . T.pack . show $ pollId    poll
    "polltitle"      ## I.textSplice . T.pack . show $ pollTitle poll
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
        withTop db $ savePoll user $ parseParameters params
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
                      
-- Triggers on the /signin page
handlerLogin :: Handler Pollock (AuthManager Pollock) ()
handlerLogin = method GET handleForm <|> method POST handleFormSubmit
    where
        handleForm = render "login"
        handleFormSubmit = loginUser "username" "password" Nothing renderError (redirect "/")

-- Triggers on the /signout page
handlerLogout :: Handler Pollock (AuthManager Pollock) ()
handlerLogout = logout >> redirect "/"

-- | Build a new Pollock snaplet.
pollockInit :: SnapletInit Pollock Pollock
pollockInit = 
  makeSnaplet "pollock" 
              "Best polling system!" 
              Nothing 
  $ do
      h <- nestSnaplet "heist" heist $
             heistInit "templates"
      d <- nestSnaplet "db" db sqliteInit 
      s <- nestSnaplet "sess"  sess  $
             initCookieSessionManager "site_key.txt" "sess" (Just 3600)
      a <- nestSnaplet "auth"  auth  $
             initSqliteAuth sess d
      
      let c = sqliteConn $ d ^# snapletValue
      liftIO $ withMVar c $ \conn -> Db.createTables conn

      addRoutes routes
      addAuthSplices h auth -- add <ifLoggedIn> <ifLoggedOut> tags support
      return $ Pollock { _heist = h, _sess = s, _auth = a , _db=d}
             
main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing pollockInit
  quickHttpServe site
