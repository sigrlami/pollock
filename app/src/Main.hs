{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE TemplateHaskell     #-}

import           Control.Applicative
import           Control.Concurrent
import           Control.Lens
import qualified Control.Exception as Exception
import           Control.Monad.IO.Class
import qualified System.IO as IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Database.PostgreSQL.Simple
import           Servant.Server.Internal.SnapShims
import           Snap
import           Snap.Http.Server
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.PostgresqlSimple
import           Snap.Snaplet.Config
import           Snap.Snaplet.Session
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
import           Servant.API
import           Servant (serveSnap, Server)
import           Text.Read
#ifdef DEVELOPMENT
--import Snap.Loader.Dynamic (loadSnapTH)
#else
import Snap.Loader.Static (loadSnapTH)
#endif
import Snap.Loader.Static (loadSnapTH)

import           Application
import           Db
import           Handler
import           Types.Poll
import           Types.Channel

--------------------------------------------------------------------------------    

routes :: [(BS.ByteString, Handler Pollock Pollock ())]
routes = [ ("/"                    , handlerIndex)  
         , ("/app"                 , with auth handlerDashboard)
         , ("/signup"              , with auth handlerSignup)
         , ("/login"               , with auth handlerLogin)
         , ("/logout"              , with auth handlerLogout)
         , ("/polls/new"           , with auth handlerPollNew)
         , ("/poll/view/:pollid"   , with auth handlerPollView)
         , ("/poll/delete/:pollid" , with auth handlerPollDelete)
         , ("static"               , serveDirectory "static")
         , (".well-known"          , serveDirectory ".well-known")   
         ]           

-- | Build a new Pollock snaplet.
appInit :: SnapletInit Pollock Pollock
appInit = 
  makeSnaplet "Pollock" "Best polling system!" Nothing $ do
    h <- nestSnaplet "heist" heist $
           heistInit "templates"
         
    d <- nestSnaplet "db" db $
           pgsInit 

    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" Nothing (Just 3600)

    a <- nestSnaplet "auth" auth $
           initPostgresAuth sess d
    
    addRoutes routes
    addAuthSplices h auth -- add <ifLoggedIn> <ifLoggedOut> tags support

    return $ Pollock { _heist = h, _sess = s, _auth = a , _db=d}

getConf :: IO (Config Snap AppConfig)
getConf = commandLineAppConfig defaultConfig

getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
    (msgs, site, cleanup) <- runSnaplet (appEnvironment =<< getOther conf) appInit
    IO.hPutStrLn IO.stderr (T.unpack msgs)
    return (site, cleanup)

main :: IO ()
main = do
  (conf, site, cleanup) <- $(loadSnapTH [| getConf |] 'getActions ["snaplets/heist/templates"])
  _ <- Exception.try (httpServe conf site) :: IO (Either Exception.SomeException ())
  cleanup
