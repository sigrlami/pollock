import Control.Applicative
import Control.Lens
import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Session
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe
import Heist.SpliceAPI
import qualified Heist.Interpreted as I
import qualified Data.Text as T

data Pollock 
  = Pollock { _heist :: Snaplet (Heist Pollock)
            , _sess  :: Snaplet SessionManager
            , _auth  :: Snaplet (AuthManager Pollock)  
            }

makeLenses ''Pollock

instance HasHeist Pollock where
  heistLens = subSnaplet heist
 
-- | The indexHandler will be invoked whenever someone 
--   accesses the root URL, "/".
handlerIndex :: Handler Pollock Pollock ()
handlerIndex = render "index"

-- Used to output an error to the user where needed.
renderError :: Show a => a -> Handler Pollock (AuthManager Pollock) ()
renderError = renderError' . show
renderError' :: String -> Handler Pollock (AuthManager  Pollock) ()
renderError' s = renderWithSplices "_error" $ "errormsg" ## I.textSplice . T.pack $ s

-- Triggers on the /signup page
-- Form is shown on GET requests, form is handled on POST requests.
-- This idea is used on every page with a form.
handlerSignup :: Handler Pollock (AuthManager Pollock) ()
handlerSignup = method GET handleForm <|> method POST handleFormSubmit
    where
        handleForm = render "signup"
        handleFormSubmit = registerUser "username" "password" >>= afterRegister
        afterRegister :: Either AuthFailure AuthUser -> Handler Pollock (AuthManager Pollock) ()
        afterRegister (Left af) = renderError af
        afterRegister _ = render "_signupsuccess"

-- Triggers on the /signin page
handlerLogin :: Handler Pollock (AuthManager Pollock) ()
handlerLogin = method GET handleForm <|> method POST handleFormSubmit
    where
        handleForm = render "signin"
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
      h <- nestSnaplet "heist" heist $ heistInit "templates"
      s <- nestSnaplet "sess" sess $ initCookieSessionManager "site_key.txt" "sess" (Just 3600)
      a <- nestSnaplet "auth" auth $ initJsonFileAuthManager defAuthSettings sess "users.json"
      addRoutes [ ("static" , serveDirectory "static")
                , (""       , handlerIndex)
                , ("/signup", with auth handlerSignup)
                , ("/login" , with auth handlerLogin)
                , ("/logout", with auth handlerLogout) 
                ]
      return $ Pollock { _heist = h, _sess = s, _auth = a }

main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing pollockInit
  quickHttpServe site
