import Snap
import Snap.Snaplet.Heist
import Control.Lens
import Snap.Util.FileServe
import qualified Data.Text as T

-- | The Pollock type identifies our application 
--   and holds anything our snaplet needs to function.
data Pollock 
  = Pollock { _heist :: Snaplet (Heist Pollock)
            }

makeLenses ''Pollock

instance HasHeist Pollock where
  heistLens = subSnaplet heist
 
-- | The indexHandler will be invoked whenever someone 
--   accesses the root URL, "/".
indexHandler :: Handler Pollock Pollock ()
indexHandler = render "index"

-- | Build a new Pollock snaplet.
memoiseInit :: SnapletInit Pollock Pollock
memoiseInit = 
  makeSnaplet "pollock" 
              "Best polling system!" 
              Nothing 
  $ do
      h <- nestSnaplet "heist" heist $ heistInit "templates"
      addRoutes [ ("static", serveDirectory "static")
                , ("", indexHandler)]
      return $ Pollock { _heist = h }

main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing memoiseInit
  quickHttpServe site
