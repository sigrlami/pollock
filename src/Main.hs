import Snap
import Snap.Snaplet.Heist
import Control.Lens
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
              "Best pollin system!" 
              Nothing 
  $ do
      h <- nestSnaplet "heist" heist $ heistInit "templates"
      addRoutes [("", indexHandler)]
      return $ Memoise { _heist = h }

main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing memoiseInit -- Init Pollock snaplet
  quickHttpServe site -- Start the Snap server
