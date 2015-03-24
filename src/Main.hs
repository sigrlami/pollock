import Snap
import qualified Data.Text as T

-- | The Pollock type identifies our application 
--   and holds anything our snaplet needs to function.
data Pollock = Pollock { } 

-- | The indexHandler will be invoked whenever someone 
--   accesses the root URL, "/".
indexHandler :: Handler Pollock Pollock ()
indexHandler = writeText $ T.pack "Hello, world!"

-- | Build a new Pollock snaplet.
memoiseInit :: SnapletInit Pollock Pollock
memoiseInit = 
  makeSnaplet "pollock" 
              "Best pollin system!" 
              Nothing 
  $ do
      addRoutes [("", indexHandler)]
      return $ Memoise { }

main :: IO ()
main = do
  (_, site, _) <- runSnaplet Nothing memoiseInit -- Init Pollock snaplet
  quickHttpServe site -- Start the Snap server
