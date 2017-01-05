{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE FlexibleInstances #-}

module Application where

import           Control.Lens
import           Control.Monad.Reader (local)
import           Control.Monad.State (get)
import qualified Data.ByteString as BS
import           Snap
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Snaplet.Session

--------------------------------------------------------------------------------

data Pollock 
  = Pollock { _heist :: Snaplet (Heist Pollock)
            , _sess  :: Snaplet SessionManager
            , _auth  :: Snaplet (AuthManager Pollock)
            , _db    :: Snaplet Postgres            
            }

makeLenses ''Pollock

instance HasHeist Pollock where
  heistLens = subSnaplet heist

instance HasPostgres (Handler b Pollock) where
  getPostgresState = with db get
  setLocalPostgresState s = local (set (db . snapletValue) s)

instance HasPostgres (Handler Pollock (AuthManager Pollock)) where
    getPostgresState = withTop db get
    
type AppGlobalHandler = Handler Pollock Pollock
















