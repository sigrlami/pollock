{-# LANGUAGE TemplateHaskell #-}

module Application where

import Control.Lens
import Snap
import Snap.Snaplet.Heist
import Snap.Snaplet.Auth
import Snap.Snaplet.Session
import Snap.Snaplet.SqliteSimple    

data Pollock 
  = Pollock { _heist :: Snaplet (Heist Pollock)
            , _sess  :: Snaplet SessionManager
            , _auth  :: Snaplet (AuthManager Pollock)
            , _db    :: Snaplet Sqlite            
            }

makeLenses ''Pollock

instance HasHeist Pollock where
  heistLens = subSnaplet heist
