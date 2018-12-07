module Types.Poll where
  
import Prelude
import Data.List
import Data.DateTime

type Poll =
  { id          :: Int
  , title       :: String
  , description :: String
  , questions   :: List String
  , startTime   :: DateTime
  , endTime     :: DateTime
  , isActive    :: Boolean
  }