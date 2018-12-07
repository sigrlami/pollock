module Types.User where
  
type User =
  { id       :: Int
  , name     :: String
  , email    :: String
  , nickname :: String
  , token    :: String
  }