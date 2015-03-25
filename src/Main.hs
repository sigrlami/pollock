import Snap
import qualified Data.Text as T

site :: Snap () 
site = writeText $ T.pack "Hello, Pollock!"

main :: IO ()
main = quickHttpServe site
