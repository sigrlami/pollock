import Snap
import qualified Data.Text as T

site = writeText $ T.pack "Hello, Pollock!"

main = quickHttpServe site
