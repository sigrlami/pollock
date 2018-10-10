module Main where

import Prelude

import App.CountForm (CountFormProps, countFormClass)
import App.MaterialUI (muiThemeProviderClass)
import Control.Monad.Eff (Eff)
import Control.Monad.Except (runExcept)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument)
import DOM.HTML.Window (document)
import DOM.Node.NonElementParentNode (getElementById)
import DOM.Node.Types (Element, ElementId(..), documentToNonElementParentNode)
import Data.Either (hush)
import Data.Foldable (for_)
import Data.Foreign (Foreign, readString)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafeCrashWith)
import Control.Monad.Eff.Exception (EXCEPTION)

main :: forall eff. Eff (dom :: DOM, exception :: EXCEPTION, uport :: UPORT | eff) Unit
main = do
  elem <- getElem
  let props = { contractAddress }
  for_ elem $ render $ ui props
  where
    ui :: CountFormProps -> R.ReactElement
    ui props =
      R.createElement muiThemeProviderClass unit
        [ R.createFactory countFormClass props ]

getElem :: forall eff. Eff (dom :: DOM | eff) (Maybe Element)
getElem = do
  win <- window
  doc <- document win
  getElementById (ElementId "app") (documentToNonElementParentNode (htmlDocumentToDocument doc))

foreign import simpleStorageAddr :: Foreign
