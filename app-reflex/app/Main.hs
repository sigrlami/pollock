{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}

module Main where

import           Data.Aeson
import           GHC.Generics
import           Control.Concurrent
import           Control.Monad (liftM, forM)
import           Data.FileEmbed
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Widget
import           GHCJS.DOM.Document
import           GHCJS.DOM.HTMLElement
import qualified Data.Text as T
import           Data.Time
import           Data.Time.Clock
import qualified Data.Map as Map
import           System.Random
import           Data.String.Quote
import qualified Data.Set as Set
import           Safe

--------------------------------------------------------------------------------

main :: IO ()
main = do
  tStart <- getCurrentTime
  rnd    <- getStdGen
  mainWidgetWithHead headElement (bodyElement tStart)

headElement :: MonadWidget t m => m ()
headElement = do
  styleSheet "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css"
  styleSheet "https://maxcdn.bootstrapcdn.com/font-awesome/4.6.3/css/font-awesome.min.css"
  styleSheet "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.6.0/styles/haskell.min.css"
  styleSheet "app.css"
  javaScript "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/9.6.0/highlight.min.js"
    where
      styleSheet link =
        elAttr "link" (
          Map.fromList
            [ ("rel" , "stylesheet")
            , ("type", "text/css")
            , ("href", link)
            ])
        $ return ()
      javaScript link =
        elAttr "script" (
          Map.fromList
            [ ("type", "text/javascript")
            , ("src", link)
            ])
        $ return ()

bodyElement :: (MonadWidget t m) => UTCTime -> m ()
bodyElement tStart = do
  navbar
  divClass "container-fluid" $ examples tStart

navbar :: (MonadWidget t m) =>  m ()
navbar = do
   elClass "nav" "navbar navbar-default navbar-static-top" $ do
     divClass "container-fluid"  $ do
      divClass "navbar-header"  $ do
         divClass "navbar-brand" $ do
           text "Pollock Reflex FrontEnd"

examples :: (MonadWidget t m) =>  UTCTime -> m ()
examples tStart = do
   divClass "row" $ do
     divClass "col-md-3" $ do
       text ""
     divClass "col-md-5" $ do
       divClass "" example1
       divClass "" (example2 tStart)
       divClass "" example3
--       divClass "" example4
     divClass "col-md-4" $ do
       text ""

example1 :: (MonadWidget t m) => m ()
example1 = do
  demoWidget "Example1: TextBox Echo"
    textEchoCode textEcho

example2 :: (MonadWidget t m) => UTCTime -> m ()
example2 tStart = do
  demoWidget "Example2: CountDown"
    basicTimerCode (basicTimer tStart)

example3 :: (MonadWidget t m) =>  m ()
example3 = do
  demoWidget "Count clicks"
    countClicksCode countClicks

demoWidget :: MonadWidget t m => String -> String -> m a -> m a
demoWidget descr src w = do
  elClass "div" "recipe" $ do
    elDynHtmlAttr' "div" ("class" =: "recipe-header") $
      (constDyn descr)
    elClass "pre" "sourceCode haskell" $ do
      elDynHtmlAttr' "code" ("class" =: "sourceCode haskell")
        (constDyn src)
--    el "hr" (return ())
    elClass "div" "demoWidget" w

textEcho :: MonadWidget t m => m ()
textEcho = do
  let cfg  = TextAreaConfig "" never (constDyn ( "class" =: "inputee"))
  t    <- textArea cfg
  tvs  <- mapDyn identifyLanguage $ value t
  el "br" (return ())
  elClass "div" "output" $ do
    dynText (value t)
  elClass "div" "output lang"   $ do
    dynText (tvs)

textEchoCode :: String
textEchoCode =
  [s|
  textEcho :: MonadWidget t m => m ()
  textEcho = do
    let cfg  = TextAreaConfig "" never (constDyn ( "class" =: "inputee"))
    t <- textArea cfg
    tvs  <- mapDyn identifyLanguage $ value t
    el "br" (return ())
    elClass "div" "output" $ do
      dynText (value t)
    elClass "div" "output lang"   $ do
      dynText (tvs)
  |]

basicTimer :: MonadWidget t m => UTCTime -> m ()
basicTimer t0 = do
  times   <- fmap (show . _tickInfo_lastUTC) <$> (tickLossy 0.2 t0)
  timeTxt <- holdDyn "No ticks yet" times
  elClass "div" "output" $ do
    dynText timeTxt

basicTimerCode :: String
basicTimerCode =
  [s|
  basicTimer :: MonadWidget t m => UTCTime -> m ()
  basicTimer t0 = do
    times   <- fmap (show . _tickInfo_lastUTC) <$> (tickLossy 0.2 t0)
    timeTxt <- holdDyn "No ticks yet" times
    elClass "div" "output" $ do
      dynText timeTxt
|]

-------------------------------------------------------------------------------
countClicks :: MonadWidget t m => m ()
countClicks = mdo

  let initialCounters = Map.fromList [(i,()) | i <- [0..2 :: Int]]
      addCounter  cs  = Map.insert (length cs) () cs
      dropCounter cs  = case Map.maxView cs of
        Nothing      -> cs
        Just (_,cs') -> cs'

  allBins <- foldDyn ($)
             initialCounters
             (leftmost $ [dropBinButton, addBinButton])

  elClass "div" "allCounts" $ do

    listWithKey allBins $ \k oneBin -> mdo

      nClicks <- foldDyn (\() -> succ) (0 :: Int) (_el_clicked boxEl)

      attrsDyn <- forDyn nClicks $ \b ->
        Map.fromList
        [("class","countBin noselect")
        ,("style","height:" ++ show (30+b*3) ++ "px;" ++
                  "background-color: hsl(" ++
                  show (b*5) ++ ",50%,50%);")]

      (boxEl,_) <- elDynAttr' "div" attrsDyn $ do
        display nClicks

      return ()

  el "br" (return ())

  -- Turn links' () click events into functions to apply to the bin map
  dropBinButton <- (fmap (\() -> dropCounter) . _link_clicked) <$>
                   linkClass "Remove " "reflexLink noselect"
  addBinButton  <- (fmap (\() -> addCounter)  . _link_clicked) <$>
                   linkClass " Add" "reflexLink noselect"
  return ()


countClicksCode :: String
countClicksCode = [s|
countClicks :: MonadWidget t m => m ()
countClicks = mdo
  let initialCounters = Map.fromList [(i,()) | i <- [0..2 :: Int]]
      addCounter  cs  = Map.insert (length cs) () cs
      dropCounter cs  = case Map.maxView cs of
        Nothing      -> cs
        Just (_,cs') -> cs'

  allBins <- foldDyn ($)
             initialCounters
             (leftmost $ [dropBinButton, addBinButton])

  elClass "div" "allCounts" $ do

    listWithKey allBins $ \k oneBin -> mdo

      nClicks <- foldDyn (\() -> succ) (0 :: Int) (_el_clicked boxEl)

      attrsDyn <- forDyn nClicks $ \b ->
        Map.fromList
        [("class","countBin noselect")
        ,("style","height:" ++ show (30+b*3) ++ "px;" ++
                  "background-color: hsl("++
                  show (b*5) ++ ",50%,50%);")]

      (boxEl,_) <- elDynAttr' "div" attrsDyn $ do
        display nClicks

      return ()

  el "br" (return ())

  -- Turn links' () click events into functions to apply to the bin map
  dropBinButton <- (fmap (\() -> dropCounter) . _link_clicked) <$>
                   linkClass "Remove Bin" "reflexLink noselect"
  addBinButton  <- (fmap (\() -> addCounter)  . _link_clicked) <$>
                   linkClass "Add Bin" "reflexLink noselect"
  return ()
|]

 --------------------------------------------------------------------------
rmdups :: Ord a => [a] -> [a]
rmdups = rmdups' Set.empty where
  rmdups' _ [] = []
  rmdups' a (b : c) = if Set.member b a
    then rmdups' a c
    else b : rmdups' (Set.insert b a) c

data Lang = Latin | Hebrew | Cyrillic | Other
  deriving (Show, Eq, Ord)

isCyrillic   :: Char -> Bool
isCyrillic c = c >= '\x0400' && c <= '\x04FF'

-- | check if Basic Latin
isLatin   :: Char -> Bool
isLatin c = c >= '\x0000' && c <= '\x007F'

isHebrew   :: Char -> Bool
isHebrew c = c >= '\x0590' && c <= '\x05FF'

-- English  Range: 0000–007F
-- Cyrillic Range: 0400–04FF
-- Hebrew   Range: 0590–05FF

checkLang :: Char -> Lang
checkLang char
  | isLatin    char = Latin
  | isHebrew   char = Hebrew
  | isCyrillic char = Cyrillic
  | otherwise       = Other

identifyLanguage :: String -> String
identifyLanguage str' = do
  let str   = T.pack str'
      res   = map checkLang $ T.unpack $ T.replace " " "" $ T.strip str
      res'  = rmdups res
      res'' = T.intercalate " - " $ map (T.pack . show) res'
  T.unpack $ res''
