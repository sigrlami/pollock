module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Elements as HEM
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Types.User
import Types.Poll

--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  log "Hello Pollock!"
  --HA.runHalogenAff do
  --  body <- HA.awaitBody
  --  runUI (HEM.h1 "Welcome to Pollock Voting app!") unit body