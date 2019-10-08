module Main where

import FluentDesign.Components.Link as Link

import Prelude
import Effect (Effect)
import Effect.Aff (Aff, forkAff, launchAff)
import Effect.Class.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

linkState :: Link.State
linkState =
  { disabled: true
  , href: "https://google.com"
  , text: "Google" }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Link.link linkState body
