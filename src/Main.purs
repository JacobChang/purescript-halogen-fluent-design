module Main where

import Prelude

import Data.Const (Const)
import Data.Symbol (SProxy(..))
import Effect (Effect)
import FluentDesign.Components.Button as Button
import FluentDesign.Components.Link as Link
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

type State =
  { linkState :: Link.State
  , buttonStandardState :: Button.StandardState }

linkState :: Link.State
linkState =
  { disabled: true
  , href: "https://google.com"
  , text: "Google" }

buttonStandardState :: Button.StandardState
buttonStandardState =
  { disabled: true
  , text: "Google" }

appState :: State
appState =
  { linkState: linkState
  , buttonStandardState: buttonStandardState }

type ChildSlots =
  ( button :: Button.Slot Unit
  , link :: Link.Slot Unit )

_button :: SProxy "button"
_button = SProxy

_link :: SProxy "link"
_link = SProxy

app :: forall m. H.Component HH.HTML (Const Unit) State Void m
app =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval }
  where
    initialState :: State -> State
    initialState = identity

    render :: State -> H.ComponentHTML Void ChildSlots m
    render state =
      HH.div []
        [ HH.slot _button unit Button.standard state.buttonStandardState absurd
        , HH.slot _link unit Link.link state.linkState absurd ]

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI app appState body
