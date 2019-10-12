module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import FluentDesign.Components.Button as Button
import FluentDesign.Components.Label as Label
import FluentDesign.Components.Link as Link
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

data Query a = Const Unit

data Action
  = HandleButton Button.Message

type Message = Void

type State =
  { toggleCount :: Int
  , linkState :: Link.State
  , labelState :: Label.State
  , buttonStandardState :: Button.StandardState }

labelState :: Label.State
labelState =
  { disabled: true
  , text: "title of label" }

linkState :: Link.State
linkState =
  { disabled: true
  , href: "https://google.com"
  , text: "title of link" }

buttonStandardState :: Button.StandardState
buttonStandardState =
  { disabled: true
  , text: "title of standard button" }

appState :: State
appState =
  { linkState: linkState
  , labelState: labelState
  , buttonStandardState: buttonStandardState
  , toggleCount: 0 }

type ChildSlots =
  ( button :: Button.Slot Unit
  , label :: Label.Slot Unit
  , link :: Link.Slot Unit )

_button :: SProxy "button"
_button = SProxy

_link :: SProxy "link"
_link = SProxy

_label :: SProxy "label"
_label = SProxy

app :: forall m. H.Component HH.HTML Query State Message m
app =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval }
  where
    initialState :: State -> State
    initialState = identity

    render :: State -> H.ComponentHTML Action ChildSlots m
    render state =
      HH.div []
        [ HH.div_ [ HH.slot _button unit Button.standard state.buttonStandardState (Just <<< HandleButton) ]
        , HH.div_ [ HH.slot _link unit Link.link state.linkState absurd ]
        , HH.div_ [ HH.slot _label unit Label.label state.labelState absurd ] ]

    handleAction ::  Action -> H.HalogenM State Action ChildSlots Message m Unit
    handleAction action =
      case action of
        HandleButton (Button.Clicked _) -> do
          H.modify_ (\st -> st { toggleCount = st.toggleCount + 1 })

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI app appState body
