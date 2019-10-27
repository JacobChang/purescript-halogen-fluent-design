module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class.Console as Console
import FluentDesign.Components.Button as Button
import FluentDesign.Components.Checkbox as Checkbox
import FluentDesign.Components.ChoiceGroup as ChoiceGroup
import FluentDesign.Components.Dropdown as Dropdown
import FluentDesign.Components.Label as Label
import FluentDesign.Components.Link as Link
import FluentDesign.Components.Slider as Slider
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.VDom.Driver (runUI)

data Query a = Const Unit

data Action
  = HandleButton Button.Message
  | HandleCheckbox Checkbox.Message
  | HandleDropdown (Dropdown.Message String)
  | HandleChoiceGroup (ChoiceGroup.Message String)
  | HandleSlider Slider.Message

type Message = Void

type State =
  { linkState :: Link.State
  , labelState :: Label.State
  , buttonState :: Button.State
  , checkboxState :: Checkbox.State
  , dropdownState :: Dropdown.State String
  , choiceGroupState :: ChoiceGroup.State String
  , sliderState :: Slider.State }

labelState :: Label.State
labelState =
  { disabled: true
  , text: "title of label" }

linkState :: Link.State
linkState =
  { disabled: false
  , href: "https://google.com"
  , text: "title of link" }

buttonState :: Button.State
buttonState =
  Button.defaultState
    { text = "title of button"
    , secondaryText = Just "secondary text" }

checkboxState :: Checkbox.State
checkboxState =
  { disabled: false
  , checked: false
  , text: "title of checkbox" }

dropdownState :: Dropdown.State String
dropdownState =
  { disabled: false
  , selected: { key: "a", value: "a", label: "a" }
  , choices: [ { key: "a", value: "a", label: "a"},{ key: "b", value: "b", label: "b"} ] }

choiceGroupState :: ChoiceGroup.State String
choiceGroupState =
  { disabled: false
  , choices: [ { key: "a", value: "a", label: "a", selected: false },{ key: "b", value: "b", label: "b", selected: false } ] }

appState :: State
appState =
  { linkState: linkState
  , labelState: labelState
  , buttonState: buttonState
  , checkboxState: checkboxState
  , dropdownState: dropdownState
  , choiceGroupState: choiceGroupState
  , sliderState: Slider.defaultState }

type ChildSlots =
  ( buttonStandard :: Button.Slot Unit
  , buttonPrimary :: Button.Slot Unit
  , label :: Label.Slot Unit
  , link :: Link.Slot Unit
  , checkbox :: Checkbox.Slot Unit
  , dropdown :: Dropdown.Slot String Unit
  , choiceGroup :: ChoiceGroup.Slot String Unit
  , slider :: Slider.Slot Unit )

_buttonStandard :: SProxy "buttonStandard"
_buttonStandard = SProxy

_buttonPrimary :: SProxy "buttonPrimary"
_buttonPrimary = SProxy

_link :: SProxy "link"
_link = SProxy

_label :: SProxy "label"
_label = SProxy

_checkbox :: SProxy "checkbox"
_checkbox = SProxy

_dropdown :: SProxy "dropdown"
_dropdown = SProxy

_choiceGroup :: SProxy "choiceGroup"
_choiceGroup = SProxy

_slider :: SProxy "slider"
_slider = SProxy

app :: H.Component HH.HTML Query State Message Aff
app =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction } }
  where
    initialState :: State -> State
    initialState = identity

    render :: State -> H.ComponentHTML Action ChildSlots Aff
    render state =
      HH.div []
        [ HH.div_ [ HH.slot _buttonStandard unit Button.standard state.buttonState (Just <<< HandleButton) ]
        , HH.div_ [ HH.slot _buttonPrimary unit Button.primary state.buttonState (Just <<< HandleButton) ]
        , HH.div_ [ HH.slot _link unit Link.link state.linkState absurd ]
        , HH.div_ [ HH.slot _label unit Label.label state.labelState absurd ]
        , HH.div_ [ HH.slot _checkbox unit Checkbox.checkbox state.checkboxState (Just <<< HandleCheckbox) ]
        , HH.div_ [ HH.slot _dropdown unit Dropdown.dropdown state.dropdownState (Just <<< HandleDropdown) ]
        , HH.div_ [ HH.slot _choiceGroup unit ChoiceGroup.choiceGroup state.choiceGroupState (Just <<< HandleChoiceGroup) ]
        , HH.div_ [ HH.slot _slider unit Slider.slider state.sliderState (Just <<< HandleSlider) ] ]

    handleAction ::  Action -> H.HalogenM State Action ChildSlots Message Aff Unit
    handleAction action =
      case action of
        HandleButton Button.Clicked -> do
          Console.log "button clicked"
        HandleCheckbox (Checkbox.Toggled toggled) -> do
          Console.log "checkbox toggled"
        HandleDropdown (Dropdown.Selected choice) -> do
          Console.log choice.key
        HandleChoiceGroup (ChoiceGroup.Toggled choice) -> do
          Console.log choice.key
        HandleSlider (Slider.Changed progress) ->
          Console.log "progress"

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI app appState body
