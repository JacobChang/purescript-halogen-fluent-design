module FluentDesign.Components.Slider where

import Prelude

import CSS as CSS
import Data.Array (cons)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Slot = H.Slot Query Message

data Query a = Progress (Int -> a)

type State =
  { progress :: Int
  , disabled :: Boolean
  , isDragging :: Boolean }

defaultState :: State
defaultState =
  { disabled: false
  , progress: 0
  , isDragging: false }

data Message
  = Changed Int

data Action
  = Change Int
  | DragStart
  | DragMove
  | DragEnd

slider :: forall m. H.Component HH.HTML Query State Message m
slider =
  H.mkComponent
    { initialState: initialState
    , render: render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction} }
  where
    initialState :: State -> State
    initialState state = state

    render :: State -> H.ComponentHTML Action () m
    render state =
      HH.div (cons elemClass props) [ activeProgress, inactiveProgress, indicator ]
      where
        activeStyle =
          HCSS.style do CSS.width (CSS.pct $ toNumber state.progress)
        inactiveStyle =
          HCSS.style do CSS.width (CSS.pct $ toNumber (100 - state.progress))
        activeProgress = HH.span [ HP.class_ $ HH.ClassName "slider__progress slider__progress--active", activeStyle ] []
        inactiveProgress = HH.span [ HP.class_ $ HH.ClassName "slider__progress slider__progress--inactive", inactiveStyle ] []
        indicator =
          HH.span
            [ HP.class_ $ HH.ClassName "slider__indicator"
            , HE.onMouseDown \_ -> Just DragStart ]
            []
        props =
          [ HE.onMouseMove \evt -> Just DragMove
          , HE.onMouseUp \evt -> Just DragEnd
          , HE.onMouseOut \evt -> Just DragEnd ]
        elemClass =
          case state.disabled of
            true -> HP.class_ $ HH.ClassName "slider slider--disabled"
            false -> HP.class_ $ HH.ClassName "slider"

    handleAction :: Action -> H.HalogenM State Action () Message m Unit
    handleAction action =
      case action of
        DragStart -> do
          H.modify_ (\st -> st { isDragging = true })
        DragEnd -> do
          newState <- H.modify (\st -> st { isDragging = false, progress = 100 })
          H.raise $ Changed newState.progress
        _ -> do
          pure unit
