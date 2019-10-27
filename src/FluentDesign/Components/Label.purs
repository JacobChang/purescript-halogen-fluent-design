module FluentDesign.Components.Label where

import Prelude

import Data.Const (Const)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Query = Const Unit

type Slot = H.Slot Query Void

type State =
    { disabled :: Boolean
    , text :: String }

type Message = Void

type Action = Void

label :: forall m. H.Component HH.HTML Query State Message m
label =
  H.mkComponent
    { initialState: initialState
    , render: render
    , eval: H.mkEval H.defaultEval }
  where
    initialState :: State -> State
    initialState state = state

    render :: State -> H.ComponentHTML Action () m
    render state =
      HH.a props [ HH.text state.text ]
      where
        props =
          case state.disabled of
            true -> [ HP.class_ $ HH.ClassName "label label--disabled" ]
            false -> [ HP.class_ $ HH.ClassName "label" ]
