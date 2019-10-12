module FluentDesign.Components.Label where

import Prelude

import Data.Const (Const)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Slot = H.Slot (Const Unit) Void

type State =
    { disabled :: Boolean
    , text :: String }

label :: forall m. H.Component HH.HTML (Const Unit) State Void m
label =
  H.mkComponent
    { initialState: initialState
    , render: render
    , eval: H.mkEval H.defaultEval }
  where
    initialState state = state

    render state =
      HH.a props [ HH.text state.text ]
      where
        props =
          case state.disabled of
            true -> [ HP.class_ $ HH.ClassName "label label--disabled" ]
            false -> [ HP.class_ $ HH.ClassName "label" ]
