module FluentDesign.Components.Button where

import Prelude

import Data.Const (Const)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Slot = H.Slot (Const Unit) Void

type StandardState =
    { disabled :: Boolean
    , text :: String }

standard :: forall m. H.Component HH.HTML (Const Unit) StandardState Void m
standard =
  H.mkComponent
    { initialState: initialState
    , render: render
    , eval: H.mkEval H.defaultEval }
  where
    initialState state = state

    render state =
      HH.button props [ HH.text state.text ]
      where
        props =
          case state.disabled of
            true -> [ HP.title state.text,  HP.class_ $ HH.ClassName "button button--disabled" ]
            false -> [HP.title state.text,  HP.class_ $ HH.ClassName "button"]
