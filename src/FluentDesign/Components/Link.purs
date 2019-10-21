module FluentDesign.Components.Link where

import Prelude

import Data.Const (Const)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

type Slot = H.Slot (Const Unit) Void

type State =
    { disabled :: Boolean
    , href :: String
    , text :: String }

link :: forall m. H.Component HH.HTML (Const Unit) State Void m
link =
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
            true -> [ HP.class_ $ HH.ClassName "link link--disabled" ]
            false -> [ HP.class_ $ HH.ClassName "link", HP.href state.href ]
