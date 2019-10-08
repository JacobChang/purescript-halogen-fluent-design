module FluentDesign.Components.Link where

import Prelude

import Data.Const (Const)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

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
            true -> [ HP.class_ $ HH.ClassName "fd--link fd-link--disabled" ]
            false -> [ HP.class_ $ HH.ClassName "fd-link",  HP.href state.href ]
