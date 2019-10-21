module FluentDesign.Components.Checkbox where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP

data Query a = IsChecked (Boolean -> a)

type Slot = H.Slot Query Message

data Action = Toggle

data Message = Toggled Boolean

type State =
    { disabled :: Boolean
    , checked :: Boolean
    , text :: String }

checkbox :: forall m. H.Component HH.HTML Query State Message m
checkbox =
  H.mkComponent
    { initialState: initialState
    , render: render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction} }
  where
    initialState :: State -> State
    initialState state = state

    render :: State -> HH.ComponentHTML Action () m
    render state =
      HH.div props [ child ]
      where
        props =
          case state.disabled of
            true ->
              [ HP.class_ $ HH.ClassName "checkbox checkbox--disabled" ]
            false ->
              [ HP.class_ $ HH.ClassName "checkbox" ]
        input =
          HH.input
            [ HP.type_ InputCheckbox
            , HP.checked state.checked
            , HP.disabled state.disabled
            , HE.onInput \_ -> Just Toggle ]
        child =
          HH.label []
            [ input
            , HH.text state.text ]

    handleAction :: Action -> H.HalogenM State Action () Message m Unit
    handleAction action =
      case action of
        Toggle -> do
          newState <- H.modify (\st -> st { checked = not st.checked })
          H.raise (Toggled newState.checked)

    handleQuery :: forall a. Query a -> H.HalogenM State Action () Message m (Maybe a)
    handleQuery = case _ of
      IsChecked k -> do
        checked <- H.gets _.checked
        pure (Just (k checked))