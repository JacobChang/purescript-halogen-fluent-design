module FluentDesign.Components.Button where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

data Query a = IsDisabled (Boolean -> a)

type State =
  { disabled :: Boolean
  , text :: String
  , secondaryText :: Maybe String }

defaultState :: State
defaultState =
  { disabled: false
  , text: ""
  , secondaryText: Nothing }

data Message = Clicked

data Action = Click

type Slot = H.Slot Query Message

standard :: forall m. MonadEffect m => H.Component HH.HTML Query State Message m
standard =
  H.mkComponent
    { initialState: initialState
    , render: render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }}
  where
    render :: State -> H.ComponentHTML Action () m
    render state =
      HH.button props children
      where
        props =
          case state.disabled of
            true -> [ HP.title state.text,  HP.class_ $ HH.ClassName "button button--disabled" ]
            false -> [ HP.title state.text,  HP.class_ $ HH.ClassName "button", HE.onClick \_ -> Just Click ]
        children =
          case state.secondaryText of
            Just secondaryText ->
              [ HH.span [] [ HH.text state.text], HH.span [] [ HH.text secondaryText ] ]
            Nothing ->
              [ HH.span [] [ HH.text state.text] ]

primary :: forall m. MonadEffect m => H.Component HH.HTML Query State Message m
primary =
  H.mkComponent
    { initialState: initialState
    , render: render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }}
  where
    render :: State -> H.ComponentHTML Action () m
    render state =
      HH.button props children
      where
        props =
          case state.disabled of
            true -> [ HP.title state.text,  HP.class_ $ HH.ClassName "button button--primary button--disabled" ]
            false -> [HP.title state.text,  HP.class_ $ HH.ClassName "button button--primary", HE.onClick \_ -> Just Click ]
        children =
          case state.secondaryText of
            Just secondaryText ->
              [ HH.span [] [ HH.text state.text], HH.span [] [ HH.text secondaryText ] ]
            Nothing ->
              [ HH.span [] [ HH.text state.text] ]

initialState :: State -> State
initialState state = state

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM State Action () Message m Unit
handleAction action =
  case action of
    Click -> do
      disabled <- H.gets _.disabled
      H.raise Clicked

handleQuery :: forall a m. Query a -> H.HalogenM State Action () Message m (Maybe a)
handleQuery = case _ of
  IsDisabled k -> do
    disabled <- H.gets _.disabled
    pure (Just (k disabled))