module FluentDesign.Components.ChoiceGroup where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Choice a =
  { key :: String
  , label :: String
  , value :: a
  , selected :: Boolean }

data Query a q = Choices (Array (Choice a) -> q)

type Slot a = H.Slot (Query a) (Message a)

data Action a = Toggle (Choice a)

data Message a = Toggled (Choice a)

type State a =
  { disabled :: Boolean
  , choices :: Array (Choice a) }

toggleChoice :: forall a. Choice a -> Array (Choice a) -> Array (Choice a)
toggleChoice choice choices =
  map toggle choices
  where
    toggle c =
      case c.key == choice.key of
        true -> c { selected = not c.selected }
        false -> c

choiceSpan :: forall m a. Choice a -> HH.ComponentHTML (Action a) () m
choiceSpan choice =
  HH.span
    [ HP.class_ classes
    , HE.onClick \evt -> Just $ Toggle choice]
    [ HH.text choice.label ]
  where
    classes =
      case choice.selected of
        true ->
          HH.ClassName "choicegroup__choice choicegroup__choice--selected"
        false ->
          HH.ClassName "choicegroup__choice"

choiceGroup :: forall m a. H.Component HH.HTML (Query a) (State a) (Message a) m
choiceGroup =
  H.mkComponent
    { initialState: initialState
    , render: render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction} }
  where
    initialState :: State a -> State a
    initialState state = state

    render :: State a -> HH.ComponentHTML (Action a) () m
    render state =
      HH.div props choices
      where
        props =
          case state.disabled of
            true ->
              [ HP.class_ $ HH.ClassName "choicegroup choicegroup--disabled" ]
            false ->
              [ HP.class_ $ HH.ClassName "choicegroup" ]
        choices =
          map choiceSpan state.choices

    handleAction :: (Action a) -> H.HalogenM (State a) (Action a) () (Message a) m Unit
    handleAction action =
      case action of
        Toggle choice -> do
          newState <- H.modify (\st -> st { choices = toggleChoice choice st.choices })
          H.raise (Toggled choice)

    handleQuery :: forall q. Query a q -> H.HalogenM (State a) (Action a) () (Message a) m (Maybe q)
    handleQuery = case _ of
      Choices k -> do
        choices <- H.gets _.choices
        pure (Just (k choices))