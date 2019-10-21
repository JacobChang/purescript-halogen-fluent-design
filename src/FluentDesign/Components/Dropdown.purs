module FluentDesign.Components.Dropdown where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type Choice a =
  { key :: String
  , label :: String
  , value :: a }

data Query a q = SelectedChoice (Choice a -> q)

type Slot a = H.Slot (Query a) (Message a)

data Action a = Select (Choice a)

data Message a = Selected (Choice a)

type State a =
  { disabled :: Boolean
  , selected :: Choice a
  , choices :: Array (Choice a) }

findChoice :: forall a.String -> Array (Choice a) -> Maybe (Choice a)
findChoice key choices =
  Array.find (\choice -> choice.key == key) choices

handleInput :: forall a. Array (Choice a) -> String -> Maybe (Action a)
handleInput choices key =
  map Select (findChoice key choices)

choiceInput :: forall m a. Choice a -> Choice a -> HH.ComponentHTML (Action a) () m
choiceInput selected choice =
  HH.option
    [ HP.value choice.key
    , HP.class_ $ HH.ClassName "dropdown__choice"
    , HP.selected (selected.key == choice.key) ]
    [ HH.text choice.label ]

dropdown :: forall m a. H.Component HH.HTML (Query a) (State a) (Message a) m
dropdown =
  H.mkComponent
    { initialState: initialState
    , render: render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction} }
  where
    initialState :: State a -> State a
    initialState state = state

    render :: State a -> HH.ComponentHTML (Action a) () m
    render state =
      HH.select props choices
      where
        props =
          case state.disabled of
            true ->
              [ HP.class_ $ HH.ClassName "dropdown dropdown--disabled" ]
            false ->
              [ HP.class_ $ HH.ClassName "dropdown"
              , HE.onValueChange (handleInput state.choices) ]
        choices =
          map (choiceInput state.selected) state.choices

    handleAction :: (Action a) -> H.HalogenM (State a) (Action a) () (Message a) m Unit
    handleAction action =
      case action of
        Select choice -> do
          newState <- H.modify (\st -> st { selected = choice })
          H.raise (Selected newState.selected)


    handleQuery :: forall q. Query a q -> H.HalogenM (State a) (Action a) () (Message a) m (Maybe q)
    handleQuery = case _ of
      SelectedChoice k -> do
        selected <- H.gets _.selected
        pure (Just (k selected))