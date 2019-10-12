module FluentDesign.Components.Button where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data Query a = IsDisabled (Boolean -> a)

type StandardState =
  { disabled :: Boolean
  , text :: String }

data Message = Clicked Boolean

data Action = Click

type Slot = H.Slot Query Message

standard :: forall m. H.Component HH.HTML Query StandardState Message m
standard =
  H.mkComponent
    { initialState: initialState
    , render: render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }}
  where
    initialState state = state

    render state =
      HH.button props [ HH.text state.text ]
      where
        props =
          case state.disabled of
            true -> [ HP.title state.text,  HP.class_ $ HH.ClassName "button button--disabled" ]
            false -> [HP.title state.text,  HP.class_ $ HH.ClassName "button"]
    
    handleAction :: Action -> H.HalogenM StandardState Action () Message m Unit
    handleAction action =
      case action of
        Click -> do
          disabled <- H.gets _.disabled
          H.raise (Clicked disabled)

    handleQuery :: forall a. Query a -> H.HalogenM StandardState Action () Message m (Maybe a)
    handleQuery = case _ of
      IsDisabled k -> do
        disabled <- H.gets _.disabled
        pure (Just (k disabled))