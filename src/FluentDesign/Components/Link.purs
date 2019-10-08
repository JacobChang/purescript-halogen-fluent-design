module FluentDesign.Components.Link where

import Prelude

import Data.Const (Const)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.HTML as HH

type State =
    { href :: String
    , text :: String }

link :: H.Component HH.HTML (Const Unit) State Void Aff
link =
  H.mkComponent
    { initialState: initialState
    , render: \_ -> renderHtml
    , eval: H.mkEval H.defaultEval }
  where
    initialState state = const state
    renderHtml = HH.span [] []
