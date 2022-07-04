module Main where

import Prelude
import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Int (fromString)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = { x :: Int }

data Action =
  Increment
  | Decrement
  | Reset

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component :: forall query input output m. H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ = { x: 0 }

showState :: State -> String
showState { x: value } = show value

render state =
  HH.div_
    [
      HH.button [ HE.onClick \_ -> Decrement ] [ HH.text "-" ]
      , HH.div [] [ HH.text (showState state) ]
      , HH.button [ HE.onClick \_ -> Increment ] [ HH.text "+" ]
      , HH.button [ HE.onClick \_ -> Reset ] [ HH.text "Reset" ]
    ]

handleAction = case _ of
  Decrement ->
    H.modify_ \state -> state { x = state.x - 1 }
  Increment ->
    H.modify_ \state -> state { x = state.x + 1 }
  Reset ->
    H.modify_ \state -> state { x = 0 }

