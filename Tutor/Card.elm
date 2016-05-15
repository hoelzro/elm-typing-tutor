module Tutor.Card exposing (..)

import Html exposing (Html, div, hr, span, text)
import Html.Attributes exposing (style)
import String

type Card = Card String String

isPrefixOf = flip String.startsWith

renderTyped : String -> String -> Html
renderTyped expected got =
  if expected `isPrefixOf` got
    then text got
    else span [] [ (text <| String.dropRight 1 got),
      span [style [("color", "red")]] [ text <| String.right 1 got ] ]

showCard : Card -> Html
showCard (Card target typed) =
  let targetDisplay = text target
      ruleDisplay = hr [] []
      typedDisplay = renderTyped target typed
      styleAttr =
    style [("width", "400px"),
           ("height", "400px"),
           ("border", "thin solid lightgray"),
           ("box-shadow", "5px 5px lightgray"),
           ("position", "absolute"),
           ("top", "100px"),
           ("left", "100px")]
  in div [styleAttr] [ targetDisplay, ruleDisplay, typedDisplay ]

updateCard : Char -> Card -> Card
updateCard character (Card target typed) =
  let newTyped = typed ++ (String.fromChar character)
  in Card target newTyped

type CardState = Complete | Incomplete | Incorrect

cardState : Card -> CardState
cardState (Card target typed) =
  if target == typed
    then Complete
    else if target `isPrefixOf` typed
      then Incomplete
      else Incorrect

makeCard : String -> Card
makeCard s = Card s ""

blankCard : Card -> Card
blankCard (Card target _ ) = Card target ""
