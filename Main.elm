import Html exposing (Html)
import Html.App as App
import Keyboard
import Random
import Time exposing (Time)

import Tutor.Card exposing (Card, blankCard, cardState, makeCard, showCard, updateCard)
import Tutor.Keymaps exposing (qwerty2jcuken)
import Tutor.Utils exposing (randomNgram)
import Tutor.RussianNGrams exposing (ngrams)

incorrectLockTime = 1000
clockSpeed = 100

type Message = Clock Time
  | Keypress Char
  | NewCard String

type alias Model = {
  lockTime : Maybe Int,
  currentCard : Card
}

noCmd : a -> (a, Cmd b)
noCmd arg = (arg, Cmd.none)

generateRandomCard : Model -> (Model, Cmd Message)
generateRandomCard model =
  (model, Random.generate NewCard <| randomNgram ngrams)

decrementLockTime : Int -> Model -> Model
decrementLockTime lockTime model = { model | lockTime = Just <| lockTime - clockSpeed }

clearLock : Model -> Model
clearLock model = { model | lockTime = Nothing }

resetCard : Model -> Model
resetCard model = { model | currentCard = blankCard model.currentCard }

lockExpired : Int -> Bool
lockExpired lockTime = lockTime - clockSpeed < 0

setUpNewCard : Model -> String -> Model
setUpNewCard model ngrams =
  { model | currentCard = makeCard ngrams }

lockUI : Model -> Model
lockUI model = { model | lockTime = Just incorrectLockTime }

handleClock : Time -> Model -> Model
handleClock t model =
  case model.lockTime of
    Nothing -> model
    Just lockTime ->
      if lockTime - clockSpeed < 0
        then clearLock <| resetCard model
        else decrementLockTime lockTime model

handleKeypress : Char -> Model -> (Model, Cmd Message)
handleKeypress c model =
  case model.lockTime of
    Just _ -> noCmd model
    Nothing ->
      let newCard = updateCard c model.currentCard
      in case cardState newCard of
          Tutor.Card.Complete   -> generateRandomCard model
          Tutor.Card.Incomplete -> noCmd { model | currentCard = newCard }
          Tutor.Card.Incorrect  -> noCmd <| lockUI <| { model | currentCard = newCard }

view : Model -> Html Message
view {currentCard} =
  showCard currentCard

update : Message -> Model -> (Model, Cmd Message)
update message model =
  case message of
    Clock t    -> noCmd <| handleClock t model
    Keypress c -> handleKeypress c model
    NewCard ngrams -> noCmd <| setUpNewCard model ngrams

init : (Model, Cmd Message)
init =
  let initialState = { currentCard = Tutor.Card.Card "" "", lockTime = Nothing }
  in generateRandomCard initialState

subscriptions : Model -> Sub Message
subscriptions _ =
  let clock = Time.every clockSpeed Clock
      inputChars = Keyboard.presses (Keypress << qwerty2jcuken)
  in Sub.batch [ clock, inputChars ]

main : Program Never
main = App.program {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
  }
