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

type Event = Clock Time
  | Keypress Char
  | NewCard String

type alias Model = {
  lockTime : Maybe Int,
  currentCard : Card
}

generateRandomCard : Model -> (Model, Cmd Event)
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

handleKeypress : Char -> Model -> (Model, Cmd Event)
handleKeypress c model =
  case model.lockTime of
    Just _ -> (model, Cmd.none)
    Nothing ->
      let newCard = updateCard c model.currentCard
      in case cardState newCard of
          Tutor.Card.Complete   -> generateRandomCard model
          Tutor.Card.Incomplete -> ({ model | currentCard = newCard }, Cmd.none)
          Tutor.Card.Incorrect  -> (lockUI <| { model | currentCard = newCard }, Cmd.none)

view : Model -> Html Event
view {currentCard} =
  showCard currentCard

update : Event -> Model -> (Model, Cmd Event)
update event model =
  case event of
    Clock t    -> (handleClock t model, Cmd.none)
    Keypress c -> handleKeypress c model
    NewCard ngrams -> (setUpNewCard model ngrams, Cmd.none)

init : (Model, Cmd Event)
init =
  let initialState = { currentCard = Tutor.Card.Card "" "", lockTime = Nothing }
  in generateRandomCard initialState

subscriptions : Model -> Sub Event
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
