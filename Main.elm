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

type alias State = {
  lockTime : Maybe Int,
  currentCard : Card
}

generateRandomCard : State -> (State, Cmd Event)
generateRandomCard state =
  (state, Random.generate NewCard <| randomNgram ngrams)

decrementLockTime : Int -> State -> State
decrementLockTime lockTime state = { state | lockTime = Just <| lockTime - clockSpeed }

clearLock : State -> State
clearLock state = { state | lockTime = Nothing }

resetCard : State -> State
resetCard state = { state | currentCard = blankCard state.currentCard }

lockExpired : Int -> Bool
lockExpired lockTime = lockTime - clockSpeed < 0

setUpNewCard : State -> String -> State
setUpNewCard state ngrams =
  { state | currentCard = makeCard ngrams }

lockUI : State -> State
lockUI state = { state | lockTime = Just incorrectLockTime }

handleClock : Time -> State -> State
handleClock t state =
  case state.lockTime of
    Nothing -> state
    Just lockTime ->
      if lockTime - clockSpeed < 0
        then clearLock <| resetCard state
        else decrementLockTime lockTime state

handleKeypress : Char -> State -> (State, Cmd Event)
handleKeypress c state =
  case state.lockTime of
    Just _ -> (state, Cmd.none)
    Nothing ->
      let newCard = updateCard c state.currentCard
      in case cardState newCard of
          Tutor.Card.Complete   -> generateRandomCard state
          Tutor.Card.Incomplete -> ({ state | currentCard = newCard }, Cmd.none)
          Tutor.Card.Incorrect  -> (lockUI <| { state | currentCard = newCard }, Cmd.none)

view : State -> Html Event
view {currentCard} =
  showCard currentCard

update : Event -> State -> (State, Cmd Event)
update event state =
  case event of
    Clock t    -> (handleClock t state, Cmd.none)
    Keypress c -> handleKeypress c state
    NewCard ngrams -> (setUpNewCard state ngrams, Cmd.none)

init : (State, Cmd Event)
init =
  let initialState = { currentCard = Tutor.Card.Card "" "", lockTime = Nothing }
  in generateRandomCard initialState

subscriptions : State -> Sub Event
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
