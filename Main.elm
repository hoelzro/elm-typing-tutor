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

type alias State = {
  lockTime : Maybe Int,
  initialized : Bool,
  currentCard : Card,
  seed : Random.Seed
}

generateRandomCard : State -> (State, Card)
generateRandomCard state =
  let (ngram, seed1) = Random.generate (randomNgram ngrams) state.seed
  in ({state | seed = seed1}, makeCard ngram)

initializeRNG : Time -> State -> State
initializeRNG t state =  { state | seed = Random.initialSeed <| round t, initialized = True }

decrementLockTime : Int -> State -> State
decrementLockTime lockTime state = { state | lockTime = Just <| lockTime - clockSpeed }

clearLock : State -> State
clearLock state = { state | lockTime = Nothing }

resetCard : State -> State
resetCard state = { state | currentCard = blankCard state.currentCard }

lockExpired : Int -> Bool
lockExpired lockTime = lockTime - clockSpeed < 0

setUpNewCard : State -> State
setUpNewCard state =
  let (newState, card) = generateRandomCard state
  in { newState | currentCard = card }

lockUI : State -> State
lockUI state = { state | lockTime = Just incorrectLockTime }

handleClock : Time -> State -> State
handleClock t state =
  if state.initialized
    then
      case state.lockTime of
        Nothing -> state
        Just lockTime ->
          if lockTime - clockSpeed < 0
            then clearLock <| resetCard state
            else decrementLockTime lockTime state
    else
      setUpNewCard <| initializeRNG t state

handleKeypress : Char -> State -> State
handleKeypress c state =
  case state.lockTime of
    Just _ -> state
    Nothing ->
      let newCard = updateCard c state.currentCard
      in case cardState newCard of
          Tutor.Card.Complete   -> setUpNewCard state
          Tutor.Card.Incomplete -> { state | currentCard = newCard }
          Tutor.Card.Incorrect  -> lockUI <| { state | currentCard = newCard }

view : State -> Html Event
view {currentCard} =
  showCard currentCard

update : Event -> State -> State
update event state =
  case event of
    Clock t    -> handleClock t state
    Keypress c -> handleKeypress c state

init : (State, Cmd Event)
init =
  let initialState = { currentCard = Tutor.Card.Card "" "", seed = Random.initialSeed 0, initialized = False, lockTime = Nothing }
  in (initialState, Cmd.none)

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
