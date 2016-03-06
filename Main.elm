import Html exposing (Html)
import Keyboard
import Random
import Signal
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

setUpNewCard : State -> State
setUpNewCard state =
  let (newState, card) = generateRandomCard state
  in { newState | currentCard = card }

handleClock : Time -> State -> State
handleClock t state =
  if state.initialized
    then
      case state.lockTime of
        Nothing -> state
        Just lockTime ->
          if lockTime - clockSpeed < 0
            then { state | lockTime = Nothing, currentCard = blankCard state.currentCard }
            else { state | lockTime = Just <| lockTime - clockSpeed }
    else
      let tempState = { state | seed = Random.initialSeed <| round t, initialized = True }
      in setUpNewCard tempState

handleKeypress : Char -> State -> State
handleKeypress c state =
  case state.lockTime of
    Just _ -> state
    Nothing ->
      let newCard = updateCard c state.currentCard
      in case cardState newCard of
          Tutor.Card.Complete   -> setUpNewCard state
          Tutor.Card.Incomplete -> { state | currentCard = newCard }
          Tutor.Card.Incorrect  -> { state | currentCard = newCard, lockTime = Just incorrectLockTime }

view : State -> Html
view {currentCard} =
  showCard currentCard

update : Event -> State -> State
update event state =
  case event of
    Clock t    -> handleClock t state
    Keypress c -> handleKeypress c state

main : Signal Html
main =
  let clock = Signal.map Clock <| Time.every clockSpeed
      inputChars = Signal.map (Keypress << qwerty2jcuken) Keyboard.presses
      combined = Signal.mergeMany [clock, inputChars]

      initialState = { currentCard = Tutor.Card.Card "" "", seed = Random.initialSeed 0, initialized = False, lockTime = Nothing }

  in Signal.map view <| Signal.foldp update initialState combined

