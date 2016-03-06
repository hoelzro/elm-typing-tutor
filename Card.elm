import Char
import Dict
import Graphics.Element exposing (Element, show)
import Html exposing (Html, div, hr, text)
import Html.Attributes exposing (style)
import Keyboard
import Random
import Signal
import String
import Time exposing (Time)

import RussianNGrams exposing (ngrams)

pairListToDict : List (comparable, v) -> Dict.Dict comparable v
pairListToDict list =
  case list of
    []               -> Dict.empty
    ((k, v) :: tail) -> Dict.insert k v <| pairListToDict tail

dictToFunction : v -> Dict.Dict comparable v -> comparable -> v
dictToFunction bottom mapping = \key ->
  case Dict.get key mapping of
    Just value -> value
    Nothing    -> bottom

defineKeyMap : List (Char, Char) -> Char.KeyCode -> Char
defineKeyMap mapping =
  let (leftKeys, rightKeys) = List.unzip mapping
      keyCodes              = List.map Char.toCode leftKeys
      codesToRightKeys      = List.map2 (\code right -> (code, right)) keyCodes rightKeys
  in dictToFunction ' ' <| pairListToDict codesToRightKeys

-- https://en.wikipedia.org/wiki/QWERTY
-- https://en.wikipedia.org/wiki/JCUKEN
qwerty2jcuken = defineKeyMap [
  ('q', 'й'), ('w', 'ц'), ('e', 'у'), ('r', 'к'), ('t', 'е'), ('y', 'н'), ('u', 'г'), ('i', 'ш'), ('o', 'щ'), ('p', 'з'),
  ('[', 'х'), (']', 'ъ'), ('a', 'ф'), ('s', 'ы'), ('d', 'в'), ('f', 'а'), ('g', 'п'), ('h', 'р'), ('j', 'о'), ('k', 'л'), ('l', 'д'), (';', 'ж'), ('\'', 'э'),
  ('z', 'я'), ('x', 'ч'), ('c', 'с'), ('v', 'м'), ('b', 'и'), ('n', 'т'), ('m', 'ь'), (',', 'б'), ('.', 'ю'), ('/', '.'),

  ('Q', 'Й'), ('W', 'Ц'), ('E', 'У'), ('R', 'К'), ('T', 'Е'), ('Y', 'Н'), ('U', 'Г'), ('I', 'Ш'), ('O', 'Щ'), ('P', 'З'), ('{', 'Х'), ('}', 'Ъ'),
  ('A', 'Ф'), ('S', 'Ы'), ('D', 'В'), ('F', 'А'), ('G', 'П'), ('H', 'Р'), ('J', 'О'), ('K', 'Л'), ('L', 'Д'), (':', 'Ж'), ('"', 'Э'),
  ('Z', 'Я'), ('X', 'Ч'), ('C', 'С'), ('V', 'М'), ('B', 'И'), ('N', 'Т'), ('M', 'Ь'), ('<', 'Б'), ('>', 'Ю'), ('?', ',') ]

incorrectLockTime = 1000
clockSpeed = 100

type Card = Card String String

showCard : Card -> Html
showCard (Card target typed) =
  let targetDisplay = text target
      ruleDisplay = hr [] []
      typedDisplay = text typed
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

{-
handleCard : Signal Char -> Card -> Signal Html
handleCard keyPresses card =
  Signal.map showCard <| Signal.foldp updateCard card keyPresses

main : Signal Html
main = Signal.map showCard <| Signal.foldp updateCard (Card "вать" "") <| Signal.map qwerty2jcuken Keyboard.presses
-}

type Event = Clock Time
  | Keypress Char

type alias State = {
  lockTime : Maybe Int,
  initialized : Bool,
  currentCard : Card,
  seed : Random.Seed
}

type CardState = Complete | Incomplete | Incorrect

cardState : Card -> CardState
cardState (Card target typed) =
  if target == typed
    then Complete
    else if String.startsWith typed target
      then Incomplete
      else Incorrect

makeCard : String -> Card
makeCard s = Card s ""

blankCard : Card -> Card
blankCard (Card target _ ) = Card target ""

-- XXX unsafeFromJust
fromJust : Maybe a -> a
fromJust maybeValue =
  case maybeValue of
    Just value -> value
    Nothing    -> Debug.crash "Got Nothing when Just was expected"

last : List a -> a
last = fromJust << List.head << List.reverse

selectValue : List (Float, a) -> Float -> a
selectValue pairs selector =
  let lowerThanSelector = List.filter (\(p, _) -> p <= selector) pairs
  in snd <| last lowerThanSelector

weightedChoice : List (Float, a) -> Random.Generator a
weightedChoice possibilities =
  let (proportions, values) = List.unzip possibilities
      total = List.sum proportions
      cumulativeProportions = List.scanl (+) 0 proportions
      cumulativePossibilties = List.map2 (\a b -> (a, b)) cumulativeProportions values
      generateProportionValue = Random.float 0.0 (total - 1)
  in Random.map (selectValue cumulativePossibilties) generateProportionValue

unsafeIndex : List a -> Int -> a
unsafeIndex values index =
  case (index, values) of
    (_, [])           -> Debug.crash "index too large"
    (0, (value :: _)) -> value
    (_, (_ :: tail))  -> unsafeIndex tail <| index - 1

randomMember : List a -> Random.Generator a
randomMember values =
  let numValues = List.length values
      indexGenerator = Random.int 0 (numValues - 1)
  in Random.map (unsafeIndex values) indexGenerator

randomNgram : List (Int, List (Float, String)) -> Random.Generator String
randomNgram ngrams =
  let randomNgramSet = randomMember <| List.map snd ngrams
  in randomNgramSet `Random.andThen` weightedChoice

generateRandomCard : State -> (State, Card)
generateRandomCard state =
  let (ngram, seed1) = Random.generate (randomNgram ngrams) state.seed
  in ({state | seed = seed1}, makeCard ngram)

view : State -> Element
view state =
  show state

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
      let (Card target typed) = state.currentCard
          newCard = Card target (typed ++ (String.fromChar c))
      in case cardState newCard of
          Complete   -> setUpNewCard state
          Incomplete -> { state | currentCard = newCard }
          Incorrect  -> { state | currentCard = newCard, lockTime = Just incorrectLockTime }

update : Event -> State -> State
update event state =
  case event of
    Clock t    -> handleClock t state
    Keypress c -> handleKeypress c state

main : Signal Element
main =
  let clock = Signal.map Clock <| Time.every clockSpeed
      inputChars = Signal.map (Keypress << qwerty2jcuken) Keyboard.presses
      combined = Signal.mergeMany [clock, inputChars]

      initialState = { currentCard = Card "" "", seed = Random.initialSeed 0, initialized = False, lockTime = Nothing }

  in Signal.map view <| Signal.foldp update initialState combined

