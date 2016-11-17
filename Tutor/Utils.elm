module Tutor.Utils exposing (..)

import Dict
import Random

dictToFunction : v -> Dict.Dict comparable v -> comparable -> v
dictToFunction bottom mapping = \key ->
  case Dict.get key mapping of
    Just value -> value
    Nothing    -> bottom

unsafeFromJust : Maybe a -> a
unsafeFromJust maybeValue =
  case maybeValue of
    Just value -> value
    Nothing    -> Debug.crash "Got Nothing when Just was expected"

last : List a -> a
last = unsafeFromJust << List.head << List.reverse

selectValue : List (Float, a) -> Float -> a
selectValue pairs selector =
  let lowerThanSelector = List.filter (\(p, _) -> p <= selector) pairs
  in Tuple.second <| last lowerThanSelector

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
  let randomNgramSet = randomMember <| List.map Tuple.second ngrams
  in Random.andThen weightedChoice randomNgramSet
