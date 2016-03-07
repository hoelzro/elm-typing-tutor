module Tutor.Keymaps where

import Char
import Dict

import Tutor.Utils exposing (dictToFunction)

defineKeyMap : List (Char, Char) -> Char.KeyCode -> Char
defineKeyMap mapping =
  let (leftKeys, rightKeys) = List.unzip mapping
      keyCodes              = List.map Char.toCode leftKeys
      codesToRightKeys      = List.map2 (\code right -> (code, right)) keyCodes rightKeys
  in dictToFunction ' ' <| Dict.fromList codesToRightKeys

-- https://en.wikipedia.org/wiki/QWERTY
-- https://en.wikipedia.org/wiki/JCUKEN
qwerty2jcuken = defineKeyMap [
  ('q', 'й'), ('w', 'ц'), ('e', 'у'), ('r', 'к'), ('t', 'е'), ('y', 'н'), ('u', 'г'), ('i', 'ш'), ('o', 'щ'), ('p', 'з'),
  ('[', 'х'), (']', 'ъ'), ('a', 'ф'), ('s', 'ы'), ('d', 'в'), ('f', 'а'), ('g', 'п'), ('h', 'р'), ('j', 'о'), ('k', 'л'), ('l', 'д'), (';', 'ж'), ('\'', 'э'),
  ('z', 'я'), ('x', 'ч'), ('c', 'с'), ('v', 'м'), ('b', 'и'), ('n', 'т'), ('m', 'ь'), (',', 'б'), ('.', 'ю'), ('/', '.'),

  ('Q', 'Й'), ('W', 'Ц'), ('E', 'У'), ('R', 'К'), ('T', 'Е'), ('Y', 'Н'), ('U', 'Г'), ('I', 'Ш'), ('O', 'Щ'), ('P', 'З'), ('{', 'Х'), ('}', 'Ъ'),
  ('A', 'Ф'), ('S', 'Ы'), ('D', 'В'), ('F', 'А'), ('G', 'П'), ('H', 'Р'), ('J', 'О'), ('K', 'Л'), ('L', 'Д'), (':', 'Ж'), ('"', 'Э'),
  ('Z', 'Я'), ('X', 'Ч'), ('C', 'С'), ('V', 'М'), ('B', 'И'), ('N', 'Т'), ('M', 'Ь'), ('<', 'Б'), ('>', 'Ю'), ('?', ',') ]
