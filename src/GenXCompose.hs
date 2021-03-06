{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module GenXCompose where

import Control.Lens
import Control.Monad
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Numeric

data Symbol = Symbol
  { _symbol_character :: Char
  , _symbol_codepoint :: Int
  , _symbol_comment :: String
  }

symbolToEntry :: Symbol -> String
symbolToEntry s = mconcat
  [ "\"", pure (_symbol_character s), "\""
  , " U", pad (showHex (_symbol_codepoint s) "")
  , " # ", _symbol_comment s
  ]
 where
  pad c = replicate (4 - length c) '0' <> c

infixr 1 ?
(?) :: Char -> String -> Symbol
s ? comment = Symbol
  { _symbol_character = s
  , _symbol_codepoint = fromEnum s
  , _symbol_comment = comment
  }

infixr 0 ##

(##) :: String -> Symbol -> Entries Symbol
k ## sym = Entries $ Map.singleton k sym

--TODO: Handle conflicts where one entry's input sequence is a prefix of another's
-- by using a prefix tree structure
newtype Entries s = Entries { unEntries :: Map String s }

stringToKeySequence :: (Char -> r) -> (String -> r) -> String ->  r
stringToKeySequence kerr ksucc' = go (ksucc' . ("<Multi_key>" <>))
 where
  go ksucc = \case
    [] -> ksucc ""
    c:cs -> case charToKeysym c of
      Nothing -> kerr c
      Just c' -> go (ksucc . ((" <" <> c' <> ">") <>)) cs

charToKeysym :: Char -> Maybe String
charToKeysym c = do
  guard $ isAscii c
  let code = fromEnum c
  case c of
    ' ' -> pure "space"
    '!' -> pure "exclam"
    '"' -> pure "quotedbl"
    '#' -> pure "numbersign"
    '$' -> pure "dollar"
    '%' -> pure "percent"
    '&' -> pure "ampersand"
    '\'' -> pure "apostrophe"
    '(' -> pure "parenleft"
    ')' -> pure "parenright"
    '*' -> pure "asterisk"
    '+' -> pure "plus"
    ',' -> pure "comma"
    '-' -> pure "minus"
    '.' -> pure "period"
    '/' -> pure "slash"
    '0' -> pure "0"
    '1' -> pure "1"
    '2' -> pure "2"
    '3' -> pure "3"
    '4' -> pure "4"
    '5' -> pure "5"
    '6' -> pure "6"
    '7' -> pure "7"
    '8' -> pure "8"
    '9' -> pure "9"
    ':' -> pure "colon"
    ';' -> pure "semicolon"
    '<' -> pure "less"
    '=' -> pure "equal"
    '>' -> pure "greater"
    '?' -> pure "question"
    '@' -> pure "at"
    '[' -> pure "bracketleft"
    '\\' -> pure "backslash"
    ']' -> pure "bracketright"
    '^' -> pure "asciicircum"
    '_' -> pure "underscore"
    '`' -> pure "grave"
    '{' -> pure "braceleft"
    '|' -> pure "bar"
    '}' -> pure "braceright"
    '~' -> pure "asciitilde"
    _ | (code >= 0x41 && code <= 0x5A)
        || (code >= 0x61 && code <= 0x7a) -> pure $ pure c
      | otherwise -> Nothing

deriving newtype instance Functor Entries
deriving newtype instance Foldable Entries
deriving stock instance Traversable Entries

instance Wrapped (Entries s) where
  type Unwrapped (Entries s) = Map String s
  _Wrapped' = iso (\(Entries x) -> x) Entries

instance Rewrapped (Entries s) (Entries s')

instance FunctorWithIndex String Entries where
  imap f = over _Wrapped (imap f)

instance FoldableWithIndex String Entries where
  ifoldMap f (Entries es) = ifoldMap f es
  ifoldr f x (Entries es) = ifoldr f x es
  ifoldl' f x (Entries es) = ifoldl' f x es

instance TraversableWithIndex String Entries where
  itraverse f (Entries es) = fmap Entries $ itraverse f es

instance Semigroup (Entries s) where
  (Entries e1) <> (Entries e2) = Entries $ Map.unionWithKey (\k -> error $ "Overlap at " ++ k) e1 e2

instance Monoid (Entries s) where
  mempty = Entries mempty

family :: String -> Entries s -> Entries s
family prefix = Entries . Map.mapKeysMonotonic (prefix <>) . unEntries

allEntries :: Entries Symbol
allEntries = mconcat
  [ -- Superscripts
    family "-" $ mconcat
    [ "1" ## '??' ? "superscript one"
    , "2" ## '??' ? "superscript two"
    , "3" ## '??' ? "superscript three"
    , "4" ## '???' ? "superscript four"
    , "5" ## '???' ? "superscript five"
    , "6" ## '???' ? "superscript six"
    , "7" ## '???' ? "superscript seven"
    , "8" ## '???' ? "superscript eight"
    , "9" ## '???' ? "superscript nine"
    , "0" ## '???' ? "superscript zero"
    , "a" ## '???' ? "modifier letter small a"
    , "b" ## '???' ? "modifier letter small b"
    , "c" ## '???' ? "modifier letter small c"
    , "d" ## '???' ? "modifier letter small d"
    , "e" ## '???' ? "modifier letter small e"
    , "f" ## '???' ? "modifier letter small f"
    , "g" ## '???' ? "modifier letter small g"
    , "h" ## '??' ? "modifier letter small h"
    , "i" ## '???' ? "modifier letter small i"
    , "j" ## '??' ? "modifier letter small j"
    , "k" ## '???' ? "modifier letter small k"
    , "l" ## '??' ? "modifier letter small l"
    , "m" ## '???' ? "modifier letter small m"
    , "n" ## '???' ? "modifier letter small n"
    , "o" ## '???' ? "modifier letter small o"
    , "p" ## '???' ? "modifier letter small p"
    , "r" ## '??' ? "modifier letter small r"
    , "s" ## '??' ? "modifier letter small s"
    , "t" ## '???' ? "modifier letter small t"
    , "u" ## '???' ? "modifier letter small u"
    , "v" ## '???' ? "modifier letter small v"
    , "w" ## '??' ? "modifier letter small w"
    , "x" ## '??' ? "modifier letter small x"
    , "y" ## '??' ? "modifier letter small y"
    , "z" ## '???' ? "modifier letter small z"
    ]
  , -- Subscripts
    family "_" $ mconcat
    [ "0" ## '???' ? "subscript zero"
    , "1" ## '???' ? "subscript one"
    , "2" ## '???' ? "subscript two"
    , "3" ## '???' ? "subscript three"
    , "4" ## '???' ? "subscript four"
    , "5" ## '???' ? "subscript five"
    , "6" ## '???' ? "subscript six"
    , "7" ## '???' ? "subscript seven"
    , "8" ## '???' ? "subscript eight"
    , "9" ## '???' ? "subscript nine"
    , "a" ## '???' ? "latin subscript small letter a"
    , "e" ## '???' ? "latin subscript small letter e"
    , "h" ## '???' ? "latin subscript small letter h"
    , "i" ## '???' ? "latin subscript small letter i"
    , "k" ## '???' ? "latin subscript small letter k"
    , "l" ## '???' ? "latin subscript small letter l"
    , "m" ## '???' ? "latin subscript small letter m"
    , "n" ## '???' ? "latin subscript small letter n"
    , "o" ## '???' ? "latin subscript small letter o"
    , "p" ## '???' ? "latin subscript small letter p"
    , "r" ## '???' ? "latin subscript small letter r"
    , "s" ## '???' ? "latin subscript small letter s"
    , "t" ## '???' ? "latin subscript small letter t"
    , "u" ## '???' ? "latin subscript small letter u"
    , "v" ## '???' ? "latin subscript small letter v"
    , "x" ## '???' ? "latin subscript small letter x"
    ]
  , --
    family ";" $ mconcat
    [ "A" ## '????' ? "mathematical script capital A"
    , "B" ## '???' ? "mathematical script capital B"
    , "C" ## '????' ? "mathematical script capital C"
    , "D" ## '????' ? "mathematical script capital D"
    , "E" ## '???' ? "mathematical script capital E"
    , "F" ## '???' ? "mathematical script capital F"
    , "G" ## '????' ? "mathematical script capital G"
    , "H" ## '???' ? "mathematical script capital H"
    , "I" ## '???' ? "mathematical script capital I"
    , "J" ## '????' ? "mathematical script capital J"
    , "K" ## '????' ? "mathematical script capital K"
    , "L" ## '???' ? "mathematical script capital L"
    , "M" ## '???' ? "mathematical script capital M"
    , "N" ## '????' ? "mathematical script capital N"
    , "O" ## '????' ? "mathematical script capital O"
    , "P" ## '????' ? "mathematical script capital P"
    , "Q" ## '????' ? "mathematical script capital Q"
    , "R" ## '???' ? "mathematical script capital R"
    , "S" ## '????' ? "mathematical script capital S"
    , "T" ## '????' ? "mathematical script capital T"
    , "U" ## '????' ? "mathematical script capital U"
    , "V" ## '????' ? "mathematical script capital V"
    , "W" ## '????' ? "mathematical script capital W"
    , "X" ## '????' ? "mathematical script capital X"
    , "Y" ## '????' ? "mathematical script capital Y"
    , "Z" ## '????' ? "mathematical script capital Z"
    , "a" ## '????' ? "mathematical script small a"
    , "b" ## '????' ? "mathematical script small b"
    , "c" ## '????' ? "mathematical script small c"
    , "d" ## '????' ? "mathematical script small d"
    , "e" ## '???' ? "mathematical script small e"
    , "f" ## '????' ? "mathematical script small f"
    , "g" ## '???' ? "mathematical script small g"
    , "h" ## '????' ? "mathematical script small h"
    , "i" ## '????' ? "mathematical script small i"
    , "j" ## '????' ? "mathematical script small j"
    , "k" ## '????' ? "mathematical script small k"
    , "l" ## '????' ? "mathematical script small l"
    , "m" ## '????' ? "mathematical script small m"
    , "n" ## '????' ? "mathematical script small n"
    , "o" ## '???' ? "mathematical script small o"
    , "p" ## '????' ? "mathematical script small p"
    , "q" ## '????' ? "mathematical script small q"
    , "r" ## '????' ? "mathematical script small r"
    , "s" ## '????' ? "mathematical script small s"
    , "t" ## '????' ? "mathematical script small t"
    , "u" ## '????' ? "mathematical script small u"
    , "v" ## '????' ? "mathematical script small v"
    , "w" ## '????' ? "mathematical script small w"
    , "x" ## '????' ? "mathematical script small x"
    , "y" ## '????' ? "mathematical script small y"
    , "z" ## '????' ? "mathematical script small z"
    ]
  , -- Blackboard
    family ":" $ mconcat
    [ "A" ## '????' ? "mathematical double-struck capital A"
    , "B" ## '????' ? "mathematical double-struck capital B"
    , "C" ## '???' ? "double-struck capital C"
    , "D" ## '????' ? "mathematical double-struck capital D"
    , "E" ## '????' ? "mathematical double-struck capital E"
    , "F" ## '????' ? "mathematical double-struck capital F"
    , "G" ## '????' ? "mathematical double-struck capital G"
    , "H" ## '???' ? "double-struck capital H"
    , "I" ## '????' ? "mathematical double-struck capital I"
    , "J" ## '????' ? "mathematical double-struck capital J"
    , "K" ## '????' ? "mathematical double-struck capital K"
    , "L" ## '????' ? "mathematical double-struck capital L"
    , "M" ## '????' ? "mathematical double-struck capital M"
    , "N" ## '???' ? "double-struck capital N"
    , "O" ## '????' ? "mathematical double-struck capital O"
    , "P" ## '???' ? "double-struck capital P"
    , "Q" ## '???' ? "double-struck capital Q"
    , "R" ## '???' ? "double-struck capital R"
    , "S" ## '????' ? "mathematical double-struck capital S"
    , "T" ## '????' ? "mathematical double-struck capital T"
    , "U" ## '????' ? "mathematical double-struck capital U"
    , "V" ## '????' ? "mathematical double-struck capital V"
    , "W" ## '????' ? "mathematical double-struck capital W"
    , "X" ## '????' ? "mathematical double-struck capital X"
    , "Y" ## '????' ? "mathematical double-struck capital Y"
    , "Z" ## '???' ? "double-struck capital Z"
    , "a" ## '????' ? "mathematical double-struck small a"
    , "b" ## '????' ? "mathematical double-struck small a"
    , "c" ## '????' ? "mathematical double-struck small a"
    , "d" ## '????' ? "mathematical double-struck small a"
    , "e" ## '????' ? "mathematical double-struck small a"
    , "f" ## '????' ? "mathematical double-struck small a"
    , "g" ## '????' ? "mathematical double-struck small a"
    , "h" ## '????' ? "mathematical double-struck small a"
    , "i" ## '????' ? "mathematical double-struck small a"
    , "j" ## '????' ? "mathematical double-struck small a"
    , "k" ## '????' ? "mathematical double-struck small a"
    , "l" ## '????' ? "mathematical double-struck small a"
    , "m" ## '????' ? "mathematical double-struck small a"
    , "n" ## '????' ? "mathematical double-struck small a"
    , "o" ## '????' ? "mathematical double-struck small a"
    , "p" ## '????' ? "mathematical double-struck small a"
    , "q" ## '????' ? "mathematical double-struck small a"
    , "r" ## '????' ? "mathematical double-struck small a"
    , "s" ## '????' ? "mathematical double-struck small a"
    , "t" ## '????' ? "mathematical double-struck small a"
    , "u" ## '????' ? "mathematical double-struck small a"
    , "v" ## '????' ? "mathematical double-struck small a"
    , "w" ## '????' ? "mathematical double-struck small a"
    , "x" ## '????' ? "mathematical double-struck small a"
    , "y" ## '????' ? "mathematical double-struck small a"
    , "z" ## '????' ? "mathematical double-struck small a"
    , "0" ## '????' ? "mathematical double-struck small zero"
    , "1" ## '????' ? "mathematical double-struck small one"
    , "2" ## '????' ? "mathematical double-struck small two"
    , "3" ## '????' ? "mathematical double-struck small three"
    , "4" ## '????' ? "mathematical double-struck small four"
    , "5" ## '????' ? "mathematical double-struck small five"
    , "6" ## '????' ? "mathematical double-struck small six"
    , "7" ## '????' ? "mathematical double-struck small seven"
    , "8" ## '????' ? "mathematical double-struck small eight"
    , "9" ## '????' ? "mathematical double-struck small nine"
    ]
  , -- Greek letters
    family "q" $ mconcat
    [ "G" ## '??' ? "greek capital letter gamma"
    , "D" ## '??' ? "greek capital letter gamma"
    , "Th" ## '??' ? "greek capital letter gamma"
    , "L" ## '??' ? "greek capital letter gamma"
    , "X" ## '??' ? "greek capital letter gamma"
    , "Pi" ## '??' ? "greek capital letter gamma"
    , "S" ## '??' ? "greek capital letter gamma"
    , "F" ## '??' ? "greek capital letter gamma"
    , ">" ## '??' ? "greek capital letter gamma"
    , "O" ## '??' ? "greek capital letter gamma"
    , "a" ## '??' ? "greek small letter alpha"
    , "b" ## '??' ? "greek small letter beta"
    , "g" ## '??' ? "greek small letter gamma"
    , "d" ## '??' ? "greek small letter delta"
    , "e" ## '??' ? "greek small letter epsilon"
    , "z" ## '??' ? "greek small letter zeta"
    , "E" ## '??' ? "greek small letter eta"
    , "t" ## '??' ? "greek small letter theta"
    , "i" ## '??' ? "greek small letter iota"
    , "k" ## '??' ? "greek small letter kappa"
    , "l" ## '??' ? "greek small letter lambda"
    , "m" ## '??' ? "greek small letter mu"
    , "n" ## '??' ? "greek small letter nu"
    , "x" ## '??' ? "greek small letter xi"
    , "p" ## '??' ? "greek small letter pi"
    , "r" ## '??' ? "greek small letter rho"
    , "s" ## '??' ? "greek small letter sigma"
    , "T" ## '??' ? "greek small letter tau"
    , "u" ## '??' ? "greek small letter upsilon"
    , "f" ## '??' ? "greek small letter phi"
    , "c" ## '??' ? "greek small letter chi"
    , "." ## '??' ? "greek small letter psi"
    , "o" ## '??' ? "greek small letter omega"
    ]
  , -- Arrows
    family "a" $ mconcat
    [ "a" ## '???' ? "rightwards arrow"
    , "A" ## '???' ? "rightwards double arrow"
    , "o" ## '???' ? "leftwards arrow"
    , "O" ## '???' ? "leftwards double arrow"
    , "e" ## '???' ? "downwards arrow"
    , "E" ## '???' ? "downwards double arrow"
    , "u" ## '???' ? "upwards arrow"
    , "U" ## '???' ? "upwards double arrow"
    , "'" ## '???' ? "rightwards arrow from bar"
    , "\"" ## '???' ? "leftwards arrow from bar"
    , "," ## '???' ? "left right arrow"
    , "<" ## '???' ? "left right double arrow"
    , "." ## '???' ? "rightwards arrow with stroke"
    , ">" ## '???' ? "rightwards double arrow with stroke"
    , "p" ## '???' ? "leftwards arrow with stroke"
    , "P" ## '???' ? "leftwards double arrow with stroke"
    , "s" ## '???' ? "rightwards two headed arrow"
    , "S" ## '???' ? "leftwards two headed arrow"
    , "n" ## '???' ? "rightwards arrow with hook"
    , "N" ## '???' ? "leftwards arrow with hook"
    , "l" ## '???' ? "multimap"
    , "L" ## '???' ? "left multimap"
    ]
    -- Logic
  , family "'" $ mconcat
    [ "n" ## '??' ? ""
    , "N" ## '???' ? ""
    , "a" ## '???' ? ""
    , "e" ## '???' ? ""
    , "x" ## '???' ? ""
    , "/" ## '???' ? ""
    , "\\" ## '???' ? ""
    , "t" ## '???' ? ""
    , "T" ## '???' ? ""
    , "s" ## '???' ? ""
    , "S" ## '???' ? ""
    , "," ## '???' ? "top"
    , "." ## '???' ? "bottom"
    , "d" ## '???' ? "daimon"
    , "o" ## '???' ? ""
    , "c" ## '???' ? ""
    , "u" ## '???' ? ""
    , "p" ## '???' ? ""
    ]
    -- Brackets
  , "," ## '???' ? ""
  , "." ## '???' ? ""
  , "<" ## '???' ? ""
  , ">" ## '???' ? ""
  , "[" ## '???' ? ""
  , "]" ## '???' ? ""
  , "9" ## '???' ? ""
  , "0" ## '???' ? ""
  , "(" ## '???' ? ""
  , ")" ## '???' ? ""
  , "__" ## '???' ? ""
  , "--" ## '???' ? ""
  -- Equalish
  , ":=" ## '???' ? ""
  , family "=" $ mconcat
    [ "/" ## '???' ? ""
    , "a" ## '???' ? ""
    , "o" ## '???' ? ""
    , "e" ## '???' ? ""
    , "`" ## '???' ? ""
    , "q" ## '???' ? ""
    , "t" ## '???' ? ""
    , ":" ## '???' ? ""
    ]
  , -- Relations
    family "e" $ mconcat
    [ "a" ## '???' ? ""
    , "A" ## '???' ? ""
    , "o" ## '???' ? ""
    , "O" ## '???' ? ""
    , "e" ## '???' ? ""
    , "E" ## '???' ? ""
    , "," ## '???' ? ""
    , "<" ## '???' ? ""
    , "." ## '???' ? ""
    , ">" ## '???' ? ""
    ]
    -- Operations
  , "x" ## '??' ? ""
  , "/" ## '??' ? ""
  , "$" ## '???' ? ""
  , family "o" $ mconcat
    [ "a" ## '???' ? ""
    , "A" ## '???' ? ""
    , "o" ## '???' ? ""
    , "O" ## '???' ? ""
    , "e" ## '???' ? ""
    , "E" ## '???' ? ""
    , "." ## '???' ? "ring operator"
    ]
  , -- Set
    family "u" $ mconcat
    [ "n" ## '???' ? ""
    , "e" ## '???' ? ""
    , "E" ## '???' ? ""
    , "h" ## '???' ? ""
    , "H" ## '???' ? ""
    , "a" ## '???' ? ""
    , "o" ## '???' ? ""
    , "," ## '???' ? ""
    , "." ## '???' ? ""
    , "<" ## '???' ? ""
    , ">" ## '???' ? ""
    , "9" ## '???' ? ""
    , "0" ## '???' ? ""
    , "(" ## '???' ? ""
    , ")" ## '???' ? ""
    , "'" ## '???' ? ""
    , "\"" ## '???' ? ""
    , "w" ## '???' ? ""
    , "v" ## '???' ? ""
    , "[" ## '???' ? ""
    , "]" ## '???' ? ""
    , "{" ## '???' ? ""
    , "}" ## '???' ? ""
    ]
  , -- Misc
    family "Q" $ mconcat
    [ "nab" ## '???' ? ""
    , "inf" ## '???' ? ""
    , "dag" ## '???' ? ""
    ]
  -- TODO: Calculus
  ]
