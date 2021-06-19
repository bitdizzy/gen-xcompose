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

(##) :: Char -> Symbol -> Entries Symbol
k ## sym = Entries $ Map.singleton (pure k) sym

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

--TODO: Handle conflicts where one entry's input sequence is a prefix of another's
instance Semigroup (Entries s) where
  (Entries e1) <> (Entries e2) = Entries $ Map.unionWithKey (\k -> error $ "Overlap at " ++ k) e1 e2

instance Monoid (Entries s) where
  mempty = Entries mempty

family :: String -> Entries s -> Entries s
family prefix = Entries . Map.mapKeysMonotonic (prefix <>) . unEntries

allEntries :: Entries Symbol
allEntries = mconcat
  [ -- Superscripts
    family "sup" $ mconcat
    [ '1' ## '¹' ? "superscript one"
    , '2' ## '²' ? "superscript two"
    , '3' ## '³' ? "superscript three"
    , '4' ## '⁴' ? "superscript four"
    , '5' ## '⁵' ? "superscript five"
    , '6' ## '⁶' ? "superscript six"
    , '7' ## '⁷' ? "superscript seven"
    , '8' ## '⁸' ? "superscript eight"
    , '9' ## '⁹' ? "superscript nine"
    , '0' ## '⁰' ? "superscript zero"
    , 'a' ## 'ᵃ' ? "modifier letter small a"
    , 'b' ## 'ᵇ' ? "modifier letter small b"
    , 'c' ## 'ᶜ' ? "modifier letter small c"
    , 'd' ## 'ᵈ' ? "modifier letter small d"
    , 'e' ## 'ᵉ' ? "modifier letter small e"
    , 'f' ## 'ᶠ' ? "modifier letter small f"
    , 'g' ## 'ᵍ' ? "modifier letter small g"
    , 'h' ## 'ʰ' ? "modifier letter small h"
    , 'i' ## 'ⁱ' ? "modifier letter small i"
    , 'j' ## 'ʲ' ? "modifier letter small j"
    , 'k' ## 'ᵏ' ? "modifier letter small k"
    , 'l' ## 'ˡ' ? "modifier letter small l"
    , 'm' ## 'ᵐ' ? "modifier letter small m"
    , 'n' ## 'ⁿ' ? "modifier letter small n"
    , 'o' ## 'ᵒ' ? "modifier letter small o"
    , 'p' ## 'ᵖ' ? "modifier letter small p"
    , 'r' ## 'ʳ' ? "modifier letter small r"
    , 's' ## 'ˢ' ? "modifier letter small s"
    , 't' ## 'ᵗ' ? "modifier letter small t"
    , 'u' ## 'ᵘ' ? "modifier letter small u"
    , 'v' ## 'ᵛ' ? "modifier letter small v"
    , 'w' ## 'ʷ' ? "modifier letter small w"
    , 'x' ## 'ˣ' ? "modifier letter small x"
    , 'y' ## 'ʸ' ? "modifier letter small y"
    , 'z' ## 'ᶻ' ? "modifier letter small z"
    ]
  , -- Subscripts
    family "sub" $ mconcat
    [ '0' ## '₀' ? "subscript zero"
    , '1' ## '₁' ? "subscript one"
    , '2' ## '₂' ? "subscript two"
    , '3' ## '₃' ? "subscript three"
    , '4' ## '₄' ? "subscript four"
    , '5' ## '₅' ? "subscript five"
    , '6' ## '₆' ? "subscript six"
    , '7' ## '₇' ? "subscript seven"
    , '8' ## '₈' ? "subscript eight"
    , '9' ## '₉' ? "subscript nine"
    , 'a' ## 'ₐ' ? "latin subscript small letter a"
    , 'e' ## 'ₑ' ? "latin subscript small letter e"
    , 'h' ## 'ₕ' ? "latin subscript small letter h"
    , 'k' ## 'ₖ' ? "latin subscript small letter k"
    , 'l' ## 'ₗ' ? "latin subscript small letter l"
    , 'm' ## 'ₘ' ? "latin subscript small letter m"
    , 'n' ## 'ₙ' ? "latin subscript small letter n"
    , 'o' ## 'ₒ' ? "latin subscript small letter o"
    , 'p' ## 'ₚ' ? "latin subscript small letter p"
    , 'r' ## 'ᵣ' ? "latin subscript small letter r"
    , 's' ## 'ₛ' ? "latin subscript small letter s"
    , 't' ## 'ₜ' ? "latin subscript small letter t"
    , 'u' ## 'ᵤ' ? "latin subscript small letter u"
    , 'v' ## 'ᵥ' ? "latin subscript small letter v"
    , 'x' ## 'ₓ' ? "latin subscript small letter x"
    ]
  , -- Arrows
    family "arr" $ mconcat
    [
    ]
  , -- Relations
    family "rel" $ mconcat
    [ -- Equalities
    ]
  , -- Operations
    family "op" $ mconcat
    [ mconcat []
    , -- circled operations
      family "o" $ mconcat
      [
      ]
    ]
  ]
