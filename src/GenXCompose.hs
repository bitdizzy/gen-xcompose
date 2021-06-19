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

instance Semigroup (Entries s) where
  (Entries e1) <> (Entries e2) = Entries $ Map.unionWithKey (\k -> error $ "Overlap at " ++ k) e1 e2

instance Monoid (Entries s) where
  mempty = Entries mempty

family :: Char -> Entries s -> Entries s
family c = Entries . Map.mapKeysMonotonic (c:) . unEntries

allEntries :: Entries Symbol
allEntries = mconcat
  [ -- Superscripts
    family '^' $ mconcat
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
    ]
  , -- Arrows
    family 'a' $ mconcat
    [
    ]
  , -- Relations
    family 'r' $ mconcat
    [ -- Equalities
      family 'e' $ mconcat
      [
      ]
    , -- Comparisons
      family 'c' $ mconcat
      [
      ]
    , -- Set theoretic
      family 's' $ mconcat
      [
      ]
    ]
  , -- Operations
    family 'o' $ mconcat
    [ mconcat []
    , -- circled operations
      family 'o' $ mconcat
      [
      ]
    ]
  ]
