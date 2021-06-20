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
    [ "1" ## '¹' ? "superscript one"
    , "2" ## '²' ? "superscript two"
    , "3" ## '³' ? "superscript three"
    , "4" ## '⁴' ? "superscript four"
    , "5" ## '⁵' ? "superscript five"
    , "6" ## '⁶' ? "superscript six"
    , "7" ## '⁷' ? "superscript seven"
    , "8" ## '⁸' ? "superscript eight"
    , "9" ## '⁹' ? "superscript nine"
    , "0" ## '⁰' ? "superscript zero"
    , "a" ## 'ᵃ' ? "modifier letter small a"
    , "b" ## 'ᵇ' ? "modifier letter small b"
    , "c" ## 'ᶜ' ? "modifier letter small c"
    , "d" ## 'ᵈ' ? "modifier letter small d"
    , "e" ## 'ᵉ' ? "modifier letter small e"
    , "f" ## 'ᶠ' ? "modifier letter small f"
    , "g" ## 'ᵍ' ? "modifier letter small g"
    , "h" ## 'ʰ' ? "modifier letter small h"
    , "i" ## 'ⁱ' ? "modifier letter small i"
    , "j" ## 'ʲ' ? "modifier letter small j"
    , "k" ## 'ᵏ' ? "modifier letter small k"
    , "l" ## 'ˡ' ? "modifier letter small l"
    , "m" ## 'ᵐ' ? "modifier letter small m"
    , "n" ## 'ⁿ' ? "modifier letter small n"
    , "o" ## 'ᵒ' ? "modifier letter small o"
    , "p" ## 'ᵖ' ? "modifier letter small p"
    , "r" ## 'ʳ' ? "modifier letter small r"
    , "s" ## 'ˢ' ? "modifier letter small s"
    , "t" ## 'ᵗ' ? "modifier letter small t"
    , "u" ## 'ᵘ' ? "modifier letter small u"
    , "v" ## 'ᵛ' ? "modifier letter small v"
    , "w" ## 'ʷ' ? "modifier letter small w"
    , "x" ## 'ˣ' ? "modifier letter small x"
    , "y" ## 'ʸ' ? "modifier letter small y"
    , "z" ## 'ᶻ' ? "modifier letter small z"
    ]
  , -- Subscripts
    family "_" $ mconcat
    [ "0" ## '₀' ? "subscript zero"
    , "1" ## '₁' ? "subscript one"
    , "2" ## '₂' ? "subscript two"
    , "3" ## '₃' ? "subscript three"
    , "4" ## '₄' ? "subscript four"
    , "5" ## '₅' ? "subscript five"
    , "6" ## '₆' ? "subscript six"
    , "7" ## '₇' ? "subscript seven"
    , "8" ## '₈' ? "subscript eight"
    , "9" ## '₉' ? "subscript nine"
    , "a" ## 'ₐ' ? "latin subscript small letter a"
    , "e" ## 'ₑ' ? "latin subscript small letter e"
    , "h" ## 'ₕ' ? "latin subscript small letter h"
    , "i" ## 'ᵢ' ? "latin subscript small letter i"
    , "k" ## 'ₖ' ? "latin subscript small letter k"
    , "l" ## 'ₗ' ? "latin subscript small letter l"
    , "m" ## 'ₘ' ? "latin subscript small letter m"
    , "n" ## 'ₙ' ? "latin subscript small letter n"
    , "o" ## 'ₒ' ? "latin subscript small letter o"
    , "p" ## 'ₚ' ? "latin subscript small letter p"
    , "r" ## 'ᵣ' ? "latin subscript small letter r"
    , "s" ## 'ₛ' ? "latin subscript small letter s"
    , "t" ## 'ₜ' ? "latin subscript small letter t"
    , "u" ## 'ᵤ' ? "latin subscript small letter u"
    , "v" ## 'ᵥ' ? "latin subscript small letter v"
    , "x" ## 'ₓ' ? "latin subscript small letter x"
    ]
  , --
    family "b" $ mconcat
    [ "A" ## '𝒜' ? "mathematical script capital A"
    , "B" ## 'ℬ' ? "mathematical script capital B"
    , "C" ## '𝒞' ? "mathematical script capital C"
    , "D" ## '𝒟' ? "mathematical script capital D"
    , "E" ## 'ℰ' ? "mathematical script capital E"
    , "F" ## 'ℱ' ? "mathematical script capital F"
    , "G" ## '𝒢' ? "mathematical script capital G"
    , "H" ## 'ℋ' ? "mathematical script capital H"
    , "I" ## 'ℐ' ? "mathematical script capital I"
    , "J" ## '𝒥' ? "mathematical script capital J"
    , "K" ## '𝒦' ? "mathematical script capital K"
    , "L" ## 'ℒ' ? "mathematical script capital L"
    , "M" ## 'ℳ' ? "mathematical script capital M"
    , "N" ## '𝒩' ? "mathematical script capital N"
    , "O" ## '𝒪' ? "mathematical script capital O"
    , "P" ## '𝒫' ? "mathematical script capital P"
    , "Q" ## '𝒬' ? "mathematical script capital Q"
    , "R" ## 'ℛ' ? "mathematical script capital R"
    , "S" ## '𝒮' ? "mathematical script capital S"
    , "T" ## '𝒯' ? "mathematical script capital T"
    , "U" ## '𝒰' ? "mathematical script capital U"
    , "V" ## '𝒱' ? "mathematical script capital V"
    , "W" ## '𝒲' ? "mathematical script capital W"
    , "X" ## '𝒳' ? "mathematical script capital X"
    , "Y" ## '𝒴' ? "mathematical script capital Y"
    , "Z" ## '𝒵' ? "mathematical script capital Z"
    , "a" ## '𝒶' ? "mathematical script small a"
    , "b" ## '𝒷' ? "mathematical script small b"
    , "c" ## '𝒸' ? "mathematical script small c"
    , "d" ## '𝒹' ? "mathematical script small d"
    , "e" ## 'ℯ' ? "mathematical script small e"
    , "f" ## '𝒻' ? "mathematical script small f"
    , "g" ## 'ℊ' ? "mathematical script small g"
    , "h" ## '𝒽' ? "mathematical script small h"
    , "i" ## '𝒾' ? "mathematical script small i"
    , "j" ## '𝒿' ? "mathematical script small j"
    , "k" ## '𝓀' ? "mathematical script small k"
    , "l" ## '𝓁' ? "mathematical script small l"
    , "m" ## '𝓂' ? "mathematical script small m"
    , "n" ## '𝓃' ? "mathematical script small n"
    , "o" ## 'ℴ' ? "mathematical script small o"
    , "p" ## '𝓅' ? "mathematical script small p"
    , "q" ## '𝓆' ? "mathematical script small q"
    , "r" ## '𝓇' ? "mathematical script small r"
    , "s" ## '𝓈' ? "mathematical script small s"
    , "t" ## '𝓉' ? "mathematical script small t"
    , "u" ## '𝓊' ? "mathematical script small u"
    , "v" ## '𝓋' ? "mathematical script small v"
    , "w" ## '𝓌' ? "mathematical script small w"
    , "x" ## '𝓍' ? "mathematical script small x"
    , "y" ## '𝓎' ? "mathematical script small y"
    , "z" ## '𝓏' ? "mathematical script small z"
    ]
  , -- Blackboard
    family "B" $ mconcat
    [ "A" ## '𝔸' ? "mathematical double-struck capital A"
    , "B" ## '𝔹' ? "mathematical double-struck capital B"
    , "C" ## 'ℂ' ? "double-struck capital C"
    , "D" ## '𝔻' ? "mathematical double-struck capital D"
    , "E" ## '𝔼' ? "mathematical double-struck capital E"
    , "F" ## '𝔽' ? "mathematical double-struck capital F"
    , "G" ## '𝔾' ? "mathematical double-struck capital G"
    , "H" ## 'ℍ' ? "double-struck capital H"
    , "I" ## '𝕀' ? "mathematical double-struck capital I"
    , "J" ## '𝕁' ? "mathematical double-struck capital J"
    , "K" ## '𝕂' ? "mathematical double-struck capital K"
    , "L" ## '𝕃' ? "mathematical double-struck capital L"
    , "M" ## '𝕄' ? "mathematical double-struck capital M"
    , "N" ## 'ℕ' ? "double-struck capital N"
    , "O" ## '𝕆' ? "mathematical double-struck capital O"
    , "P" ## 'ℙ' ? "double-struck capital P"
    , "Q" ## 'ℚ' ? "double-struck capital Q"
    , "R" ## 'ℝ' ? "double-struck capital R"
    , "S" ## '𝕊' ? "mathematical double-struck capital S"
    , "T" ## '𝕋' ? "mathematical double-struck capital T"
    , "U" ## '𝕌' ? "mathematical double-struck capital U"
    , "V" ## '𝕍' ? "mathematical double-struck capital V"
    , "W" ## '𝕨' ? "mathematical double-struck capital W"
    , "X" ## '𝕏' ? "mathematical double-struck capital X"
    , "Y" ## '𝕐' ? "mathematical double-struck capital Y"
    , "Z" ## 'ℤ' ? "double-struck capital Z"
    , "a" ## '𝕒' ? "mathematical double-struck small a"
    , "b" ## '𝕓' ? "mathematical double-struck small a"
    , "c" ## '𝕔' ? "mathematical double-struck small a"
    , "d" ## '𝕕' ? "mathematical double-struck small a"
    , "e" ## '𝕖' ? "mathematical double-struck small a"
    , "f" ## '𝕗' ? "mathematical double-struck small a"
    , "g" ## '𝕘' ? "mathematical double-struck small a"
    , "h" ## '𝕙' ? "mathematical double-struck small a"
    , "i" ## '𝕚' ? "mathematical double-struck small a"
    , "j" ## '𝕛' ? "mathematical double-struck small a"
    , "k" ## '𝕜' ? "mathematical double-struck small a"
    , "l" ## '𝕝' ? "mathematical double-struck small a"
    , "m" ## '𝕞' ? "mathematical double-struck small a"
    , "n" ## '𝕟' ? "mathematical double-struck small a"
    , "o" ## '𝕠' ? "mathematical double-struck small a"
    , "p" ## '𝕡' ? "mathematical double-struck small a"
    , "q" ## '𝕢' ? "mathematical double-struck small a"
    , "r" ## '𝕣' ? "mathematical double-struck small a"
    , "s" ## '𝕤' ? "mathematical double-struck small a"
    , "t" ## '𝕥' ? "mathematical double-struck small a"
    , "u" ## '𝕦' ? "mathematical double-struck small a"
    , "v" ## '𝕧' ? "mathematical double-struck small a"
    , "w" ## '𝕨' ? "mathematical double-struck small a"
    , "x" ## '𝕩' ? "mathematical double-struck small a"
    , "y" ## '𝕪' ? "mathematical double-struck small a"
    , "z" ## '𝕫' ? "mathematical double-struck small a"
    ]
  , -- Greek letters
    family "g" $ mconcat
    [ "G" ## 'Γ' ? "greek capital letter gamma"
    , "D" ## 'Δ' ? "greek capital letter gamma"
    , "Th" ## 'Θ' ? "greek capital letter gamma"
    , "L" ## 'Λ' ? "greek capital letter gamma"
    , "X" ## 'Ξ' ? "greek capital letter gamma"
    , "Pi" ## 'Π' ? "greek capital letter gamma"
    , "S" ## 'Σ' ? "greek capital letter gamma"
    , "F" ## 'Φ' ? "greek capital letter gamma"
    , "Ps" ## 'Ψ' ? "greek capital letter gamma"
    , "O" ## 'Ω' ? "greek capital letter gamma"
    , "a" ## 'α' ? "greek small letter alpha"
    , "b" ## 'β' ? "greek small letter beta"
    , "g" ## 'γ' ? "greek small letter gamma"
    , "d" ## 'δ' ? "greek small letter delta"
    , "ep" ## 'ε' ? "greek small letter epsilon"
    , "z" ## 'ζ' ? "greek small letter zeta"
    , "et" ## 'η' ? "greek small letter eta"
    , "th" ## 'θ' ? "greek small letter theta"
    , "i" ## 'ι' ? "greek small letter iota"
    , "k" ## 'κ' ? "greek small letter kappa"
    , "l" ## 'λ' ? "greek small letter lambda"
    , "m" ## 'μ' ? "greek small letter mu"
    , "n" ## 'ν' ? "greek small letter nu"
    , "x" ## 'ξ' ? "greek small letter xi"
    , "pi" ## 'π' ? "greek small letter pi"
    , "r" ## 'ρ' ? "greek small letter rho"
    , "s" ## 'σ' ? "greek small letter sigma"
    , "ta" ## 'τ' ? "greek small letter tau"
    , "u" ## 'υ' ? "greek small letter upsilon"
    , "f" ## 'φ' ? "greek small letter phi"
    , "c" ## 'χ' ? "greek small letter chi"
    , "ps" ## 'ψ' ? "greek small letter psi"
    , "o" ## 'ω' ? "greek small letter omega"
    ]
  , -- Arrows
    family "a" $ mconcat
    [ "a" ## '→' ? "rightwards arrow"
    , "A" ## '⇒' ? "rightwards double arrow"
    , "o" ## '←' ? "leftwards arrow"
    , "O" ## '⇐' ? "leftwards double arrow"
    , "e" ## '↓' ? "downwards arrow"
    , "E" ## '⇓' ? "downwards double arrow"
    , "u" ## '↑' ? "upwards arrow"
    , "U" ## '⇑' ? "upwards double arrow"
    , "'" ## '↦' ? "rightwards arrow from bar"
    , "\"" ## '↤' ? "leftwards arrow from bar"
    , "," ## '↔' ? "left right arrow"
    , "<" ## '⇔' ? "left right double arrow"
    , "." ## '↛' ? "rightwards arrow with stroke"
    , ">" ## '⇏' ? "rightwards double arrow with stroke"
    , "p" ## '↚' ? "leftwards arrow with stroke"
    , "P" ## '⇍' ? "leftwards double arrow with stroke"
    , "s" ## '↠' ? "rightwards two headed arrow"
    , "S" ## '↞' ? "leftwards two headed arrow"
    , "n" ## '↪' ? "rightwards arrow with hook"
    , "N" ## '↩' ? "leftwards arrow with hook"
    , "l" ## '⊸' ? "multimap"
    , "L" ## '⟜' ? "left multimap"
    ]
    -- Logic
  , "&" ## '⅋' ? ""
  , family "l" $ mconcat
    [ "n" ## '¬' ? ""
    , "a" ## '∀' ? ""
    , "e" ## '∃' ? ""
    , "x" ## '∄' ? ""
    , "/" ## '∧' ? ""
    , "\\" ## '∨' ? ""
    , "t" ## '⊢' ? ""
    , "T" ## '⊬' ? ""
    , "s" ## '⊣' ? ""
    , "S" ## '⊨' ? ""
    , "1" ## '⊤' ? ""
    , "0" ## '⊥' ? ""
    , "dai" ## '✠' ? ""
    , "A" ## '∏' ? ""
    , "C" ## '∐' ? ""
    , "E" ## '∑' ? ""
    ]
    -- Brackets
  , "," ## '⟨' ? ""
  , "." ## '⟩' ? ""
  , "<" ## '⟪' ? ""
  , ">" ## '⟫' ? ""
  , "[" ## '⟦' ? ""
  , "]" ## '⟧' ? ""
  , "9" ## '⌞' ? ""
  , "0" ## '⌟' ? ""
  , "(" ## '⌜' ? ""
  , ")" ## '⌝' ? ""
  , "__" ## '⎵' ? ""
  , "--" ## '⎴' ? ""
  -- Equalish
  , ":=" ## '≔' ? ""
  , family "=" $ mconcat
    [ "/" ## '≠' ? ""
    , "a" ## '≅' ? ""
    , "o" ## '≡' ? ""
    , "e" ## '≈' ? ""
    , "`" ## '≁' ? ""
    , "q" ## '≟' ? ""
    , "t" ## '≜' ? ""
    , ":" ## '≕' ? ""
    ]
  , -- Relations
    family "r" $ mconcat
    [ "c" ## '⁐' ? ""
    , "C" ## '⌢' ? ""
    , "n" ## '≍' ? ""
    , "N" ## '⌣' ? ""
    , "l" ## '≤' ? ""
    , "g" ## '≥' ? ""
    ]
    -- Operations
  , "x" ## '×' ? ""
  , "/" ## '÷' ? ""
  , "$" ## '⋔' ? ""
  , "%" ## '⋉' ? ""
  , "^" ## '⋊' ? ""
  , family "o" $ mconcat
    [ "=" ## '⊕' ? ""
    , "x" ## '⊗' ? ""
    , "+" ## '⊞' ? ""
    , "X" ## '⊠' ? ""
    ]
  , -- Set
    family "s" $ mconcat
    [ "n" ## '∅' ? ""
    , "e" ## '∈' ? ""
    , "E" ## '∉' ? ""
    , "h" ## '∋' ? ""
    , "H" ## '∌' ? ""
    , "i" ## '∩' ? ""
    , "u" ## '∪' ? ""
    , "," ## '⊂' ? ""
    , "." ## '⊃' ? ""
    , "<" ## '⊄' ? ""
    , ">" ## '⊅' ? ""
    , "9" ## '⊆' ? ""
    , "0" ## '⊇' ? ""
    , "(" ## '⊈' ? ""
    , ")" ## '⊉' ? ""
    , "w" ## '⊊' ? ""
    , "v" ## '⊋' ? ""
    , "W" ## '⊏' ? ""
    , "V" ## '⊐' ? ""
    , "[" ## '⊑' ? ""
    , "]" ## '⊒' ? ""
    , "{" ## '⋢' ? ""
    , "}" ## '⋣' ? ""
    ]
  , -- Misc
    family "m" $ mconcat
    [ "nab" ## '∇' ? ""
    , "inf" ## '∞' ? ""
    , "dag" ## '†' ? ""
    ]
  -- TODO: Calculus
  ]
