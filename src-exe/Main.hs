module Main where

import Control.Lens
import qualified Data.Map as Map
import Data.Semigroup
import System.Exit
import System.IO

import GenXCompose

main :: IO ()
main = do
  let Max longest = foldMap (Max . length) (Map.keys $ unEntries allEntries)
  iforM_ allEntries $ \inputseq sym -> do
    case stringToKeySequence Left Right inputseq of
      Left c -> do
        hPutStrLn stderr $ pure c <> " shouldn't be used in an input sequence senpai :3"
        exitFailure
      Right s -> hPutStrLn stdout $ padRight (max 40 longest) s <> " : " <> symbolToEntry sym
  pure ()

padRight :: Int -> String -> String
padRight n s = s ++ replicate (n - length s) ' '
