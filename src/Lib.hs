module Lib
    ( runNoteCards
    ) where

import Control.Monad
import System.Random

data NoteCard = NoteCard {
  cardPinyin :: String,
  cardMeaning :: String,
  cardCharacter :: String
}

instance Show NoteCard where
  show (NoteCard p m c) = p ++ "\n" ++ m ++ "\n" ++ c ++ "\n"

pickCards gen cards = map (cards !!) $ randomRs (0, maxCard) gen
  where maxCard = length cards - 1

quizCard :: StdGen -> NoteCard -> (String, StdGen)
quizCard gen card
  | (n :: Integer) == 0 = (cardPinyin card, newGen)
  | otherwise = (cardMeaning card, newGen)
  where
    (n, newGen) = randomR (0,1) gen

readCardContents :: String -> [NoteCard]
readCardContents contents = map linesToCard result
  where
    l = filter (not . null) (lines contents)
    linesToCard [c,m,p] = NoteCard p m c
    (result, _) = foldl fn ([[]], 0) l
    fn (a:accum, cnt) el
      | cnt < 3 = ((el:a):accum, cnt + 1)
      | otherwise = ([el]:a:accum, 1)

cardInteraction :: StdGen -> NoteCard -> IO StdGen
cardInteraction gen card = do
  putStrLn cardText
  getLine
  print card
  getLine
  return newGen
  where
    (cardText, newGen) = quizCard gen card 

runNoteCards :: String -> IO ()
runNoteCards fileContents = do
  gen <- getStdGen
  let (g1, g2) = split gen
  let cards = readCardContents fileContents
  foldM_ cardInteraction g1 (pickCards g2 cards)
