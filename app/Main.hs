module Main where

import Control.Monad (forM_, when)

import Data.IORef (modifyIORef, newIORef, readIORef)
import Data.List ((\\))
import Data.Semigroup ((<>))

import System.Random (Random(randomRIO))

import Text.Printf (printf)

numRuns :: Int
numRuns = 30000

times :: Int -> IO a -> IO ()
times n = forM_ [1 .. n] . const

trials :: ChooseFn -> String -> IO ()
trials chooseFn trialName = do
  numWinsRef <- newIORef 0
  times numRuns $ do
    won <- montyHall chooseFn
    when won $ modifyIORef numWinsRef succ
  numWins <- readIORef numWinsRef
  putStrLn $ renderTrial numWins
  where
    renderTrial numWins =
      let winPercentage :: Float
          winPercentage = (fromIntegral numWins / fromIntegral numRuns) * 100
      in printf "%s: %.2f" trialName winPercentage

main :: IO ()
main = do
  trials chooseRandomly "Randomize"
  trials (choosePredetermined Stay) "Always Stay"
  trials (choosePredetermined Switch) "Always Switch"

type Door = Int

maxDoor :: Door
maxDoor = 3

minDoor :: Door
minDoor = 1

doors :: [Door]
doors = [minDoor .. maxDoor]

chooseDoor :: IO Door
chooseDoor = randomRIO (minDoor, maxDoor)

others :: [Door] -> [Door]
others = (doors \\)

randomElem :: [a] -> IO a
randomElem xs = do
  x <- randomRIO (0, length xs - 1)
  pure $ xs !! x

generateDoors :: IO (Door, [Door])
generateDoors = do
  winner <- chooseDoor
  pure (winner, others [winner])

revealGoat :: Door -> Door -> IO Door
revealGoat winner choice = randomElem $ others [choice, winner]

data Action
  = Stay
  | Switch

type ChooseFn = Door -> Door -> IO Door

choosePredetermined :: Action -> ChooseFn
choosePredetermined action choice otherDoor =
  pure $
  case action of
    Stay -> choice
    Switch -> otherDoor

chooseRandomly :: ChooseFn
chooseRandomly choice otherDoor = randomElem [choice, otherDoor]

montyHall :: ChooseFn -> IO Bool
montyHall chooseFn = do
  problem@(winner, losers) <- generateDoors
  choice <- chooseDoor
  goatDoor <- revealGoat winner choice
  let otherDoor = head $ others [choice, goatDoor]
  choice' <- chooseFn choice otherDoor
  pure $ choice' == winner
