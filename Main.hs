module Main where

import Universe
import Rules
import Control.Comonad
import Codec.Picture
import System.Random
import System.IO

getRule :: IO Int
getRule = do
  putStr "Rule (Wolfram code): "
  hFlush stdout
  rule_str <- getLine
  return (read rule_str :: Int)

getInitialConditions :: IO (Loop Bool)
getInitialConditions = do
  putStr "Random initial conditions (y/n): "
  hFlush stdout
  random_init <- getLine
  case random_init of "y" -> randomLoopConditions
                      _   -> defaultLoopConditions

defaultListConditions :: IO (ListZipper Bool)
defaultListConditions = do
  return (LZ (repeat False) True (repeat False))

randomListConditions :: IO (ListZipper Bool)
randomListConditions = do
  genleft <- stdGenIO
  leftList <- return (randoms genleft)
  middle <- randomIO :: IO Bool
  genright <- stdGenIO
  rightList <- return (randoms genright)
  return (LZ leftList middle rightList)

defaultLoopConditions :: IO (Loop Bool)
defaultLoopConditions = do
  return (L (replicate 100 False) True)

randomLoopConditions :: IO (Loop Bool)
randomLoopConditions = do
  genleft <- stdGenIO
  leftList <- return (take 100 $ randoms genleft)
  middle <- randomIO :: IO Bool
  {-genright <- stdGenIO
  rightList <- return (randoms genright)-}
  return (L leftList middle)

stdGenIO :: IO StdGen
stdGenIO = do
  seed <- randomIO
  return (mkStdGen seed)

getImage :: Universe u => Int -> Int -> [u Bool] -> Image Pixel8
getImage width height u = generateImage (imageRenderer (toWord8 width height u)) width height

imageRenderer :: [[Pixel8]] -> Int -> Int -> Pixel8
imageRenderer pixels i j = pixels !! j !! i

main :: IO()
main = do
  rule <- getRule
  u <- getInitialConditions
  let us = iterate (=>> rule_N rule) u
  --putStr $ (toString 30 10) $ us
  writePng "test.png" $ getImage 400 800 us
