module Main where

import Data.Array.IArray
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
defaultListConditions = return (LZ (repeat False) True (repeat False))

randomListConditions :: IO (ListZipper Bool)
randomListConditions = do
  genleft <- stdGenIO
  leftList <- return (randoms genleft)
  middle <- randomIO :: IO Bool
  genright <- stdGenIO
  rightList <- return (randoms genright)
  return (LZ leftList middle rightList)

defaultLoopConditions :: IO (Loop Bool)
defaultLoopConditions = return (L (replicate 95 False) True)

randomLoopConditions :: IO (Loop Bool)
randomLoopConditions = do
  genleft <- stdGenIO
  leftList <- return (take 95 $ randoms genleft)
  middle <- randomIO :: IO Bool
  return (L leftList middle)

stdGenIO :: IO StdGen
stdGenIO = mkStdGen <$> randomIO

getImage :: Universe u => Int -> Int -> [u Bool] -> Image Pixel8
getImage width height u = generateImage (imageRenderer arr) width height
  where arr = listArray ((0,0),(height-1,width-1)) pixels
        pixels = concat (toWord8 width height u)

imageRenderer :: Array (Int,Int) Pixel8 -> Int -> Int -> Pixel8
imageRenderer pixels i j = pixels ! (j,i)

main :: IO()
main = do
  rule <- getRule
  u <- getInitialConditions
  let us = iterate (=>> rule_N rule) u
  --putStr $ (toString 33 10) $ us
  writePng "test.png" $ getImage 400 800 us
