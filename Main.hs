module Main where

import Universe
import Rules
import Control.Comonad
import Codec.Picture
import System.Random

stdGenIO :: IO StdGen
stdGenIO = do
  seed <- randomIO
  return (mkStdGen seed)

imageRenderer :: [[Pixel8]] -> Int -> Int -> Pixel8
imageRenderer pixels i j = pixels !! j !! i

getImage :: Universe u => Int -> Int -> [u Bool] -> Image Pixel8
getImage width height u = generateImage (imageRenderer (toWord8 width height u)) width height

defaultInitialConditions :: IO (ListZipper Bool)
defaultInitialConditions = do
  return (LZ (repeat False) True (repeat False))

randomInitialConditions :: IO (ListZipper Bool)
randomInitialConditions = do
  genleft <- stdGenIO
  leftList <- return (randoms genleft)
  middle <- randomIO :: IO Bool
  genright <- stdGenIO
  rightList <- return (randoms genright)
  return (LZ leftList middle rightList)

main :: IO()
main = do
  putStr "Rule (Wolfram code): "
  rule_str <- getLine

  u <- randomInitialConditions
  --let u = LZ leftList middle rightList

  let rule = read rule_str :: Int
  let us = iterate (=>> rule_N rule) u
  --putStr $ (toString 9 63) $ us
  writePng "test.png" $ getImage 1000 1000 us
