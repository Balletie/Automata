module Main where

import Universe
import Control.Comonad
import System.Random

stdGenIO :: IO StdGen
stdGenIO = do
  seed <- randomIO
  return (mkStdGen seed)

main :: IO()
main = do
  genleft <- stdGenIO
  leftList <- return (randoms genleft)
  middle <- randomIO :: IO Bool
  genright <- stdGenIO
  rightList <- return (randoms genright)

  let u = LZ leftList middle rightList
  putStr $ (toString 100 63) $ iterate (=>> rule_90) u
