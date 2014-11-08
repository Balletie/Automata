module Main where

import Universe
import Control.Comonad
import System.Random

main :: IO()
main = do
  seedleft <- randomIO
  let genleft = mkStdGen seedleft
  leftList <- return (randoms genleft)

  middle <- randomIO :: IO Bool

  seedright <- randomIO
  let genright = mkStdGen seedright
  rightList <- return (randoms genright)
  let u = LZ leftList middle rightList
  putStr $ (toString 100 63) $ iterate (=>> rule_90) u
