module Main where

import Universe
import Control.Comonad

main :: IO()
main =
  let u = LZ (repeat False) True (repeat False)
    in putStr $ unlines $ take 63 $
       map (map (\x -> if x then '#' else '.') . toList (-72) 73) $
       iterate (=>> rule_90) u
