module Main where

import Data.Array.IArray
import Universe
import Rules
import Control.Comonad
import Codec.Picture
import System.IO
import Graphics.Gloss
import Graphics.Gloss.Juicy

getType :: IO Bool
getType = do
  putStr "Loop (y/n): "
  hFlush stdout
  type_str <- getLine
  return $ case type_str of "y" -> True
                            _   -> False

getRule :: IO Int
getRule = do
  putStr "Rule (Wolfram code): "
  hFlush stdout
  rule_str <- getLine
  return (read rule_str :: Int)

getInitialConditions :: Universe u => IO (u Bool)
getInitialConditions = do
  putStr "Random initial conditions (y/n): "
  hFlush stdout
  random_init <- getLine
  case random_init of "y" -> random_universe
                      _   -> return (default_universe)

getImage :: Universe u => Int -> Int -> [u Bool] -> Image Pixel8
getImage width height u = generateImage (imageRenderer arr) width height
  where arr = listArray ((0,0),(height-1,width-1)) pixels
        pixels = concat (toWord8 width height u)

imageRenderer :: Array (Int,Int) Pixel8 -> Int -> Int -> Pixel8
imageRenderer pixels i j = pixels ! (j,i)

main :: IO()
main = do
  u_type <- getType
  rule <- getRule
  u <- if u_type then
         getInitialConditions :: IO (Loop Bool)
         else
           getInitialConditions
  let us = iterate (=>> rule_N rule) u
      width = 400
      height = 800
      image = getImage width height us
      disp  = (InWindow ("Rule " ++ (show rule)) (width, height) (5, 5))
  display disp white (fromImageY8 image)
  --putStr $ (toString 33 10) $ us
  --writePng "test.png" image
