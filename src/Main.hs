module Main where

import Data.Array.IArray
import Universe
import Rules
import Util
import Text.Read
import System.Random
import Control.Comonad
import Codec.Picture
import Graphics.Gloss
import Graphics.Gloss.Juicy

getType :: IO Bool
getType = promptYesNo "Loop" (Just False)

getRule :: IO Rule
getRule = promptWithDefault readMaybe show question def
  where question = "Rule (Wolfram code)"
        def = Just 110

getInitialCondition :: IO Bool
getInitialCondition = promptYesNo "Random initial conditions" (Just False)

getImage :: Universe u => Int -> Int -> [u Bool] -> Image Pixel8
getImage width height u = generateImage (imageRenderer arr) width height
  where arr = listArray ((0,0),(height-1,width-1)) pixels
        pixels = concat (toWord8 width height u)

imageRenderer :: Array (Int,Int) Pixel8 -> Int -> Int -> Pixel8
imageRenderer pixels i j = pixels ! (j,i)

main :: IO()
main = do
  loop_type <- getType
  rule <- getRule
  random_init <- getInitialCondition
  if loop_type then do
    loop_u <- getLoopU random_init
    runUniverse rule loop_u
  else do
    list_u <- getListU random_init
    runUniverse rule list_u

runUniverse :: Universe u => Rule -> u Bool -> IO()
runUniverse rule u = do
  let us = iterate (=>> rule_N rule) u
      width = 400
      height = 800
      image = getImage width height us
      disp  = (InWindow ("Rule " ++ (show rule)) (width, height) (5, 5))

  display disp white (fromImageY8 image)
  --putStr $ (toString 33 10) $ us
  --writePng "test.png" image

getLoopU :: Bool -> IO (Loop Bool)
getLoopU True = random_loop_u
getLoopU _    = return def_loop_u

random_loop_u :: IO (Loop Bool)
random_loop_u = do
  genleft <- stdGenIO
  leftList <- return (take 95 $ randoms genleft)
  middle <- randomIO :: IO Bool
  return (L leftList middle)

def_loop_u :: Loop Bool
def_loop_u = L (replicate 95 False) True

getListU :: Bool -> IO (ListZipper Bool)
getListU True = random_list_u
getListU _    = return def_list_u

random_list_u :: IO (ListZipper Bool)
random_list_u = do
  genleft <- stdGenIO
  leftList <- return (randoms genleft)
  middle <- randomIO :: IO Bool
  genright <- stdGenIO
  rightList <- return (randoms genright)
  return (LZ leftList middle rightList)

def_list_u :: ListZipper Bool
def_list_u = LZ (repeat False) True (repeat False)
