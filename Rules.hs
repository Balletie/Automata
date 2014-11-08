module Rules where

import Data.Bits

type Rule = Int

mux :: [Bool] -> Rule -> Bool
mux bs rule = testBit rule (toInt bs)

toInt :: [Bool] -> Int
toInt = toInt' 0 . reverse

toInt' :: Int -> [Bool] -> Int
toInt' _     []     = 0
toInt' place (b:bs) = (boolToInt b) * 2 ^ place + toInt' (succ place) bs
  where boolToInt False = 0
        boolToInt True  = 1
