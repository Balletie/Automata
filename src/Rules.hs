module Rules where

import Data.Bits
import Universe

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

rule_30 :: (Universe u) => u Bool -> Bool
rule_30 = rule_N 30

rule_90 :: (Universe u) => u Bool -> Bool
rule_90 u = (u_read . left $ u) ⊕ (u_read . right $ u)
  where (⊕) = (/=)  -- XOR is the same as not equal

rule_110 :: (Universe u) => u Bool -> Bool
rule_110 = rule_N 110

rule_184 :: (Universe u) => u Bool -> Bool
rule_184 = rule_N 184

rule_N :: (Universe u) => Int -> u Bool -> Bool
rule_N n u = mux [u_read . left $ u, u_read u, u_read . right $ u] n
