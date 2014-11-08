module Universe where

import Control.Comonad
import Data.Sequence (iterateN)
import qualified Data.Foldable as Foldable (toList)

class (Comonad u) => Universe u where
  left :: u a -> u a
  right :: u a -> u a
  u_read :: u a -> a
  u_write :: a -> u a -> u a
  u_take :: Int -> u a -> [a]

data ListZipper a = LZ [a] a [a]

instance Universe ListZipper where
  left (LZ (x:xs) y ys) = LZ xs x (y:ys)
  left _ = error "Empty universe"

  right (LZ xs x (y:ys)) = LZ (x:xs) y ys
  right _ = error "Empty universe"

  u_read (LZ _ x _) = x
  u_write x (LZ xs _ ys) = LZ xs x ys
  u_take i (LZ _ x xs) = x : (take (pred i) xs)

instance Functor ListZipper where
  fmap f (LZ xs x ys) = LZ (map f xs) (f x) (map f ys)

instance Comonad ListZipper where
  duplicate a = LZ (tail $ iterate left a) a (tail $ iterate right a)
  extract = u_read

data Loop a = L [a] a

loopLength :: Loop a -> Int
loopLength (L xs _) = succ $ length xs

instance Universe Loop where
  left (L xs y) = L (xs++[y]) $ head xs
  right (L xs y) = L (y:xs) $ last xs

  u_read (L _ x) = x

  u_write x (L xs _) = L xs x

  u_take i _        | i <= 0 = []
  u_take i (L xs x)          = x : (take (pred i) xs) ++ (u_take ((pred i) - length xs) (L xs x))

instance Functor Loop where
  fmap f (L xs x) = L (map f xs) (f x)

instance Comonad Loop where
  duplicate u = L (Foldable.toList (iterateN (pred $ loopLength u) left u)) $ u
  extract = u_read

shift :: (Universe u) => Int -> u a -> u a
shift i u = iterate (if i < 0 then left else right) u !! abs i

toList :: (Universe u) => Int -> Int -> u a -> [a]
toList i j = u_take (j-i) . shift i

toString :: (Universe u) => Int
                         -> Int
                         -> [u Bool]
                         -> String
toString a b u = unlines $ take b $ map (map stringRep . toList (1-middle) middle) u
  where middle = a `quot` 2
        stringRep x = case x of True -> '@'
                                False -> '.'

rule_90 :: (Universe u) => u Bool -> Bool
rule_90 u = (u_read . left $ u) ⊕ (u_read . right $ u)
  where (⊕) = (/=)  -- XOR is the same as not equal
