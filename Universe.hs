module Universe where

import Control.Comonad

class Universe u where
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

--data Loop a = L [a] a

shift :: (Universe u) => Int -> u a -> u a
shift i u = iterate (if i < 0 then left else right) u !! abs i

toList :: (Universe u) => Int -> Int -> u a -> [a]
toList i j = u_take (j-i) . shift i

rule_90 :: (Universe u) => u Bool -> Bool
rule_90 u = (u_read . left $ u) ⊕ (u_read . right $ u)
  where (⊕) = (/=)  -- XOR is the same as not equal
