module Universe where

import Control.Comonad

data ListZipper a = LZ [a] a [a]

left :: ListZipper a -> ListZipper a
left (LZ (x:xs) y ys) = LZ xs x (y:ys)

right :: ListZipper a -> ListZipper a
right (LZ xs x (y:ys)) = LZ (x:xs) y ys

listRead :: ListZipper a -> a
listRead (LZ _ x _) = x

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ xs _ ys) = LZ xs x ys

instance Functor ListZipper where
  fmap f (LZ xs x ys) = LZ (map f xs) (f x) (map f ys)

instance Comonad ListZipper where
  duplicate a = LZ (tail $ iterate left a) a (tail $ iterate right a)
  extract = listRead

rule :: ListZipper Bool -> Bool
rule (LZ (a:_) _ (c:_)) = a ⊕ c
  where (⊕) = (/=)

shift :: Int -> ListZipper a -> ListZipper a
shift i u = iterate (if i < 0 then left else right) u !! abs i

toList :: Int -> Int -> ListZipper a -> [a]
toList i j u = take (j-i) $ half $ shift i u
  where half (LZ _ b c) = [b] ++ c
