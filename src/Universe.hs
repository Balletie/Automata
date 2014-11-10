module Universe where

import Control.Comonad
import Data.Sequence (iterateN)
import qualified Data.Foldable as Foldable (toList)
import Data.Word

class (Comonad u) => Universe u where
  left :: u a -> u a
  right :: u a -> u a
  u_read :: u a -> a
  u_write :: a -> u a -> u a
  u_take :: Int -> u a -> [a]

data ListZipper a = LZ [a] a [a]
  deriving Show

instance Universe ListZipper where
  left (LZ (x:xs) y ys) = LZ xs x (y:ys)
  left _                = error "Empty universe"

  right (LZ xs x (y:ys)) = LZ (x:xs) y ys
  right _                = error "Empty universe"

  u_read (LZ _ x _) = x
  u_write x (LZ xs _ ys) = LZ xs x ys
  u_take i (LZ _ x xs) = x : (take (pred i) xs)

instance Functor ListZipper where
  fmap f (LZ xs x ys) = LZ (map f xs) (f x) (map f ys)

instance Comonad ListZipper where
  duplicate a = LZ (tail $ iterate left a) a (tail $ iterate right a)
  extract = u_read

data Loop a = L [a] a
  deriving Show

loopLength :: Loop a -> Int
loopLength (L xs _) = succ $ length xs

instance Universe Loop where
  right (L xs y) = L (tail xs ++ [y]) $ head xs

  left (L xs y) = L (y : init xs) $ last xs

  u_read (L _ x) = x

  u_write x (L xs _) = L xs x

  u_take i (L xs x)
    | i <= 0    = []
    | otherwise = x : (take j xs) ++ (u_take (j - length xs) u)
    where j  = pred i
          u  = (L xs x)

instance Functor Loop where
  fmap f (L xs x) = L (map f xs) (f x)

instance Comonad Loop where
  duplicate u = L (tail $ Foldable.toList (iterateN (loopLength u) right u)) $ u
  extract = u_read

-- |Shift a Universe "|n|" times to the left or the right, depending on the sign
-- of "n".
shift :: (Universe u) => Int -> u a -> u a
shift i u = iterate (if i < 0 then left else right) u !! abs i

-- |Return a part of the Universe as a List, using the given bounds.
-- From Dan Piponi's blog post.
toList :: (Universe u) => Int -> Int -> u a -> [a]
toList i j = u_take (j-i) . shift i

-- |Convert each Bool in the Universe to a representation,
-- using the given conversion function. Inspired by Dan Piponi's blog post
toView :: (Universe u) => (Bool -> a) -- ^ The conversion function
                       -> Int         -- ^ The width of the view
                       -> Int         -- ^ The height of the view
                       -> [u Bool]    -- ^ The Universes to be converted
                       -> [[a]]       -- ^ The representation
toView f a b u = take b $ map (map f . toList (-middle) (middle+oddity)) u
  where middle = a `quot` 2
        oddity = a `mod`  2

-- |Convert the given Universes to a String, which can be
-- displayed in a terminal window
toString :: (Universe u) => Int       -- ^ The width of each line
                         -> Int       -- ^ The number of lines (the height)
                         -> [u Bool]  -- ^ The Universes
                         -> String    -- ^ The resulting String
toString = unlines .** toView stringRep
  where stringRep x = case x of True -> '@'
                                False -> '.'

-- |Convert the Universes to a grid of pixel values (Word8), for writing to
-- an image file, or displaying it in a window.
toWord8 :: Universe u => Int -> Int -> [u Bool] -> [[Word8]]
toWord8 = toView pixelRep
  where pixelRep x = case x of True -> minBound :: Word8
                               False -> maxBound :: Word8

-- |Utility function used by toString
(.**) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.**) = (.) . (.) . (.)
