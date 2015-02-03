module Util where

import System.Random
import Data.Functor

stdGenIO :: IO StdGen
stdGenIO = mkStdGen <$> randomIO

-- |Utility function used by toString
(.**) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.**) = (.) . (.) . (.)
