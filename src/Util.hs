module Util where

import Data.Functor
import System.Random
import System.IO

stdGenIO :: IO StdGen
stdGenIO = mkStdGen <$> randomIO

promptYesNo :: String -> Maybe Bool -> IO Bool
promptYesNo str = promptWithDefault parseYesNo showYesNo (str ++ " (y/n)")
  where parseYesNo ('y':_) = Just True
        parseYesNo ('Y':_) = parseYesNo "y"
        parseYesNo ('n':_) = Just False
        parseYesNo ('N':_) = parseYesNo "n"
        parseYesNo _       = Nothing
        showYesNo True  = "y"
        showYesNo False = "n"

promptWithDefault :: (String -> Maybe t)
                  -> (t -> String)
                  -> String
                  -> Maybe t
                  -> IO t
promptWithDefault parser shower msg def = do
  putStr $ msg ++ "?" ++ default_msg
  hFlush stdout
  input <- getLine
  case (input, def) of
    ("", Just d) -> return d
    _            -> case parser input of
                      Just ans -> return ans
                      _        -> do putStr " Invalid input, please try again\n"
                                     hFlush stdout
                                     promptWithDefault parser shower msg def
  where default_msg = case shower <$> def of
                        Just v -> " (Default: " ++ v ++ ") "
                        _      -> " "

-- |Utility function used by toString
(.**) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(.**) = (.) . (.) . (.)
