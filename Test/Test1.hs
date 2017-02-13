module Test.Test1 where

import Control.Monad(when,unless)
import Control.Applicative(pure,(<$>),(<*>))
import Data.List
import Debug.Trace
import qualified Data.Map    as DM
import qualified Data.IntMap as DIM


main :: IO ()
main = do
  unless False $ putStrLn "hi"
  print $ sort "asdf"
  succ <$> return (1 :: Int)
  return ()

empty_map :: DM.Map String Int
empty_map = DM.empty
