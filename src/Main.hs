module Main where

import qualified TidyImports as TI

import System.Environment

main :: IO ()
main = getArgs >>= run

run :: [String] -> IO ()
run = TI.run
