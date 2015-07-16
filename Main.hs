module Main where

import Shoot
import Data.Word

main :: IO ()
main = compile $ fastpow (var 0 :: E Int) (var 1 :: E Word)
