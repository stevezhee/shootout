module Main where

import Prelude hiding (max)
import Shoot
import Data.Word

main :: IO ()
main = compile $ max (var 0 :: E Int) (var 1) -- fastpow (var 0 :: E Int) (var 1 :: E Word)
