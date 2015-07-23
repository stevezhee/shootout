module Main where

import Prelude hiding (max)
import Shoot
import Data.Word

main :: IO ()
main = compile $
  ((snd $ tkMain (var 0)) :: E Word64)
  -- tkFoo (var 0)
  -- factorial (var 0 :: E Word64)
  -- tkFlip $ fst $ nextPerm $ nextPerm perm0
  -- fst $ nextPerm $ nextPerm perm0
  -- fst perm0
  --updix list0 (var 0) (+ 3)
  -- setix list0 (var 0) 2
  -- getix list0 (var 0)
  -- rotate list0 (var 0)
--  (fannkuchredux ([4,2,1,5,3] :: [E Word]) :: E Word)
--  nbody (var 0)
--  sqrt (var 0 :: E Double)
--  (+) (var 0 :: E Double) (var 0 :: E Double)
--  max (var 0 :: E Int) (var 1)
--  fastpow (var 0 :: E Int) (var 1 :: E Word)
