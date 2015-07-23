{-# LANGUAGE EmptyDataDecls #-}
module Main where

import Prelude hiding (max)
import Shoot
import Data.Word

data C16
instance Count C16 where countof _ = 16
data C1
instance Count C1 where countof _ = 1

main :: IO ()
main = compile $
  (ex (var 0) (vec [0 .. ] :: E (V C1 Int)))
  -- (ex (var 0) (vec [0 .. ] :: E (V C16 Int)))
  -- evalA 1 (var 0)
  -- ((snd $ fkMain (var 0)) :: E Int)
  -- factorial (var 0 :: E Word64)
  -- fkFlip $ fst $ nextPerm $ nextPerm perm0
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
