{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding (max)
import Shoot
import Data.Word

data C16
instance Count C16 where ecountof _ = 16
data C4
instance Count C4 where ecountof _ = 4

main :: IO ()
main = compile $
  let arr :: E (V C4 Int) = vec [5 .. ] in
  (vfold (+) (var 0) arr)
  -- (ex (vmap (+ 1) arr) (var 0))
  -- (ex arr (var 0))
  -- doesn't work (ex (ex (vec (repeat $ vec [5 .. ]) :: E (V C4 (V C4 Int))) (var 0)) (var 0))
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
