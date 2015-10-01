{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RebindableSyntax #-}

module Main where

import Prelude hiding (Num(..))
import Typed
-- import Untyped
-- import Data.Word

-- data C16 = C16
-- instance Count C16 where ecountof _ = 16
-- data C4 = C4
-- instance Count C4 where ecountof _ = 4
-- data C1 = C1
-- instance Count C1 where ecountof _ = 1
-- data C2 = C2
-- instance Count C2 where ecountof _ = 2
-- data C5500 = C5500
-- instance Count C5500 where ecountof _ = 5500

foo x = do
  print $ pp x
  print $ pp $ runEval x
  
main :: IO ()
main = do
  -- print tt
  foo $ fastpow 2 (3 :: E Int)
  foo $ fastpow (3 :: E Int) 2
  foo $ dbl (dbl (2 :: E Double))
  
-- main = compile $
--   spctMain C1
  -- let arr :: E (V C4 Int) = vec [5 .. ] in
  -- (vfold (+) (var 0) arr)
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
