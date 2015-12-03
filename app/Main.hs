{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Shoot
import Fannkuch
import NBody
import Spectral

main = do
  print $ pp $ compile
    -- [ defIO $ proc "foo" $ \() -> do
    --      puti 42
    --      puti 27
    --      puti 39
    -- [ def $ func "foo" $ \() -> 42 :: Int' -- BAL: incorrect
    -- [ def $ func "foo" $ \x -> x + 42 :: Int'
    -- [ def $ func "foo" $ \(x,y) -> x + y :: Int'
    -- [ def $ func "foo" $ \x -> switch x [] x :: Word'
    -- [ def $ func "foo" $ \x -> switch x [42] x :: Word'
    -- [ def $ func "foo" $ \(x,y) -> switch x [42,y] x :: Word'
    [
      -- def $ func "foo" $ \(x :: Word') -> while 0 $ \i -> (i < x, succ i)
      -- def $ func "foo" $ \(x :: Word', y) -> fastpow x y
      -- def $ func "foo" $ \(x :: Word') -> spctMain C1
      -- def $ func "foo" $ \(x :: Word') -> negate x -- the wrong definition of negate will make this loop
      -- def $ func "foo" $ \(x :: Word') -> reps 3 x (+ 1)
      -- def $ func "factorial" $ \(i :: Int') -> factorial i
      -- defIO $ proc "factorial" (puti . factorial)
      -- defIO fannkuchredux
      -- defIO nbody
      def $ func "spectral" $ \(x :: Word') -> spctMain C1
    ]

data C1 = C1
instance Count C1 where countof _ = 1
                        
fastpow :: (Agg a, Arith a, Cmp a Bool') => a -> a -> a
fastpow b e =
  while_ ((b, e), 1) $ \((b, e), r) ->
    (e > 0, ((b * b, e / 2), if' ((e % 2) /= 0) (r * b) r))

-- import Prelude ((>>), ($), print, return, IO, Float, Int, Double, (.))
-- import Typed
-- import Eval
-- import SMTEval
-- import CEval
-- -- import Untyped
-- import Data.Word
-- import CBOR

-- -- data C16 = C16
-- -- instance Count C16 where ecountof _ = 16
-- -- data C4 = C4
-- -- instance Count C4 where ecountof _ = 4
-- -- data C1 = C1
-- -- instance Count C1 where ecountof _ = 1
-- -- data C2 = C2
-- -- instance Count C2 where ecountof _ = 2
-- -- data C5500 = C5500
-- -- instance Count C5500 where ecountof _ = 5500

-- foo x = do
--   print $ pp x
--   print $ pp $ runEval x

-- f1 = func "myfunc" $ \(a :: E Int) -> a + a
-- f2 = func "myfunc2" $ \(a :: E Int, b) -> a + cast b
-- f3 = func "myfunc3" $ \(a, b :: E Float) -> f1 a - f2 (a, b)
-- f4 = extern "extfunc1" :: E Float -> E Double
-- f5 = extern "extfunc2" :: (E Int, E Float) -> E Double

-- fpint = func "fastpowint" $ \(a, b :: E Int) -> fastpow a b

-- main :: IO ()
-- main = do
--   -- print tt
--   -- foo $ fastpow 2 (3 :: E Int)
--   -- foo $ fastpow (3 :: E Int) 2
--   -- foo $ dbl (dbl (2 :: E Double))
--   -- compile $ dbl $ fastpow (var 0 :: E Int) (var 1)
--   -- compile $ dbl (dbl (2 :: E Double))
--   let
--     t = compile "mymodule"
--       [
--         -- def fpint,
--         -- def $ func "fastpowint2" $ dbl . fpint
-- --        def $ func "foo" $ \(u0 :: E Word) -> switch u0 [7,8] (9 :: E Int)
--         def $ func "foo" $ \(u0 :: E Word) ->
--           if' (u0 > 42) 4 (if' (u0 < 10) (13 :: E Int) 5),
--         def $ func "bar" $ \(u0 :: E Int) ->
--           if' (u0 > 42) 4 (if' (u0 < 10) (13 :: E Int) 5)
--         -- def $ func "foo" $ \(u0 :: E Int) ->
--         -- if' (u0 > 42) 4 (if' (u0 < 10) (13 :: E Int) 5)
--         -- def $ func "foo" $ \(u1 :: E Int, u2 :: E Int) ->
--         -- if' (u1 > u2) (if' (u1 < 3) (13 :: E Int) 42) 5
--          -- if' ((u1 + u2) > 3)
--          --   (if' (u1 < 12)
--          -- (u1 + 4) u2)
--          --   (if' (u2 > 42) 42 (if' (u1 < 12) (u1 + u2) (u1 + 4)))
--                                                         -- , def f1
--                                                         -- , def f2
--                                                         -- , def f3
--                                                         -- , def f4
--                                                         -- , def f5
--       ]
--   -- printC t
--   printPP t
--   print $ smtEval t
  
-- -- main = compile $
-- --   spctMain C1
--   -- let arr :: E (V C4 Int) = vec [5 .. ] in
--   -- (vfold (+) (var 0) arr)
--   -- (ex (vmap (+ 1) arr) (var 0))
--   -- (ex arr (var 0))
--   -- doesn't work (ex (ex (vec (repeat $ vec [5 .. ]) :: E (V C4 (V C4 Int))) (var 0)) (var 0))
--   -- (ex (var 0) (vec [0 .. ] :: E (V C16 Int)))
--   -- evalA 1 (var 0)
--   -- ((snd $ fkMain (var 0)) :: E Int)
--   -- factorial (var 0 :: E Word64)
--   -- fkFlip $ fst $ nextPerm $ nextPerm perm0
--   -- fst $ nextPerm $ nextPerm perm0
--   -- fst perm0
--   --updix list0 (var 0) (+ 3)
--   -- setix list0 (var 0) 2
--   -- getix list0 (var 0)
--   -- rotate list0 (var 0)
-- --  (fannkuchredux ([4,2,1,5,3] :: [E Word]) :: E Word)
-- --  nbody (var 0)
-- --  sqrt (var 0 :: E Double)
-- --  (+) (var 0 :: E Double) (var 0 :: E Double)
-- --  max (var 0 :: E Int) (var 1)
-- --  fastpow (var 0 :: E Int) (var 1 :: E Word)
