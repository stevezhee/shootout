{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

module Spectral where

import Shoot

-- spectral-norm
type EvalF = Word' -> Word' -> Double'

evalA :: Word' -> Word' -> Double'
evalA i0 j0 = 1.0/((i+j)*(i+j+1)/2+i+1) where (i,j) = (wtod i0, wtod j0)

evalATimesUF :: Count c => (EvalF -> EvalF) -> Array c Double' -> Array c Double'
evalATimesUF f u = unfoldi_ $ \i -> (foldi (\j b a -> b + (f evalA) j i * a) 0 u)

evalATimesU :: Count c => Array c Double' -> Array c Double'
evalATimesU = evalATimesUF id

evalAtTimesU :: Count c => Array c Double' -> Array c Double'
evalAtTimesU = evalATimesUF flip

evalAtATimesU :: Count c => Array c Double' -> Array c Double'
evalAtATimesU = evalAtTimesU . evalATimesU

spctMain :: Count c => c -> Double'
spctMain (c :: c) = sqrt(vBv/vv)
  where
    (u, v) = reps 10 (repeatc 1 :: Array c Double', undef) $ \(u,_) ->
             let v = evalAtATimesU u in (v, evalAtATimesU v)
    (vBv,vv) = foldi  (\i (vBv, vv) vi -> (u `extract` i * vi, vi^2)) (0,0) v

-- vunfoldi :: (Count c) =>
--   (Word' -> b -> (a, b)) -> b -> Array c a
-- vunfoldi f b = snd $ while ((0, b), undef) $ \((i, b), v) ->
--   ( i `lt` countof v
--   , let (a,b') = f i b in ((i + 1, b'), ins v (a, i))
--   )

-- vunfoldi_ :: (Count c) => (Word' -> a) -> Array c a
-- vunfoldi_ f = vunfoldi (\i _ -> (f i, b)) b
--   where b :: Word' = undef -- BAL: b = undefined this should work, but doesn't.  being too strict somewhere unused "vunfoldi_"

-- vrepeat :: (Count c) => a -> Array c a
-- vrepeat = vunfoldi_ . const

-- BAL: tickles bug
-- spctMain :: Count c => c -> Double'
-- spctMain (c :: c) = foldi (\i _ -> (*) (u `extract` i)) 0 v
--   where
--     (u, v) = (repeatc 3 :: Array c Double', repeatc 2 :: Array c Double')
