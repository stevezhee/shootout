{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

module Spectral where

import Shoot

-- -- spectral-norm
-- type EvalF = Word' -> Word' -> Double'

-- -- evalA :: Word' -> Word' -> Double'
-- -- evalA i0 j0 = 1.0/((i+j)*(i+j+1)/2+i+1) where (i,j) = (tofp i0, tofp j0)

-- -- evalATimesUF :: Count c => (EvalF -> EvalF) -> V c Double' -> V c Double'
-- -- evalATimesUF f u = vunfoldi_ $ \i -> (vfoldi (\j b a -> b + (f evalA) j i * a) 0 u)

-- -- evalATimesU :: Count c => V c Double' -> V c Double'
-- -- evalATimesU = evalATimesUF id

-- -- evalAtTimesU :: Count c => V c Double' -> V c Double'
-- -- evalAtTimesU = evalATimesUF flip

-- -- evalAtATimesU :: Count c => V c Double' -> V c Double'
-- -- evalAtATimesU = evalAtTimesU . evalATimesU

-- -- spctMain :: Count c => c -> Double'
-- -- spctMain (c :: c) = sqrt(vBv/vv)
-- --   where
-- --     (u, v) = reps 10 (vrepeat 1 :: V c Double', undef) $ \(u,_) ->
-- --              let v = evalAtATimesU u in (v, evalAtATimesU v)
-- --     (vBv,vv) = vfoldi  (\i (vBv, vv) vi -> (u `ex` i * vi, vi^2)) (0,0) v

-- vunfoldi :: (Count c) =>
--   (Word' -> b -> (a, b)) -> b -> V c a
-- vunfoldi f b = snd $ while ((0, b), undef) $ \((i, b), v) ->
--   ( i `lt` countof v
--   , let (a,b') = f i b in ((i + 1, b'), ins v (a, i))
--   )

-- vunfoldi_ :: (Count c) => (Word' -> a) -> V c a
-- vunfoldi_ f = vunfoldi (\i _ -> (f i, b)) b
--   where b :: Word' = undef -- BAL: b = undefined this should work, but doesn't.  being too strict somewhere unused "vunfoldi_"

-- vrepeat :: (Count c) => a -> V c a
-- vrepeat = vunfoldi_ . const

foldi :: (Count c, Atom a, Agg a, Agg b) =>
  (Word' -> b -> a -> b) -> b -> Array c a -> b
foldi f x0 arr = repsi (count arr) x0 $ \i x -> f i x $ extract arr i

-- BAL: tickles bug
spctMain :: Count c => c -> Double'
spctMain (c :: c) = foldi (\i _ -> (*) (u `extract` i)) 0 v
  where
    (u, v) = (repeatc 3 :: Array c Double', repeatc 2 :: Array c Double')
