{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

module Fannkuch where

import Shoot

type V16W4 = Word64'

type Perm = (V16W4, (V16W4, Word64'))

shl4 x v = shl v (4*x)
shr4 x v = shr v (4*x)

maskMerge x y mask = x `xor` ((x `xor` y) `band` mask) -- from graphics.stanford.edu bithacks.html

ix v i = shr4 i v `band` 0xf

setix v i x = maskMerge v (shl4 i x) (shl4 i 0xf)

updix v i f = setix v i $ f $ ix v i

maxV16W4 :: V16W4
maxV16W4 = 0xffffffffffffffff

factorial :: (Agg a, Arith a, Cmp a Bool') => a -> a
factorial n0 = while_ (n0, 1) $ \(n, r) -> (n > 1, (pred n, r * n))

fkRotate :: V16W4 -> Word64' -> V16W4
fkRotate v n = maskMerge (maskMerge v1 v2 (shl4 (n - 1) 0xf)) v mask
  where
    v1 = shr4 1 v
    v2 = shl4 (n - 1) v
    mask = shl4 n maxV16W4

fkReverse :: V16W4 -> V16W4
fkReverse v0 = maskMerge (rev v r) v0 (shl4 n0 maxV16W4)
  where
    n0 = v0 `ix` 0
    rev v r = r `bor` (v `band` 0xf)
    (_, (v,r)) = while (n0, (v0, 0)) $ \(n, (v, r)) ->
      ( n > 1
      , (n - 1, (shr4 1 v, shl4 1 $ rev v r))
      )

fkFlip :: (Arith a, Agg a) => V16W4 -> a
fkFlip v0 = while_ (v0, 0) $ \(v,n) ->
  ( ix v 0 > 1
  , (fkReverse v, n + 1)
  )

fkPerm :: Perm -> Perm
fkPerm pci = (fkRotate p i, (updix c i (+ 1), 2))
  where
  (p, (c, i)) = while pci $ \(p,(c,i)) ->
    ( ix c i >= i
    , (fkRotate p i, (setix c i 1, i + 1))
    )

fkMain :: (Atom a, Agg a, Arith a, Cmp a Bool') => a -> (a, a)
fkMain n =
  fst $ while ((0,0), (factorial n, perm0)) $ \((max_flips, checksum),(n,pci@(p,_))) ->
    ( n > 0
    , let flips_count = fkFlip p in
        ((max' max_flips flips_count, -1 * (checksum + flips_count)), (n - 1, fkPerm pci))
    )
  where
    perm0 = (0xfedcba987654321, (0x1111111111111111, 2))

fannkuchredux :: Int' -> IO' ()
fannkuchredux = proc "fannkuchredux" $ \n -> do
  let (max_flips, checksum) = fkMain n
  puti checksum
  putln
  puti max_flips
  putln
