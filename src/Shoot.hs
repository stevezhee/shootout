{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE FlexibleContexts #-}

module Shoot
  ( module Prelude
  , module Shoot
  , module Typed
  )

where

import Prelude hiding ((==), (/=), (>), (<), (>=), (<=), (+), (-), (*), (/), (%), (^), abs, fromInteger, fromRational, pi, sqrt, negate, sum, succ, pred)
import Typed
import qualified Prelude as P

instance Arith Int where arithRec = arithRecIntegral
instance Arith Int' where arithRec = arithRecAtom
instance Arith Word' where arithRec = arithRecAtom
instance Arith Word32' where arithRec = arithRecAtom
instance Arith Word64' where arithRec = arithRecAtom
instance Arith Double' where arithRec = arithRecAtom
instance FP Double' where fpRec = fpRecAtom

instance Cmp Int' Bool' where cmpRec = cmpRecAtom
instance Cmp Word' Bool' where cmpRec = cmpRecAtom
instance Cmp Word32' Bool' where cmpRec = cmpRecAtom
instance Cmp Word64' Bool' where cmpRec = cmpRecAtom

arithRecAtom :: (Atom a, Agg a) => ArithRec a
arithRecAtom = ArithRec
  (lit . fromIntegral) (binop "add") (binop "sub") (binop "mul") (binop "div") (binop "rem") (extern "abs")

cmpRecAtom :: (Atom a, Agg a) => CmpRec a Bool'
cmpRecAtom = CmpRec (binop "eq") (binop "ne") (binop "gt") (binop "lt") (binop "gte") (binop "lte")
  
fpRecAtom :: (Atom a, Agg a) => FPRec a
fpRecAtom = FPRec lit (extern "sqrt")

binop :: (Atom c, Agg a, Agg b) => String -> a -> b -> c
binop s x y = extern s (x,y)

arithRecIntegral :: Integral a => ArithRec a
arithRecIntegral = ArithRec (P.fromInteger) (P.+) (P.-) (P.*) (P.div) (P.rem) (P.abs)

bitsRecAtom :: (Atom a, Agg a) => BitsRec a
bitsRecAtom = BitsRec (binop "shl") (binop "shr") (binop "band") (binop "bor") (binop "xor")

instance Bits Word' where bitsRec = bitsRecAtom
instance Bits Word32' where bitsRec = bitsRecAtom
instance Bits Word64' where bitsRec = bitsRecAtom

data BitsRec a = BitsRec
  { _shl :: a -> a -> a
  , _shr :: a -> a -> a
  , _band :: a -> a -> a
  , _bor :: a -> a -> a
  , _xor :: a -> a -> a
  }

class Bits a where bitsRec :: BitsRec a

shl :: Bits a => a -> a -> a
shl = _shl bitsRec

shr :: Bits a => a -> a -> a
shr = _shr bitsRec

band :: Bits a => a -> a -> a
band = _band bitsRec

bor :: Bits a => a -> a -> a
bor = _bor bitsRec

xor :: Bits a => a -> a -> a
xor = _xor bitsRec

data CmpRec a b = CmpRec
  { _eq :: a -> a -> b
  , _ne :: a -> a -> b
  , _gt :: a -> a -> b
  , _lt :: a -> a -> b
  , _gte :: a -> a -> b
  , _lte :: a -> a -> b
  }

class Cmp a b | a -> b where cmpRec :: CmpRec a b

class Arith a where arithRec :: ArithRec a

data ArithRec a = ArithRec
  { _fromInteger :: Integer -> a
  , _add :: a -> a -> a
  , _sub :: a -> a -> a
  , _mul :: a -> a -> a
  , _div :: a -> a -> a
  , _rem :: a -> a -> a
  , _abs :: a -> a
  }

class FP a where fpRec :: FPRec a

pi :: FP a => a
pi = 3.141592653589793 -- BAL: use prelude value of pi?

data FPRec a = FPRec
  { _fromRational :: Rational -> a
  , _sqrt :: a -> a
  }

fromRational :: FP a => Rational -> a
fromRational = _fromRational fpRec

sqrt :: FP a => a -> a
sqrt = _sqrt fpRec

(^) :: (Arith a, Atom a, Agg a) => a -> Int' -> a
(^) = binop "powi"

(+) :: Arith a => a -> a -> a
(+) = _add arithRec

(-) :: Arith a => a -> a -> a
(-) = _sub arithRec
  
(*) :: Arith a => a -> a -> a
(*) = _mul arithRec

(/) :: Arith a => a -> a -> a
(/) = _div arithRec

(%) :: Arith a => a -> a -> a
(%) = _rem arithRec

fromInteger :: Arith a => Integer -> a
fromInteger = _fromInteger arithRec

abs :: Arith a => a -> a
abs = _abs arithRec

negate :: Arith a => a -> a
negate = (*) (-1)

(==) :: Cmp a b => a -> a -> b
(==) = _eq cmpRec

(/=) :: Cmp a b => a -> a -> b
(/=) = _ne cmpRec

(>) :: Cmp a b => a -> a -> b
(>) = _gt cmpRec

(<) :: Cmp a b => a -> a -> b
(<) = _lt cmpRec

(>=) :: Cmp a b => a -> a -> b
(>=) = _gte cmpRec

(<=) :: Cmp a b => a -> a -> b
(<=) = _lte cmpRec

sum :: (Foldable f, Arith a) => f a -> a
sum = foldr (+) 0

reps :: (Agg a, Agg b, Cmp a Bool', Arith a) => a -> b -> (b -> b) -> b
reps n b f = repsi n b $ \_ -> f

repsi :: (Agg a, Agg b, Cmp a Bool', Arith a) => a -> b -> (a -> b -> b) -> b
repsi n b f = while_ (n, b) $ \(i, b) ->
  ( i > 0
  , let i' = pred i in (i', f i' b)
  )

while_ x = snd . while x
  
unfoldi :: (Count c, Atom a, Agg a, Agg b) => (Word' -> b -> (a, b)) -> b -> (b, Array c a)
unfoldi f b0 = repsi (count arr0) (b0, arr0) $ \i (b, arr) -> let (a,b') = f i b in (b', insert arr a i)
  where arr0 = undef

unfoldi_ :: (Count c, Atom a, Agg a) => (Word' -> a) -> Array c a
unfoldi_ f = snd $ unfoldi (\i () -> (f i, ())) ()

repeatc :: (Count c, Atom a, Agg a) => a -> Array c a
repeatc = unfoldi_ . const

succ :: Arith a => a -> a
succ = (+) 1

pred :: Arith a => a -> a
pred = (+) (-1)

max' :: (Arith a, Agg a, Cmp a Bool') => a -> a -> a
max' x y = if' (x > y) x y

min' :: (Arith a, Agg a, Cmp a Bool') => a -> a -> a
min' x y = if' (x < y) x y

ord' :: Char -> Word8'
ord' = lit . fromIntegral . fromEnum
