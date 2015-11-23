{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module CBOR where

import Prelude ((>>), ($), print, return, IO, Float, Int, Double, (.), Functor(..), Applicative(..), Monad(..), undefined, Bool, Num, snd)

import Data.Word
import Data.Int
import Typed
import Control.Applicative
import Data.Foldable
import Data.Functor (void)

data M a
instance Functor M
instance Applicative M
instance Monad M
instance Agg a => Agg (M a)

class CBOR a where
  enc :: a -> M ()
  dec :: M a

-- next 5 bits
-- -------
-- (0-23) if it fits
-- 24 means additional uint8_t
-- 25 means additional uint16_t
-- 26 means additional uint32_t
-- 27 means additional uint64_t

(^) = undefined

majmin :: E Word8 -> E Word8 -> M ()
majmin n m = put8 ((n `shl` 5) `bor` m)

put8 :: E Word8 -> M ()
put8 = undefined

put16 :: E Word16 -> M ()
put16 = undefined

put32 :: E Word32 -> M ()
put32 = undefined

put64 :: E Word64 -> M ()
put64 = undefined

getMin :: M (E Word8)
getMin = undefined

get8 :: M (E Word8)
get8 = undefined

get16 :: M (E Word16)
get16 = undefined

get32 :: M (E Word32)
get32 = undefined

get64 :: M (E Word64)
get64 = undefined

decWord n xs = switch (cast (n - 24)) xs (return $ cast n)

dec8 :: E Word8 -> M (E Word8)
dec8 n = decWord n [get8]

dec16 :: E Word8 -> M (E Word16)
dec16 n = decWord n [cast <$> get8, get16]

dec32 :: E Word8 -> M (E Word32)
dec32 n = decWord n [cast <$> get8, cast <$> get16, get32]

dec64 :: E Word8 -> M (E Word64)
dec64 n = decWord n [cast <$> get8, cast <$> get16, cast <$> get32, get64]
  
enc8 :: E Word8 -> E Word8 -> M ()
enc8 n x = if' (x >= 24) (majmin n 24 >> put8 x) (majmin n x)

enc16 :: E Word8 -> E Word16 -> M ()
enc16 n x = if' (x >= 2^8) (majmin n 25 >> put16 x) (enc8 n (cast x))

enc32 :: E Word8 -> E Word32 -> M ()
enc32 n x = if' (x >= 2^16) (majmin n 26 >> put32 x) (enc16 n (cast x))

enc64 :: E Word8 -> E Word64 -> M ()
enc64 n x = if' (x >= 2^32) (majmin n 27 >> put64 x) (enc32 n (cast x))

encInt :: (Typed a, Typed b, Arith a, Num a) =>
  (E Word8 -> E b -> M ()) -> E a -> M ()
encInt (f :: E Word8 -> E b -> M ()) x =
  if' (x >= 0) (f 0 ((cast x) :: E b)) (f 1 (cast (negate (x + 1))))

encWord f x = f 0 x

instance CBOR (E Word8) where
  enc = encWord enc8
--  dec = dec8
  
-- instance CBOR (E Int8) where
--   enc = encInt enc8

-- instance CBOR (E Word16) where
--   enc = encWord enc16

-- instance CBOR (E Word32) where
--   enc = encWord enc32

-- instance CBOR (E Word64) where
--   enc = encWord enc64

-- instance CBOR (E Int16) where
--   enc = encInt enc8

-- instance CBOR (E Int32) where
--   enc = encInt enc16

-- instance CBOR (E Int64) where
--   enc = encInt enc32

instance Foldable (V c)

instance Count (V c a)

encV x f arr = enc32 x (cast $ count arr) >> traverse_ f arr

-- instance (CBOR (E a), CBOR (E b), Count c) => CBOR (V c (E a, E b)) where
--   enc = encV 5 (\(a,b) -> enc a >> enc b)

-- instance (CBOR (E a), Count c) => CBOR (V c (E a)) where
--   enc = encV 4 enc

-- instance (Count c) => CBOR (V c (E Word8)) where
--   enc = encV 2 put8

-- -- 3 bits
-- -- -----
-- -- 0 = unsigned integer
-- -- 1 = negative integer (-1 * (encoding for unsigned integer - 1), -500 is encoded as 499)
-- -- 2 = byte string, next is length encoded as an unsigned integer
-- -- 3 = utf-8 string, same as byte string (major type 2)
-- -- 4 = array, length as in byte string is number of data items, items are each encoded individually
-- -- 5 = map of (key,value) pairs, # of pairs is same as byte string length, duplicate keys are not valid even though they are well-formed
-- -- 6 = optional tagging - positive integer tag followed by single data item
-- -- 7 = floating point and no content (void, unit)
-- --     0..23        Simple value (value 0..23)
-- --           0..19    (Unassigned)    
-- --           20       False           
-- --           21       True            
-- --           22       Null            
-- --           23       Undefined value 
-- --      24           Simple value in following byte (value 32..255 in following byte)   
-- --         (32 .. 255) unassigned

-- --      25           IEEE 754 Half-Precision Float (16 bits follow)   
-- --      26           IEEE 754 Single-Precision Float (32 bits follow) 
-- --      27           IEEE 754 Double-Precision Float (64 bits follow) 
-- --      28-30        (Unassigned)                                     
-- --      31           "break" stop code for indefinite-length items

instance CBOR (E Float) where
  enc x = majmin 7 26 >> put32 (bitcast x)
  dec = getMin >> bitcast <$> get32
  
instance CBOR (E Double) where
  enc x = majmin 7 27 >> put64 (bitcast x)
  dec = getMin >> bitcast <$> get64
  
instance CBOR () where
  enc _ = majmin 7 22
  dec = void getMin
  
instance CBOR (E Bool) where
  enc x = majmin 7 (if' x 21 20)
  dec = ((==) 21) <$> getMin
  
-- -- byte string == vector max char
-- -- utf-8 string == vector max char
-- -- array == vector max a | tuples | records | sums
-- -- (key, value) pairs = vector max (a,b)
