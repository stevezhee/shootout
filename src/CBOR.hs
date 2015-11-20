{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module CBOR where

import Data.Word
import Data.Int
import Typed (E, V, Count(..))

raw :: Monad m => Word -> Word64 -> m ()
raw = undefined

major :: Monad m => Word64 -> m ()
major = raw 3
minor :: Monad m => Word64 -> m ()
minor = raw 5

-- next 5 bits
-- -------
-- (0-23) if it fits
-- 24 means additional uint8_t
-- 25 means additional uint16_t
-- 26 means additional uint32_t
-- 27 means additional uint64_t

rawUInt :: Monad m => Word64 -> m ()
rawUInt x
  | x <= 23 = minor x
  | x < 2^8 = minor 24 >> raw 8 x
  | x < 2^16 = minor 25 >> raw 16 x
  | x < 2^32 = minor 26 >> raw 32 x
  | otherwise = minor 27 >> raw 64 x

encUInt :: Monad m => Word64 -> m ()
encUInt x = major 0 >> rawUInt x

encSInt :: Monad m => Int64 -> m ()
encSInt x = if x >= 0 then encUInt (fromIntegral x) else (major 1 >> rawUInt (fromIntegral (negate (x + 1))))

mymapM_ :: Monad m => (E a -> m ()) -> E (V c a) -> m ()
mymapM_ =  undefined

bar :: E Word8 -> m ()
bar = undefined

bytestr :: (Monad m, Count c) => E (V c Word8) -> m ()
bytestr arr = major 2 >> encCount arr >> mymapM_ bar arr

encCount :: (Monad m, Count a) => a -> m ()
encCount = rawUInt . fromIntegral . countof

-- instance (CBOR a) => CBOR (E a)

instance (CBOR (E a), Count c) => CBOR (E (V c a)) where
  enc arr = major 4 >> encCount arr >> mymapM_ enc arr

-- 3 bits
-- -----
-- 0 = unsigned integer
-- 1 = negative integer (-1 * (encoding for unsigned integer - 1), -500 is encoded as 499)
-- 2 = byte string, next is length encoded as an unsigned integer
-- 3 = utf-8 string, same as byte string (major type 2)
-- 4 = array, length as in byte string is number of data items, items are each encoded individually
-- 5 = map of (key,value) pairs, # of pairs is same as byte string length, duplicate keys are not valid even though they are well-formed
-- 6 = optional tagging - positive integer tag followed by single data item
-- 7 = floating point and no content (void, unit)
--     0..23        Simple value (value 0..23)
--           0..19    (Unassigned)    
--           20       False           
--           21       True            
--           22       Null            
--           23       Undefined value 
--      24           Simple value in following byte (value 32..255 in following byte)   
--         (32 .. 255) unassigned

--      25           IEEE 754 Half-Precision Float (16 bits follow)   
--      26           IEEE 754 Single-Precision Float (32 bits follow) 
--      27           IEEE 754 Double-Precision Float (64 bits follow) 
--      28-30        (Unassigned)                                     
--      31           "break" stop code for indefinite-length items


class CBOR a where
  enc :: Monad m => a -> m ()
  dec :: Monad m => m a
  
instance CBOR Word8
instance CBOR Word16
instance CBOR Word32
instance CBOR Word64

instance CBOR Int8
instance CBOR Int16
instance CBOR Int32
instance CBOR Int64

-- byte string == vector max char
-- utf-8 string == vector max char
-- array == vector max a | tuples | records | sums
-- (key, value) pairs = vector max (a,b)
