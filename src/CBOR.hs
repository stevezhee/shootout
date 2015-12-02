{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module CBOR where

import Prelude ((>>), ($), print, return, IO, Float, Int, Double, (.), Functor(..), Applicative(..), Monad(..), undefined, Bool, Num, fst, snd, flip, Enum)

import Control.Monad (foldM)
import Data.Word
import Data.Int
import Typed
import Control.Applicative
import Data.Foldable (foldr)
-- import Data.Traversable
import Data.Functor (void)
import Control.Monad.State

type M a = StateT (E Word32) IO a

newtype IO' a = IO'{ unIO' :: World -> (a, World) }

data Ptr a

instance Typed a => Typed (Ptr a)

alloc :: Typed a => IO' (E (Ptr a))
alloc = mkIO' $ extern "ffi_alloc" ()

load :: Typed a => E (Ptr a) -> IO' (E a)
load x = mkIO' $ extern "load" x

store :: E a -> E (Ptr a) -> IO' ()
store a p = undefined

-- connect :: IO' (E Socket)
-- connect = IO' $ \w -> alloc $ \p -> let w' = f (p, w) in (load p w', w')
--  where f =

connect :: IO' (E Socket)
connect = mkIO' $ ffi_connect ()

ffi_connect = extern "ffi_connect" :: () -> E Socket

recv :: Count c => E Socket -> E Word -> IO' (E Word, E (V c Word8))
recv sock n = do
  p <- alloc
  m <- mkIO' $ ffi_recv (sock, n, p)
  a <- load p
  return (m, a)
  
ffi_recv  :: Count c => (E Socket, E Word, E (Ptr (V c Word8))) -> E Word
ffi_recv = extern "ffi_recv"

new :: Typed a => E a -> IO' (E (Ptr a))
new a = alloc >>= \p -> store a p >> return p

send :: Count c => E Socket -> E Word -> E (V c Word8) -> IO' (E Word)
send sock n a = do
  p <- new a
  mkIO' $ ffi_send (sock, n, p)
  
ffi_send :: Count c => (E Socket, E Word, E (Ptr (V c Word8))) -> E Word
ffi_send = extern "ffi_send"

ffi_accept :: () -> E Socket
ffi_accept = extern "ffi_accept"

mkIO' a = IO' $ \w -> (a, succ w)

type Socket = Word
type World = E Word

evalIO' :: IO' a -> (World -> a)
evalIO' m = fst . unIO' m
  
instance Applicative IO' where
  pure  = IO' . (,)
  m <*> n = IO' $ \w -> let (f, w') = unIO' m w in let (a, w'') = unIO' n w' in (f a, w'')
    
instance Functor IO' where
  fmap f m = IO' $ \w -> let (a, w') = unIO' m w in (f a, w')
    
instance Monad IO' where
  m >>= f = IO' $ \w -> let (a, w') = unIO' m w in let IO' g = f a in g w'
    
blarg :: (Count c, Agg b) => V c a -> b -> (E Word -> b -> b) -> b
blarg arr x f = snd $ while (0, x) $ \(i, a) -> (i < count arr, (succ i, f i a))

traverse_' :: (Agg b, Agg a, Count c) => (a -> b -> b) -> V c a -> b -> b
traverse_' f arr st0 = blarg arr st0 $ \i st -> f (extract arr i) st

unfold :: Count c => M a -> M (V c a)
unfold = undefined

unfold' :: (Count c, Typed a, Agg a) => (E Word -> V c a -> a) -> V c a
unfold' (f :: (E Word -> V c a -> a)) = blarg arr0 arr0 $ \i arr -> insert arr (f i arr, i)
  where
    arr0 :: V c a = undefined

traverse_ :: (a -> M ()) -> V c a -> M ()
traverse_ f arr = undefined

(^) = undefined

put8 :: E Word8 -> M ()
put8 = undefined

-- put8' :: E Word8 -> M ()
put8' :: (Count c) =>
  E Word8 -> (V c (E Word8) -> World -> World) -> (V c (E Word8), (E Word, World)) ->
  (V c (E Word8), (E Word, World))
put8' x f (buf, (i, world)) =
  (buf', if' (i' == count buf) (0, f buf' world) (i', world))
  where
    buf' = insert buf (x,i)
    i' = succ i
-- BAL: send the total message
    
get8 :: M (E Word8)
get8 = undefined

get8' :: (Count c) =>
  (World -> (V c (E Word8), World)) -> (E Word, (V c (E Word8), World)) ->
  (E Word8, (E Word, (V c (E Word8), World)))

get8' f (i, (buf, world)) =
  if' (i == count buf) (get8' f (0, f world)) (extract buf i, (succ i, (buf, world)))
-- if idx = count buf
--   idx = 0
--   recv buf on sock
-- return buf[idx]
-- BAL: get the total message size at the start

put8s :: (Typed a, Arith a, Num a, Enum a) => a -> E a -> M ()
put8s n x = mapM_ (put8 . cast . ashr x . lit) [ n - 8, n - 16 .. 8 ] >> put8 (cast x)

put16 :: E Word16 -> M ()
put16 = put8s 16

put32 :: E Word32 -> M ()
put32 = put8s 32

put64 :: E Word64 -> M ()
put64 = put8s 64

get16 :: M (E Word16)
get16 = get8s 16

get32 :: M (E Word32)
get32 = get8s 32

get64 :: M (E Word64)
get64 = get8s 64

get8s :: (Typed a, Arith a, Num a, Enum a) => a -> M (E a)
get8s n =
  foldr bor <$> f <*> sequence [ flip shl (lit i) <$> f | i <- [ n - 8, n - 16 .. 8 ] ]
  where f = cast <$> get8

class CBOR a where
  enc :: a -> M ()
  dec :: M a

majMin :: E Word8 -> E Word8 -> M ()
majMin n m = put8 ((n `shl` 5) `bor` m)

getMajMin :: M (E Word8, E Word8)
getMajMin = do
  x <- get8
  return (x `ashr` 5, 0x1f `band` x)

getMin :: M (E Word8)
getMin = snd <$> getMajMin

dec8 :: E Word8 -> M (E Word8)
dec8 n = decN n [get8]

dec16 :: E Word8 -> M (E Word16)
dec16 n = decN n [cast <$> get8, get16]

dec32 :: E Word8 -> M (E Word32)
dec32 n = decN n [cast <$> get8, cast <$> get16, get32]

dec64 :: E Word8 -> M (E Word64)
dec64 n = decN n [cast <$> get8, cast <$> get16, cast <$> get32, get64]

ifM x y z = if' x <$> y <*> z

enc8 :: E Word8 -> E Word8 -> M ()
enc8 n x = ifM (x >= 24) (majMin n 24 >> put8 x) (majMin n x)

enc16 :: E Word8 -> E Word16 -> M ()
enc16 n x = ifM (x >= 2^8) (majMin n 25 >> put16 x) (enc8 n (cast x))

enc32 :: E Word8 -> E Word32 -> M ()
enc32 n x = ifM (x >= 2^16) (majMin n 26 >> put32 x) (enc16 n (cast x))

enc64 :: E Word8 -> E Word64 -> M ()
enc64 n x = ifM (x >= 2^32) (majMin n 27 >> put64 x) (enc32 n (cast x))

encInt :: (Typed a, Typed b, Arith a, Num a) =>
  (E Word8 -> E b -> M ()) -> E a -> M ()
encInt (f :: E Word8 -> E b -> M ()) x =
  ifM (x >= 0) (f 0 ((cast x) :: E b)) (f 1 (cast (negate (x + 1))))

encWord :: Num a => (E Word8 -> E a -> M ()) -> E a -> M ()
encWord f x = f 0 x

switchM x ys z = switch x <$> sequence ys <*> z

decN :: Typed a => E Word8 -> [M (E a)] -> M (E a)
decN n xs = switchM (cast (n - 24)) xs (return $ cast n)

decWord :: (E Word8 -> M a) -> M a
decWord f = getMin >>= f

decInt :: (Num a, Typed a, Typed b, Arith a) => (E Word8 -> M (E a)) -> M (E b)
decInt f = do
  (maj, min) <- getMajMin
  x <- f min
  return $ if' (maj == 0) (cast x) (cast $ negate x - 1)

instance CBOR (E Float) where
  enc x = majMin 7 26 >> put32 (bitcast x)
  dec = getMin >> bitcast <$> get32
  
instance CBOR (E Double) where
  enc x = majMin 7 27 >> put64 (bitcast x)
  dec = getMin >> bitcast <$> get64
  
instance CBOR () where
  enc _ = majMin 7 22
  dec = void getMin
  
instance CBOR (E Bool) where
  enc x = majMin 7 (if' x 21 20)
  dec = ((==) 21) <$> getMin
    
instance CBOR (E Word8) where
  enc = encWord enc8
  dec = decWord dec8

instance CBOR (E Word16) where
  enc = encWord enc16
  dec = decWord dec16

instance CBOR (E Word32) where
  enc = encWord enc32
  dec = decWord dec32

instance CBOR (E Word64) where
  enc = encWord enc64
  dec = decWord dec64
  
instance CBOR (E Int8) where
  enc = encInt enc8
  dec = decInt dec8
  
instance CBOR (E Int16) where
  enc = encInt enc8
  dec = decInt dec8

instance CBOR (E Int32) where
  enc = encInt enc16
  dec = decInt dec16

instance CBOR (E Int64) where
  enc = encInt enc32
  dec = decInt dec32

encV x f arr = enc32 x (cast $ count arr) >> traverse_ f arr

instance (CBOR a, CBOR b, Count c) => CBOR (V c (a, b)) where
  enc = encV 5 (\(a,b) -> enc a >> enc b)
  dec = unfold ((,) <$> dec <*> dec)
  
instance (CBOR a, Count c) => CBOR (V c a) where
  enc = encV 4 enc
  dec = unfold dec
  
instance (Count c) => CBOR (V c (E Word8)) where
  enc = encV 2 put8
  dec = unfold get8
    
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

-- next 5 bits
-- -------
-- (0-23) if it fits
-- 24 means additional uint8_t
-- 25 means additional uint16_t
-- 26 means additional uint32_t
-- 27 means additional uint64_t
  
-- byte string == vector max char
-- utf-8 string == vector max char
-- array == vector max a | tuples | records | sums
-- (key, value) pairs = vector max (a,b)
