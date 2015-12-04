{-# LANGUAGE ScopedTypeVariables #-}

module Typed
  ( module Typed,
    PP(..),
    Tree(..),
    compile,
    unused,
    printPP
  )
where

import Data.Int
import Data.Word
import Untyped hiding (switch, undef, instantiate)
import qualified Untyped as U

data AggRec a = AggRec
  { _agg :: Tree Exp -> a
  , _unAgg :: a -> Tree Exp
  , _typeofAgg :: a -> Tree Type
  }

data AtomRec a = AtomRec
  { _atom :: Exp -> a
  , _unAtom :: a -> Exp
  , _typeofAtom :: a -> Type
  }

class Agg a where aggRec :: AggRec a

agg :: Agg a => Tree Exp -> a
agg = _agg aggRec

unAgg :: Agg a => a -> Tree Exp
unAgg = _unAgg aggRec

typeofAgg :: Agg a => a -> Tree Type
typeofAgg = _typeofAgg aggRec

fromAtomRec :: Atom a => AggRec a
fromAtomRec = AggRec{ _agg = atom . fromLeaf, _unAgg = Leaf . unAtom, _typeofAgg = Leaf . typeofAtom }

newtype Bool' = Bool'{ unBool' :: Exp }
instance Atom Bool' where atomRec = AtomRec Bool' unBool' $ \_ -> booltype
instance Agg Bool' where aggRec = fromAtomRec

newtype Word' = Word'{ unWord' :: Exp }
instance Atom Word' where atomRec = AtomRec Word' unWord' $ \_ -> wtype 32 -- BAL: make type architecture dependent
instance Agg Word' where aggRec = fromAtomRec

newtype Word8' = Word8'{ unWord8' :: Exp }
instance Atom Word8' where atomRec = AtomRec Word8' unWord8' $ \_ -> wtype 8
instance Agg Word8' where aggRec = fromAtomRec

newtype Word64' = Word64'{ unWord64' :: Exp }
instance Atom Word64' where atomRec = AtomRec Word64' unWord64' $ \_ -> wtype 64
instance Agg Word64' where aggRec = fromAtomRec

newtype Int' = Int'{ unInt' :: Exp }
instance Atom Int' where atomRec = AtomRec Int' unInt' $ \_ -> itype 32 -- BAL: make type architecture dependent
instance Agg Int' where aggRec = fromAtomRec

newtype Word32' = Word32'{ unWord32' :: Exp }
instance Atom Word32' where atomRec = AtomRec Word32' unWord32' $ \_ -> wtype 32
instance Agg Word32' where aggRec = fromAtomRec

newtype Double' = Double'{ unDouble' :: Exp }
instance Agg Double' where aggRec = fromAtomRec
instance Atom Double' where atomRec = AtomRec Double' unDouble' $ \_ -> TFloating 64

class Atom a where atomRec :: AtomRec a
  
atom :: Atom a => Exp -> a
atom = _atom atomRec

unAtom :: Atom a => a -> Exp
unAtom = _unAtom atomRec

typeofAtom :: Atom a => a -> Type
typeofAtom = _typeofAtom atomRec

func :: (Agg a, Atom b) => String -> (a -> b) -> a -> b
func s f = mkdefn s (Just . unAtom . f)

mkdefn :: (Atom a, Agg b) => String -> (b -> Maybe Untyped.Exp) -> b -> a
mkdefn s f = \a -> let v = atom $ U.defn s (typeofAtom v) (f a) $ unAgg a in v
           
newtype World = World{ unWorld :: Exp }
instance Atom World where atomRec = AtomRec World unWorld $ \_ -> TWorld
instance Agg World where aggRec = fromAtomRec

proc :: Agg a => String -> (a -> IO' ()) -> (a -> IO' ())
proc s f = \a0 -> IO' $ \w0 -> ((), (func s $ \(a, w) -> snd (unIO' (f a) w)) (a0, w0))

extern :: (Agg a, Atom b) => String -> a -> b
extern s = mkdefn s (\_ -> Nothing)

externIO :: Agg a => String -> a -> IO' ()
externIO s = \(a0 :: a) -> IO' $ \w0 -> ((), (extern s :: (a, World) -> World) (a0, w0))

defIO :: (Agg a) => (a -> IO' ()) -> Def
defIO f = def $ \(a, w) -> snd (unIO' (f a) w)

def :: (Agg a, Atom b) => (a -> b) -> Def
def f = case unExp $ unAtom $ f instantiate of
  Right (App a _) | body a /= Nothing -> a
  _ -> uerr "unable to create definition (not a definition)"

execIO' :: IO' () -> World -> World
execIO' m = snd . unIO' m

newtype IO' a = IO'{ unIO' :: World -> (a, World) }

instance Applicative IO' where
  pure  = IO' . (,)
  m <*> n = IO' $ \w -> let (f, w') = unIO' m w in let (a, w'') = unIO' n w' in (f a, w'')
    
instance Functor IO' where
  fmap f m = IO' $ \w -> let (a, w') = unIO' m w in (f a, w')
    
instance Monad IO' where
  m >>= f = IO' $ \w -> let (a, w') = unIO' m w in let IO' g = f a in g w'

uerr s = error $ "user error:" ++ s

instantiate :: Agg a => a
instantiate = let v = agg $ fmap var $ U.instantiate $ typeofAgg v in v

switch :: (Agg a) => Word' -> [a] -> a -> a
switch a bs c = agg $ U.switch (unWord' a) (map unAgg bs) (unAgg c)

while :: Agg a => a -> (a -> (Bool', a)) -> a
while x f = agg $ U.while (unAgg x) g
  where g = \bs -> let (a, b) = f (agg bs) in (unBool' a, unAgg b)

if' :: Agg a => Bool' -> a -> a -> a
if' a b c = switch (zext a) [c] b

wtod :: Word' -> Double'
wtod = uitofp

uitofp :: (Atom a, Agg a, Atom b) => a -> b -- a is word and b is floating
uitofp = extern "uitofp"

zext :: (Atom a, Agg a, Atom b) => a -> b -- bits of b > bits of a
zext = extern "zext"

insert :: (Count c, Atom a, Agg a) => Array c a -> a -> Word' -> Array c a
insert arr a i = extern "insert" (arr, a, i)

extract :: (Count c, Atom a) => Array c a -> Word' -> a
extract arr i = extern "extract" (arr, i)

instance (Count c, Atom a) => Agg (Array c a) where aggRec = fromAtomRec
instance (Count c, Atom a) => Atom (Array c a) where
  atomRec = AtomRec Array unArray $ \(x :: Array c a) ->
    TArray (countof x) $ typeofAtom (unused "Atom:array:type" :: a)

instance Count c => Count (Array c a) where
  countof (_ :: Array c a) = countof (unused "Count:array" :: c)

data Array c a = Array{ unArray :: Exp } -- BAL: c must be a count, a must be an atom

class Count c where countof :: c -> Integer

lit :: Atom a => Rational -> a
lit x = let v = atom $ rat (typeofAtom v) x in v

false :: Bool'
false = lit 0

true :: Bool'
true = lit 1

count :: Count c => c -> Word'
count = lit . fromIntegral . countof

-- assert s b a = if b then a else error $ "assert:" ++ s

undef :: (Atom a, Agg a) => a
undef = let v = atom $ U.undef $ typeofAtom v in v

-- vec :: (Count c, Typed a) => [E a] -> E (V c a)
-- vec (xs :: [E a]) = f (unused "vec")
--   where
--     f :: (Count c, Typed a) => c -> E (V c a)
--     f c = assert "vec:length mismatch" (not (null bs) && length bs == cnt) $
--           foldl' ins undef $ zip bs [0 .. ]
--       cnt = fromIntegral $ ecountof c
--       bs = take cnt xs

-- vupd :: (Count c, Typed a) => E (V c a) -> (E a -> E a, E Word) -> E (V c a)
-- vupd x (f, z) = vupdi x (\_ -> f, z)

-- vupdi :: (Count c, Typed a) => E (V c a) ->
--   (E Word -> E a -> E a, E Word) -> E (V c a)
-- vupdi x (f, z) = ins x (f z $ ex x z, z)

-- vmap :: (Count c, Typed a) => (E a -> E a) -> E (V c a) -> E (V c a)
-- vmap f = vmapi $ \_ -> f

-- vmapi :: (Count c, Typed a) => (E Word -> E a -> E a) -> E (V c a) -> E (V c a)
-- vmapi f xs = snd $ while (0, xs) $ \(i, xs) ->
--   ( i `lt` countof xs
--   , (i + 1, vupdi xs (f, i))
--   )

-- vfoldr :: (Count c, Agg a, Agg b) => (a -> b -> b) -> b -> V c a -> b
-- vfoldr f x arr = snd $ while (0, x) $ \(i, b) -> (i < count arr, (succ i, f (extract arr i) b))

-- vfold :: (Count c, Typed a, Agg b) => (b -> E a -> b) -> b -> E (V c a) -> b
-- vfold f = vfoldi $ \_ -> f

instance Agg () where
  aggRec = AggRec
    { _agg = \(Node []) -> ()
    , _unAgg = \() -> Node []
    , _typeofAgg = \_ -> Node []
    }
  
instance (Agg a, Agg b) => Agg (a, b) where
  aggRec = AggRec
    { _agg = \(Node [a,b]) -> (agg a, agg b)
    , _unAgg = \(a,b) -> Node [unAgg a, unAgg b]
    , _typeofAgg = \(_ :: (a,b)) -> Node [ typeofAgg (unused "Agg (a,b)" :: a)
                                         , typeofAgg (unused "Agg (a,b)" :: b) ]
    }
    
instance (Agg a, Agg b, Agg c) => Agg (a, b, c) where
  aggRec = AggRec
    { _agg = \(Node [a,b,c]) -> (agg a, agg b, agg c)
    , _unAgg = \(a,b,c) -> Node [unAgg a, unAgg b, unAgg c]
    , _typeofAgg = \(_ :: (a,b,c)) -> Node [ typeofAgg (unused "Agg (a,b,c)" :: a)
                                           , typeofAgg (unused "Agg (a,b,c)" :: b)
                                           , typeofAgg (unused "Agg (a,b,c)" :: c) ]
    }

instance (Agg a) => Agg [a] where
  aggRec = AggRec
    { _agg = \(Node xs) -> map agg xs
    , _unAgg = Node . map unAgg
    , _typeofAgg = \(_ :: [a]) -> Node [ typeofAgg (unused "Agg [a]" :: a) ]
    }
