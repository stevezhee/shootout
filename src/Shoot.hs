{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE ScopedTypeVariables    #-}

-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE RebindableSyntax #-}
-- {-# LANGUAGE FlexibleContexts #-}

module Shoot
  ( module Shoot
  , module Data.Int
  , module Data.Word
  -- , module Prelude
  -- , module Typed
  )

where

import Data.Word
import Data.Int
import GHC.Generics (Generic)
import Control.Monad.State hiding (forever)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List (intersperse)
import Data.Foldable
import Numeric
import Data.Char

concatM :: [M String] -> M String
concatM xs = concat <$> sequence xs
    
unused s = error $ "unused:" ++ s

tyUVal :: UVal -> M Type
tyUVal x = case x of
  Var t _ -> return t
  Lit a -> case a of
    LInt a -> ty $ Lit a
    LBool a -> ty $ Lit a
    LPtr _ t -> return t
    LWord8 a -> ty $ Lit a

data Tree a = Node [Tree a] | Leaf a deriving (Show, Eq, Generic)

instance Foldable Tree where
  foldr f b x = case x of
    Leaf a -> f a b
    Node ns -> foldr (flip (foldr f)) b ns

instance Traversable Tree where
  traverse f x = case x of
    Leaf a -> Leaf <$> f a
    Node ns -> Node <$> traverse (traverse f) ns

instance Functor Tree where
  fmap f x = case x of
    Leaf a -> Leaf $ f a
    Node ns -> Node $ fmap (fmap f) ns

data Proc a b = Proc Name (Maybe (a -> M b))
    
class Ret b where
  call :: Agg a => Proc a b -> a -> M b
  ret :: b -> M b
  tyRet :: b -> M Type

class Agg a where
  unAgg :: Tree UVal -> a
  agg :: a -> M (Tree UVal)
  instantiate :: M a

class Ty a where tyRec :: TyRec a
class Ty a => Lit a where litRec :: LitRec a

instance Lit Int where litRec = undefined

data LitRec a

type P a = Val (Ptr a)

data Ptr a = Ptr{ unPtr :: Int }
  deriving (Show, Generic, Eq, Ord)

type IntT = Val Int
type Int8T = Val Int8
type Word8T = Val Word8

ffi_fld = undefined

lit :: Lit a => a -> Val a
lit = undefined

tyRecFFI = undefined

data Val a = Lit a | Var Type Name deriving Show

type Type = String

data St = St
  { next :: Int
  , strings :: Map String (Val (Ptr (Ptr Int8)))
  , decls :: Set String
  -- , structs :: Map [Type] Type
  } deriving Show
    
type M a = StateT St IO a

type Name = String

ffi' :: Ret b => Name -> M b
ffi' x = ffi x ()

ffi :: (Agg a, Ret b) => Name -> a -> M b
ffi x y = do
  b <- call (Proc x Nothing) y
  declare x y b
  return b

declare :: (Agg a, Ret b) => Name -> a -> b -> M ()
declare n a v = do
  t <- tyRet v
  ts <- tyargs a
  let s = "declare " ++ t ++ " @" ++ n ++ ts
  modify $ \st -> st{ decls = S.insert s $ decls st }

tyargs :: Agg a => a -> M String
tyargs x = ((map tyUVal . toList) <$> agg x) >>= parens

parens :: [M String] -> M String
parens xs = concatM [return "(", commaSep xs, return ")"]

commaSep :: [M String] -> M String
commaSep = concatM . intersperse (return ", ")

load :: Ty a => P a -> M (Val a)
load x = assign $ \_ -> prim_ "load" [pp x]

pp :: Ty a => Val a -> M String
pp x = toUVal x >>= ppUValT

int :: Int -> IntT
int = Lit

ppUValT :: UVal -> M String
ppUValT x = unwordsM [tyUVal x, ppUVal x]
                      
ppUVal :: UVal -> M String
ppUVal x = case x of
  Lit a -> ppULit a
  Var _ s -> return s

ppULit :: ULit -> M String
ppULit x = return $ case x of
  LInt a -> show a
  LWord8 a -> show a
  LBool a -> case a of
    True -> "true"
    False -> "false"
  LPtr a _ -> if a == 0 then "null" else show a
  
type UVal = Val ULit

data ULit
  = LInt{ unLInt :: Int }
  | LWord8{ unLWord8 :: Word8 }
  | LBool{ unLBool :: Bool }
  | LPtr Int Type
  deriving (Show, Generic, Eq, Ord)

data TyRec a = TyRec
  { _ty :: Val a -> M Type
  , _toULit :: a -> M ULit
  , _unULit :: ULit -> a
  }

ty :: Ty a => Val a -> M Type
ty = _ty tyRec

toULit :: Ty a => a -> M ULit
toULit = _toULit tyRec

unULit :: Ty a => ULit -> a
unULit = _unULit tyRec

unwordsM :: [M String] -> M String
unwordsM xs = unwords <$> sequence xs

-- class PP a where ppu :: a -> String

tyRecInt :: (Int -> a) -> (a -> Int) -> TyRec a
tyRecInt f g = tyRecPrim "i32" (LInt . g) (f . unLInt)

tyRecWord8 :: (Word8 -> a) -> (a -> Word8) -> TyRec a
tyRecWord8 f g = tyRecPrim "i8" (LWord8 . g) (f . unLWord8)

tyRecPrim t f g = TyRec
  { _ty = return . const t
  , _toULit = return . f
  , _unULit = g
  }

instance Ty Int where tyRec = tyRecInt id id
instance Ty Word8 where tyRec = tyRecWord8 id id
instance Ty Int8 where tyRec = undefined
instance Ty Bool where tyRec = tyRecPrim "i1" LBool unLBool
instance Ty Char where tyRec = tyRecWord8 (toEnum . fromIntegral) (fromIntegral . fromEnum)

instance Ty a => Ty (Ptr a) where
  tyRec = TyRec
    { _ty = const $ concatM [ ty (unused "ty (Ptr a)" :: Val a), return "*" ]
    , _toULit = \x@(Ptr a) -> LPtr a <$> ty (Lit x)
    , _unULit = \(LPtr a _) -> Ptr a
    }

callM v x y = [ unwordsM [tyRet v, concatM [procName x, args y ] ] ]
retM x = outputM2 $ unwordsM [return "ret", x]

instance Ty a => Ret (Val a) where
  call (Proc x _) y = assign $ \v -> prim_ "call" $ callM v x y
  ret x = retM (pp x) >> return x
  tyRet = ty
  
instance Ret () where
  call (Proc x _) y = prim "call" $ callM () x y
  ret () = retM (return "void")
  tyRet _ = return "void"

fresh :: M Int
fresh = do
  i <- gets next
  modify $ \st -> st{ next = succ i }
  return i

store' :: Ty a => P a -> Val a -> M ()
store' = flip store

store :: Ty a => Val a -> P a -> M ()
store x y = prim "store" [pp x, pp y]

prim :: String -> [M String] -> M ()
prim x = outputM2 . prim_ x

indent :: Int -> M String -> M String
indent n m = concatM [return $ replicate n ' ', m]

outputM :: M String -> M ()
outputM x = x >>= output

outputM2 :: M String -> M ()
outputM2 x = indent 2 x >>= output

output :: String -> M ()
output s = lift (putStrLn s)

prim_ :: String -> [M String] -> M String
prim_ x ys = unwordsM [return x, commaSep ys]

ppu :: Ty a => Val a -> M String
ppu x = toUVal x >>= ppUVal

toUVal :: Ty a => Val a -> M UVal
toUVal x = case x of
  Var a b -> return $ Var a b
  Lit a -> Lit <$> toULit a

br :: Label -> M ()
br x = prim "br" [ppLabel x]

data Label = Label{ unLabel :: Int } deriving Show

ppULabel = (++) "L" . show . unLabel

ppLabel x = return $ unwords ["label", "%" ++ ppULabel x]

alloca' :: Ty a => M (P a)
alloca' = alloca $ int 1

bitcast :: (Ty a, Ty b) => Val a -> M (Val b)
bitcast x = assign $ \v -> prim_ "bitcast" [unwordsM [pp x, return "to", ty v]]

alloca :: Ty a => IntT -> M (P a)
alloca n = assign $ \(_ :: P a) -> prim_ "alloca" [ty (unused "alloca" :: Val a), pp n]

idx :: Ty a => P a -> IntT -> M (P a)
idx (x :: P a) n = assign $ \_ -> prim_ "getelementptr inbounds" [pp x, pp n]

fld :: (Ty a, Ty b) => Int -> P a -> M (P b)
fld n (x :: P a) =
  assign $ \_ -> prim_ "getelementptr inbounds" [pp x, pp $ int 0, pp $ Lit n]

add :: Ty a => Val a -> Val a -> M (Val a)
add x y = assign $ \_ -> prim_ "add" [pp x, ppu y]

eq :: Ty a => Val a -> Val a -> M BoolT
eq x y = assign $ \_ -> prim_ "icmp eq" [pp x, ppu y]

ne :: Ty a => Val a -> Val a -> M BoolT
ne x y = assign $ \_ -> prim_ "icmp ne" [pp x, ppu y]

assign :: Ty a => (Val a -> M String) -> M (Val a)
assign (f :: Val a -> M String) = do
  v <- bvar <$> ty (unused "assign" :: Val a) <*> fresh
  outputM2 $ unwordsM [ppu v, return "=", f v]
  return v

bvar t i = Var t $ "%v" ++ show i

w8 :: Word8 -> Word8T
w8 = Lit

unlinesM :: [M String] -> M String
unlinesM xs = (init . unlines) <$> sequence xs
  
brackets :: [M String] -> M String
brackets xs = unlinesM (return "[" : map (indent 4) xs ++ [indent 4 $ return "]"])

switch' :: Ty a => Val a -> M () -> [(Val a, M ())] -> M ()
switch' x y zs = do
  done <- freshLabel
  dflt <- freshLabel
  lbls <- mapM (const freshLabel) zs
  switch x dflt $ zip (map (unLit . fst) zs) lbls
  sequence_ [ label lbl >> m >> br done | (lbl, m) <- (dflt, y) : zip lbls (map snd zs) ]
  label done
      
switch :: Ty a => Val a -> Label -> [(a, Label)] -> M ()
switch x y zs =
  prim "switch" [ pp x, unwordsM [ppLabel y, brackets $ map f zs ] ]
  where
    f (a,b) = commaSep [ pp $ Lit a, ppLabel b ]

braces :: M String -> M String
braces x = unwordsM [return "{", x, return "}"]

str :: String -> M CString
str s = do
  tbl <- gets strings
  p <- case M.lookup s tbl of
    Just v -> return v
    Nothing -> do
      i <- fresh
      let v = fvar "i8**" i
      modify $ \st -> st{ strings = M.insert s v tbl }
      return v
  load p

declString :: (String, Val (Ptr (Ptr Int8))) -> M ()
declString (x, y) = do
  n <- ppu y
  let s = n ++ ".str"
  output $ unwords [ s, "= private unnamed_addr constant", t, escString x ++ ", align 1" ]
  output $ unwords [ n
                   , "= global i8* getelementptr inbounds (" ++ t ++ "*"
                   , s ++ ", i32 0, i32 0), align 4" ]
  where
    t = "[" ++ show (length x + 1) ++ " x i8]"

escString :: String -> String
escString s = "c\"" ++ concatMap escChar (s ++ "\0") ++ "\""

escChar :: Char -> [Char]
escChar c
  | c == '"' || c < ' ' || c > '~' = "\\" ++ map toUpper a'
  | otherwise = [c]
  where
    a = showHex (fromEnum c) ""
    a' = case a of
      [_] -> "0" ++ a
      _ -> a

eval :: M () -> IO ()
eval m = do
  flip evalStateT (St 0 M.empty S.empty) $ do
    mapM_ output
      [ "%struct.SDL_Rect = type { i32, i32, i32, i32 }"
      , "%struct.SDL_Color = type { i8, i8, i8, i8 }"
      , "%struct.SDL_Palette = type { i32, %struct.SDL_Color*, i32, i32 }"
      , "%struct.SDL_PixelFormat = type { i32, %struct.SDL_Palette*, i8, i8, [2 x i8], i32, i32, i32, i32, i8, i8, i8, i8, i8, i8, i8, i8, i32, %struct.SDL_PixelFormat* }"
      , "%struct.SDL_BlitMap = type opaque"
      , "%struct.SDL_Surface = type { i32, %struct.SDL_PixelFormat*, i32, i32, i32, i8*, i8*, i32, i8*, %struct.SDL_Rect, %struct.SDL_BlitMap*, i32 }"
      , "%struct.SDL_Keysym = type { i32, i32, i16, i32 }"
      , "%struct.SDL_KeyboardEvent = type { i32, i32, i32, i8, i8, i8, i8, %struct.SDL_Keysym }"
      , "%struct.SDL_TouchFingerEvent = type { i32, i32, i64, i64, float, float, float, float, float }"
      , "%union.SDL_Event = type { %struct.SDL_TouchFingerEvent, [8 x i8] }"
      ]
    () <- m
    st <- get
    mapM_ declString $ M.toList $ strings st
    mapM_ output $ decls st
    -- mapM_ declStruct $ M.toList $ structs st

proc :: (Agg a, Ret b) => Name -> (a -> M b) -> Proc a b
proc x = Proc x . Just

proc_ :: Ret b => Name -> M b -> Proc () b
proc_ x m = proc x (\() -> m)

inline :: Agg a => Proc a b -> a -> M b
inline (Proc n mf) x = maybe (error $ "unable to inline ffi call:" ++ n) (\f -> f x) mf
    
args :: Agg a => a -> M String
args x = ((map ppUValT . toList) <$> agg x) >>= parens

procName n = return ("@" ++ n)

define :: (Agg a, Ret b) => Proc a b -> M ()
define (Proc n Nothing) = error $ "unable to define ffi call:" ++ n
define ((Proc n (Just f)) :: Proc a b) = do
  a <- instantiate
  outputM $ unwordsM [ return "define", tyRet (unused "define" :: b), procName n, args a ]
  output "{"
  _ <- f a
  output "}"
 
unUVal :: Ty a => UVal -> Val a
unUVal x = case x of
  Var a b -> Var a b
  Lit a -> Lit $ unULit a

instance Ty a => Agg (Val a) where
  unAgg (Leaf a) = unUVal a
  unAgg _ = unused "unAgg"
  agg x = Leaf <$> toUVal x
  instantiate = f $ unused "instantiate"
    where
      f :: Val a -> M (Val a)
      f a = bvar <$> ty a <*> fresh

instance Agg () where
 unAgg (Node []) = ()
 unAgg _ = unused "unAgg"
 agg () = nodeM []
 instantiate = return ()

nodeM xs = Node <$> sequence xs

instance (Agg a, Agg b) => Agg (a, b) where
  unAgg (Node [a,b]) = (unAgg a, unAgg b)
  unAgg _ = unused "unAgg"
  agg (a, b) = nodeM [agg a, agg b]
  instantiate = (,) <$> instantiate <*> instantiate
 
instance (Agg a, Agg b, Agg c) => Agg (a, b, c) where
  unAgg (Node [a,b,c]) = (unAgg a, unAgg b, unAgg c)
  unAgg _ = unused "unAgg"
  agg (a, b, c) = nodeM [agg a, agg b, agg c]
  instantiate = (,,) <$> instantiate <*> instantiate <*> instantiate
 
instance (Agg a, Agg b, Agg c, Agg d) => Agg (a, b, c, d) where
  unAgg (Node [a,b,c,d]) = (unAgg a, unAgg b, unAgg c, unAgg d)
  unAgg _ = unused "unAgg"
  agg (a, b, c, d) = nodeM [agg a, agg b, agg c, agg d]
  instantiate = (,,,) <$> instantiate <*> instantiate <*> instantiate <*> instantiate
 
instance (Agg a, Agg b, Agg c, Agg d, Agg e) => Agg (a, b, c, d, e) where
  unAgg (Node [a,b,c,d,e]) = (unAgg a, unAgg b, unAgg c, unAgg d, unAgg e)
  unAgg _ = unused "unAgg"
  agg (a, b, c, d, e) = nodeM [agg a, agg b, agg c, agg d, agg e]
  instantiate = (,,,,) <$> instantiate <*> instantiate <*> instantiate <*> instantiate <*> instantiate
 
instance (Agg a, Agg b, Agg c, Agg d, Agg e, Agg f) => Agg (a, b, c, d, e, f) where
  unAgg (Node [a,b,c,d,e,f]) = (unAgg a, unAgg b, unAgg c, unAgg d, unAgg e, unAgg f)
  unAgg _ = unused "unAgg"
  agg (a, b, c, d, e, f) = nodeM [agg a, agg b, agg c, agg d, agg e, agg f]
  instantiate = (,,,,,) <$> instantiate <*> instantiate <*> instantiate <*> instantiate <*> instantiate <*> instantiate

type CString = P Int8

puts :: CString -> M ()
puts = ffi "puts"
  
freshLabel :: M Label
freshLabel = Label <$> fresh
  
label :: Label -> M ()
label x = output (ppULabel x ++ ":")

when' :: BoolT -> M () -> M ()
when' x y = if' x y (return ())

whenNot :: BoolT -> M () -> M ()
whenNot x y = if' x (return ()) y

if' :: BoolT -> M () -> M () -> M ()
if' x y z = switch' x y [(false, z)]

while :: M BoolT -> M () -> M ()
while x y = do
  loop <- freshLabel
  istrue <- freshLabel
  isfalse <- freshLabel
  br loop
  label loop
  a <- x
  switch a istrue [(False, isfalse)]
  label istrue
  y
  br loop
  label isfalse
  
nullptr :: Ty a => P a
nullptr = Lit $ Ptr 0

withPtr :: Ty a => M (P a) -> M () -> (P a -> M ()) -> M ()
withPtr m n f = do
  p <- m
  r <- p `eq` nullptr
  if' r n (f p)

tyRecStruct s = TyRec
  { _toULit = \_ -> die $ "toULit: struct: " ++ s
  , _unULit = \_ -> die $ "unULit: struct: " ++ s
  , _ty = \_ -> return s
  }

err :: String -> M ()
err x = str ("error:" ++ x) >>= puts

rerr :: String -> M ()
rerr x = err $ "unable to create " ++ x

fvar t i = Var t $ "@g" ++ show i

type BoolT = Val Bool

intc :: Char -> IntT
intc = int . fromEnum

char :: Char -> Val Char
char = Lit

unLit x = case x of
  Lit a -> a
  Var _ n -> die $ "unLit: not a literal:" ++ n

die s = error $ "error:" ++ s

true = Lit True
false = Lit False

-- type Rect = ((IntT, IntT), (IntT, IntT))

  {-
import Prelude hiding ((==), (/=), (>), (<), (>=), (<=), (+), (-), (*), (/), (%), (^), abs, fromInteger, fromRational, pi, sqrt, negate, sum, succ, pred)
import Typed
import qualified Prelude as P
import Data.Ratio

instance (Arith a, Integral a) => Arith (Ratio a) where arithRec = arithRecFractional
instance Arith Int where arithRec = arithRecIntegral
instance Arith Integer where arithRec = arithRecIntegral
instance Arith Int' where arithRec = arithRecAtom
instance Arith Bool' where arithRec = arithRecAtom
instance Arith Word' where arithRec = arithRecAtom
instance Arith Word32' where arithRec = arithRecAtom
instance Arith Word64' where arithRec = arithRecAtom
instance Arith Double' where arithRec = arithRecAtom
instance FP Double' where fpRec = fpRecAtom

instance Cmp Int' Bool' where cmpRec = cmpRecAtom
instance Cmp Int32' Bool' where cmpRec = cmpRecAtom
instance Cmp Int64' Bool' where cmpRec = cmpRecAtom
instance Cmp Word' Bool' where cmpRec = cmpRecAtom
instance Cmp Word32' Bool' where cmpRec = cmpRecAtom
instance Cmp Word64' Bool' where cmpRec = cmpRecAtom
instance Cmp Float' Bool' where cmpRec = cmpRecAtom
instance Cmp Double' Bool' where cmpRec = cmpRecAtom

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

arithRecFractional :: Fractional a => ArithRec a
arithRecFractional = ArithRec (P.fromInteger) (P.+) (P.-) (P.*) (P./) (P.undefined) (P.abs)

bitsRecAtom :: (Atom a, Agg a) => BitsRec a
bitsRecAtom =
  BitsRec (binop "shl") (binop "shr") (binop "band") (binop "bor") (binop "xor")

switch :: (Agg a) => Word' -> [a] -> a -> a
switch x ys z = foldr (\(i,a) b -> if' (x == lit i) a b) z $ zip [0 ..] ys

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

reps :: (Agg a) => Word' -> a -> (a -> a) -> a
reps n b f = repsi n b $ \_ -> f

repsi :: (Agg a) => Word' -> a -> (Word' -> a -> a) -> a
repsi n b f = while_ (n, b) $ \(i, b) ->
  ( i > 0
  , let i' = pred i in (i', f i' b)
  )

while_ :: (Agg a, Agg b) => (a, b) -> ((a, b) -> (Bool', (a, b))) -> b
while_ x = snd . while x
  
unfoldi :: (Count c, Atom a, Agg a, Agg b) => (Word' -> b -> (a, b)) -> b -> (b, Array c a)
unfoldi f b0 = repsi (count arr0) (b0, arr0) $ \i (b, arr) -> let (a,b') = f i b in (b', insert arr a i)
  where arr0 = undef

unfoldi_ :: (Count c, Atom a, Agg a) => (Word' -> a) -> Array c a
unfoldi_ f = snd $ unfoldi (\i () -> (f i, ())) ()

foldi :: (Count c, Atom a, Agg a, Agg b) =>
  (Word' -> b -> a -> b) -> b -> Array c a -> b
foldi f x0 arr = repsi (count arr) x0 $ \i x -> f i x $ extract arr i

repeatc :: (Count c, Atom a, Agg a) => a -> Array c a
repeatc = unfoldi_ . const

succ :: Arith a => a -> a
succ = (+) 1

negate :: Arith a => a -> a
negate = (-) 0 -- If you define this as (*) (-1) then you get an infinite loop

not' :: Arith a => a -> a
not' x = negate x - 1

pred :: Arith a => a -> a
pred = (+) (-1)

max' :: (Arith a, Agg a, Cmp a Bool') => a -> a -> a
max' x y = if' (x > y) x y

min' :: (Arith a, Agg a, Cmp a Bool') => a -> a -> a
min' x y = if' (x < y) x y

ord' :: Char -> Word8'
ord' = lit . fromIntegral . fromEnum

putln :: IO' ()
putln = putc $ ord' '\n'

puti :: Int' -> IO' ()
puti = externIO "puti"

putd :: Double' -> IO' ()
putd = externIO "putd"

putc :: Word8' -> IO' ()
putc = externIO "putc"
-}
