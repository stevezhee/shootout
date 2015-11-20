{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Untyped where

import Control.Applicative
import Control.Monad hiding ()
import Control.Monad.State hiding ()
import Data.Bits
import Data.Char
import Data.Foldable hiding (mapM_)
import Data.Graph hiding (Tree, Node)
import Data.Hashable
import Data.List (sort, intersperse, (\\), nub, union)
import Data.Maybe
import Data.Ratio
import Data.Traversable hiding (mapM)
import Data.Vector (Vector)
import Data.Word
import Debug.Trace
import GHC.Generics (Generic)
import Prelude hiding (foldr, foldr1, mapM, mapM_, sequence, elem, maximum, concat)
import System.Process hiding (env)
import Text.PrettyPrint hiding (int, empty)
import qualified Data.HashMap.Strict as M
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Text.PrettyPrint as PP

data Tree a = Node [Tree a] | Leaf a deriving (Show, Eq)

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
    
instance PP Int where pp = PP.int
instance PP a => PP [a] where pp = vcat . map pp
instance PP a => PP (Vector a) where pp xs = pp $ zip [0 :: Int ..] $ V.toList xs

instance (PP a, PP b) => PP (a,b) where pp (a,b) = parens (pp a <+> pp b)
instance (PP a, PP b, PP c) => PP (a,b,c) where
  pp (a,b,c) = parens (pp a <+> pp b <+> pp c)

class Typed a where typeof :: a -> Type
class PP a where pp :: a -> Doc

data Type
  = TInt Bool Integer | TFloating Integer | TVector Integer Type
  deriving (Show, Eq, Ord, Generic)
instance Hashable Type

instance PP Type where
  pp x = case x of
    TInt a b -> (text $ if a then "I" else "W") <> integer b
    TFloating a -> text "F" <> integer a
    
instance PP Double where pp = double
                         
data Lit
  = Rat Type Rational
  -- | Undef Type
  deriving (Show, Eq, Generic, Ord)
instance Hashable Lit
instance PP Lit where
  pp x = case x of
    Rat _ b -> pp b
    -- Undef _ -> text "undef"
instance Typed Lit where
  typeof x = case x of
    Rat a _ -> a
    -- Undef a -> a

instance Typed Float where typeof _ = TFloating 32
instance Typed Double where typeof _ = TFloating 64
instance Typed Int where typeof _ = TInt True 32
instance Typed Bool where typeof _ = TInt False 1 -- BAL: make general for enums
instance Typed Word where typeof _ = TInt False 32

instance PP Integer where pp = integer

instance PP (Ratio Integer) where
  pp x
    | denominator x == 1 = pp (numerator x)
    | otherwise = pp (fromRational x :: Double)

data User = User{ uid :: Integer, utype :: Type } deriving (Show, Eq, Ord, Generic)
instance Hashable User
instance PP User where pp x = text "u" <> pp (uid x)
instance Typed User where typeof = utype

data Free = Free{ fid :: Integer, ftype :: Type, fbvars :: [Var] }
          deriving (Show, Eq, Ord, Generic)
instance Hashable Free
instance PP Free where pp x = text "f" <> pp (fid x)
instance Typed Free where typeof = ftype

data Bound = Bound{ bid :: Integer, btype :: Type } deriving (Show, Eq, Ord, Generic)
instance Hashable Bound
instance Typed Bound where typeof = btype
instance PP Bound where pp x = text "b" <> pp (bid x)

data Var -- don't reorder
  = UVar User
  | BVar Bound
  | FVar Free
  deriving (Show, Eq, Generic, Ord)
instance Hashable Var
instance PP Var where
  pp x = case x of
    UVar a -> pp a
    BVar a -> pp a
    FVar a -> pp a
instance Typed Var where
  typeof x = case x of
    UVar a -> typeof a
    BVar a -> typeof a
    FVar a -> typeof a

type AExp = Either Lit Var

data Defn a = Defn{ did :: String, dbvars :: [User], dtype :: Type, body :: Maybe a }
            deriving (Show, Eq, Ord, Generic)
instance Hashable a => Hashable (Defn a)

instance Typed (Defn a) where typeof = dtype

instance PP (Defn a) where pp = text . did
                              
data CExpr a
  = App (Either Op (Defn a)) [a]
  | Switch a [a] a
  | While Integer a [(Bound, (a, a))] Bound
  deriving (Show, Eq, Ord, Generic)
instance Hashable a => Hashable (CExpr a)

type CExp = CExpr AExp
type Expr a = Either AExp (CExpr a)
newtype Exp = Exp{ unExp :: Expr Exp } deriving (Show, Eq, Ord)

instance Typed Exp where typeof = typeof . unExp

instance (Typed a, Typed b) => Typed (Either a b) where typeof = either typeof typeof
                                                        
instance Typed a => Typed (CExpr a) where
  typeof x = case x of
    App a _ -> typeof a
    Switch _ _ c -> typeof c
    While _ _ _ c -> typeof c
    
data UOp
  = Add | Sub | Mul | Div | Rem
  | Shl | AShr | And | Or | Xor
  | ExtractElement | InsertElement | ShuffleVector
  | Cast
  | Sqrt | Pow
  | Eq | Ne | Gt | Gte | Lt | Lte
  | Abs
  | Sin | Cos | ExpBaseE | LogBaseE
  | Floor | Ceil | Trunc | Round
  deriving (Show, Eq, Ord, Generic)
instance Hashable UOp
instance PP UOp where pp = text . map toLower . show

data Op = Op{ uop :: UOp, otype :: Type  } deriving (Show, Eq, Ord, Generic)
instance Hashable Op
instance Typed Op where typeof = otype
instance PP Op where pp = pp . uop

isBinop = either
  (flip elem [ Add, Mul, Sub, Div, Rem, And, Or, Xor, Shl, AShr
             , Eq, Ne, Gt, Lt, Gte, Lte ] . uop)
  (\_ -> False)

isCast = either ((==) Cast . uop) (\_ -> False)

maximumBV :: (Foldable t, Functor t) => t Exp -> Integer
maximumBV = maximum . fmap maxBV

maxBV :: Exp -> Integer
maxBV x = case unExp x of
  Left _ -> 0
  -- ^ not a typo
  -- see http://pchiusano.github.io/2014-06-20/simple-debruijn-alternative.html
  -- or http://www.cse.chalmers.se/~emax/documents/axelsson2013using.pdf
  Right e -> case e of
    App _ bs -> maximumBV bs
    Switch a bs c -> maximumBV (a : c : bs)
    While n _ _ _ -> n
  
zipWithTree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
  -- ^ trees must have the same shape
zipWithTree f x y =
  case (x,y) of
   (Leaf a, Leaf b) -> Leaf $ f a b
   (Node bs, Node cs) -> Node $ map (uncurry $ zipWithTree f) $ zip bs cs
   _ -> unused "zipWithTree"

listToTree :: [Tree a] -> Tree [a]
listToTree xs@(b:_) = foldr (zipWithTree (:)) (fmap (\_ -> []) b) xs

zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree = zipWithTree (,)

switch :: Exp -> [Tree Exp] -> Tree Exp -> Tree Exp
switch x ys z = fmap (\(a:bs) -> Exp $ Right $ Switch x bs a) $ listToTree (z : ys)

while :: Tree Exp -> (Tree Exp -> (Exp, Tree Exp)) -> Tree Exp
while x f =
  fmap (Exp . Right . (While (n+m) e $ sort $ toList $ zipTree xb $ zipTree x x1)) xb
  where
    m = fromIntegral $ length x
    n = maximum [maximumBV x, maxBV e, maximumBV x1]
    (e, x1) = f x0
    x0 = fmap (Exp . bvar) xb
    xb =
      snd $ mapAccumR (\(b:bs) j -> (bs, Bound b (typeof j))) [n..] x

bvar :: Bound -> Expr Exp
bvar = Left . Right . BVar

app :: UOp -> Type -> [Exp] -> Exp
app o t xs = case (o, [ r | Left (Left (Rat _ r)) <- map unExp xs ]) of
  (Add, [a,b]) -> asRat (a + b)
  (Sub, [a,b]) -> asRat (a - b)
  (Mul, [a,b]) -> asRat (a * b)
  (Div, [a,b]) -> case t of
    TInt{} -> asInt2 div a b
    TFloating{} -> asRat (a / b)
  (Rem, [a,b]) -> asInt2 rem a b
  (Shl, [a,b]) -> asRat $ fromInteger $ shiftL (numerator a) (fromInteger $ numerator b)
  (AShr, [a,b]) -> asRat $ fromInteger $ shiftR (numerator a) (fromInteger $ numerator b)
  (And, [a,b]) -> asInt2 (.&.) a b
  (Or, [a,b]) -> asInt2 (.|.) a b
  (Xor, [a,b]) -> asInt2 xor a b
  (Cast, [a]) -> asRat a
  (Sqrt, [a]) -> asDbl sqrt a
  (Pow, [a,b]) -> asDbl2 (**) a b
  (Eq, [a,b]) -> asCmp (a == b)
  (Ne, [a,b]) -> asCmp (a /= b)
  (Gt, [a,b]) -> asCmp (a > b)
  (Gte, [a,b]) -> asCmp (a >= b)
  (Lt, [a,b]) -> asCmp (a < b)
  (Lte, [a,b]) -> asCmp (a <= b)
  (Abs, [a]) -> asRat $ abs a
  (Sin, [a]) -> asDbl sin a
  (Cos, [a]) -> asDbl cos a
  (ExpBaseE, [a]) -> asDbl exp a
  (LogBaseE, [a]) -> asDbl log a
  (Floor, [a]) -> asDbl floor a
  (Ceil, [a]) -> asDbl ceiling a
  (Trunc, [a]) -> asDbl truncate a
  (Round, [a]) -> asDbl round a
  -- | ExtractElement | InsertElement | ShuffleVector
  _ | xs /= sort xs -> case (o, xs) of
      _ | o `elem` [Add, Mul, And, Or, Xor, Eq, Ne] -> ok o $ sort xs
      (Gt, [a,b]) -> ok Lt [b,a]
      (Gte, [a,b]) -> ok Lte [b,a]
      (Lt, [a,b]) -> ok Gt [b,a]
      (Lte, [a,b]) -> ok Gte [b,a]
      _ -> ok o xs
  _ -> ok o xs
  where
    asCmp = asRat . toRational . fromEnum
    ok op = Exp . Right . App (Left $ Op op t)
    asInt2 f a b = asRat $ toRational $ f (numerator a) (numerator b)
    asDbl2 f a b = asRat $ toRational $ f (fromRational a) (fromRational b)
    asDbl f a = asRat $ toRational $ f (fromRational a)
    asRat = rat t
    
rat :: Type -> Rational -> Exp
rat t = Exp . Left . Left . Rat t

toExp = Exp . Left

tbool = TInt False 1

defn :: String -> Type -> Maybe Exp -> Tree Exp -> Exp
defn s t x e = Exp $ Right $ App (Right $ Defn s bvs t x) $ toList e
  where
    bvs = toList $ instantiate $ fmap typeof e

instantiate :: Tree Type -> Tree User
instantiate = snd . mapAccumL user 0

user :: Integer -> Type -> (Integer, User)
user x y = (succ x, User x y)

lookupR :: (Hashable b, Eq b) => b -> MapR a b -> Maybe a
lookupR b = M.lookup b . hmapR

insertR :: (Hashable b, Eq b, Enum a) => b -> MapR a b -> (a, MapR a b)
insertR b tbl = case lookupR b tbl of
  Just a -> (a, tbl)
  Nothing -> (a, tbl{ next = succ a, hmapR = M.insert b a $ hmapR tbl })
    where a = next tbl

type F a = State (MapR Integer CExp) a

type Def = Defn Exp

defToAExp :: Def -> F (Defn AExp)
defToAExp (Defn a b c d) = Defn a b c <$> mapM toAExp d

toAExp :: Exp -> F AExp
toAExp x0 = do
  x <- toCExp x0
  case x of
    Left a -> return a
    Right e -> do
      tbl <- get
      let (a, tbl') = insertR e tbl
      modify $ \_ -> tbl'
      return $ Right $ FVar $ Free a (typeof x) []

toCExpDefn :: Defn Exp -> F (Defn AExp)
toCExpDefn (Defn a bs c d) = Defn a bs c <$> mapM toAExp d

toCExp :: Exp -> F (Either AExp CExp)
toCExp x = case unExp x of
  Left a -> return $ Left a
  Right e -> Right <$> case e of
    App a bs -> App <$> f <*> mapM toAExp bs
      where
        f = case a of
          Left o -> return $ Left o
          Right d -> Right <$> toCExpDefn d
    Switch a bs c -> Switch <$> toAExp a <*> mapM toAExp bs <*> toAExp c
    While a b cs d -> While a <$> toAExp b <*> mapM f cs <*> return d
      where f (p, (q, r)) = (,) p <$> ((,) <$> toAExp q <*> toAExp r)
  
ppSwitch a bs c = vcat [text "switch" <+> ppParens a, nest 2 $ pp $ bs ++ [c]]

ppWhile a bs c =
  vcat [ pp c <+> text "from"
       , nest 2 $ vcat [ vcat $ map (\(p, (q, _)) -> ppStore p q) bs
                       , text "while" <+> ppParens a
                       , nest 2 $ vcat $ map (\(p, (_, r)) -> ppStore p r) bs
                       ]
       ]

ppParens x = if (any isSpace $ show d) then parens d else d
  where d = pp x

ppStore x y = pp x <+> text ":=" <+> pp y
    
instance PP Exp where pp = pp . unExp

ppCommas = hcat . intersperse (text ", ")

instance (PP a, PP b) => PP (Either a b) where pp = either pp pp

instance (PP a) => PP (CExpr a) where
  pp x = case x of
    Switch a bs c -> ppSwitch a bs c
    App a bs -> pp a <+> hsep (map ppParens bs)
    While _ a bs c -> ppWhile a bs c

unused = error . (++) "unused:"
debug x = trace ("debug:" ++ show x) x

data MapR a b = MapR
  { hmapR :: M.HashMap b a
  , next :: a
  } deriving Show

uvar = Exp . Left . Right . UVar

runCExpMap :: [Def] -> ([Defn AExp], MapR Integer CExp)
runCExpMap = flip runState (MapR M.empty 0) . mapM defToAExp

updFBVarsCExp :: Vector [Var] -> CExp -> CExp
updFBVarsCExp bvs x = case x of
  App a bs -> App (either Left (Right . updFBVarsDefn bvs) a) $ map f bs
  Switch a bs c -> Switch (f a) (map f bs) (f c)
  While a b cs d -> While a (f b) [ (p, (f q, f r)) | (p, (q, r)) <- cs ] d
  where
    f = updFBVarsAExp bvs
    
updFBVarsAExp :: Vector [Var] -> AExp -> AExp
updFBVarsAExp bvs x = case x of
  Right (FVar a) -> Right $ FVar a{ fbvars = fidIdx bvs a }
  x -> x
  
updFBVarsDefn :: Vector [Var] -> Defn AExp -> Defn AExp
updFBVarsDefn bvs x = x{ body = fmap (updFBVarsAExp bvs) $ body x }

toFree :: Vector [Var] -> Integer -> Type -> Free
toFree bvs i t = let v = Free i t $ fidIdx bvs v in v

ppDefnBody = maybe (text "extern") pp . body

compile :: String -> [Def] -> ([(Free, CExp)], [Defn AExp])
compile fn xs =
  let
    (defns0, m) = runCExpMap xs
    n = next m
    cexps0 :: [(Integer, CExp)] = sort $ map swap $ M.toList $ hmapR m
    bvs :: Vector [Var] = constructB argsCExp n cexps0
    defns :: [Defn AExp] = map (updFBVarsDefn bvs) defns0
    cexps :: [(Free, CExp)] = map (\(p, q) -> (toFree bvs p (typeof q), updFBVarsCExp bvs q)) cexps0

  in (cexps, defns)

printPP :: ([(Free, CExp)], [Defn AExp]) -> IO ()
printPP (cexps, defns) = do
  print $ vcat $ map pp cexps ++ map (\x -> parens (pp x <+> ppDefnBody x)) defns

swap (x,y) = (y,x)

singleton a = [a]

idIdx f x = V.unsafeIndex x . fromIntegral . f

fidIdx = idIdx fid

argsAExp :: Vector [Var] -> AExp -> [Var]
argsAExp arr x = case x of
  Right v -> case v of
    FVar a -> fidIdx arr a
    _ -> singleton v
  Left{} -> []

argsCExp :: Vector [Var] -> CExp -> [Var]
argsCExp arr x = sort $ nub $ case x of
  App a bs -> either (\_ -> []) (map UVar . dbvars) a ++ go bs
  Switch a bs c -> go (a : c : bs)
  While _ a bs _ -> vs \\ map (BVar . fst) bs
    where vs = go (a : concat [ [p, q] | (_, (p, q)) <- bs ])
  where
    go = foldr1 union . map (argsAExp arr)

constructB :: (Vector a -> b -> a) -> Integer -> [(Integer, b)] -> Vector a
constructB f n xs = V.create $ do
  arr <- VM.new $ fromIntegral n
  iarr <- V.unsafeFreeze arr
  mapM_ (\(i,b) -> VM.unsafeWrite arr (fromIntegral i) (f iarr b)) xs
  return arr
