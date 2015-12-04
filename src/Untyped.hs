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
import Data.Int

data Tree a = Node [Tree a] | Leaf a deriving (Show, Eq)

fromLeaf x = case x of
  Leaf a -> a
  Node{} -> unused "fromLeaf"
    
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

data Type
  = TInt Bool Integer | TFloating Integer | TArray Integer Type | TWorld
  deriving (Show, Eq, Ord, Generic)
instance Hashable Type
                         
data Lit
  = Rat Type Rational
  | Undef Type
  deriving (Show, Eq, Generic, Ord)
instance Hashable Lit

data User = User{ uid :: Integer, utype :: Type } deriving (Show, Eq, Ord, Generic)
instance Hashable User

data Free = Free{ fid :: Integer, ftype :: Type }
          deriving (Show, Eq, Ord, Generic)
instance Hashable Free

data Bound = Bound{ bid :: Integer, btype :: Type } deriving (Show, Eq, Ord, Generic)
instance Hashable Bound

data Var -- don't reorder
  = UVar User
  | BVar Bound
  | FVar Free
  deriving (Show, Eq, Generic, Ord)
instance Hashable Var

type AExp = Either Lit Var

data Defn a = Defn{ did :: String, dbvars :: [Var], dtype :: Type, body :: Maybe a }
  deriving (Show, Eq, Ord, Generic)
instance Hashable a => Hashable (Defn a)

data CExpr a
  = App (Defn a) [a]
  | Switch a [a] a
  | While Integer a [(Bound, (a, a))] Bound
  deriving (Show, Eq, Ord, Generic)
instance Hashable a => Hashable (CExpr a)

type CExp = CExpr AExp
type Expr a = Either AExp (CExpr a)
newtype Exp = Exp{ unExp :: Expr Exp } deriving (Show, Eq, Ord)

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
    xb = snd $ mapAccumR (\(b:bs) j -> (bs, Bound b (etype j))) [n..] x

var :: Var -> Exp
var = Exp . Left . Right

bvar :: Bound -> Expr Exp
bvar = Left . Right . BVar
    
rat :: Type -> Rational -> Exp
rat t = Exp . Left . Left . Rat t

undef :: Type -> Exp
undef = Exp . Left . Left . Undef

defn :: String -> Type -> Maybe Exp -> Tree Exp -> Exp
defn s t x e = Exp $ Right $ App (Defn s bvs t x) $ toList e
  where
    bvs = toList $ instantiate $ fmap etype e

instantiate :: Tree Type -> Tree Var
instantiate = snd . mapAccumL f 0
  where
    f x y = (succ x, UVar $ User x y)

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
      return $ Right $ FVar $ Free a (either atype ctype x)

toCExpDefn :: Defn Exp -> F (Defn AExp)
toCExpDefn (Defn a bs c d) = Defn a bs c <$> mapM toAExp d

toCExp :: Exp -> F (Either AExp CExp)
toCExp x = case unExp x of
  Left a -> return $ Left a
  Right e -> Right <$> case e of
    App a bs -> App <$> toCExpDefn a <*> mapM toAExp bs
    Switch a bs c -> Switch <$> toAExp a <*> mapM toAExp bs <*> toAExp c
    While a b cs d -> While a <$> toAExp b <*> mapM f cs <*> return d
      where f (p, (q, r)) = (,) p <$> ((,) <$> toAExp q <*> toAExp r)

-- pretty printing

class PP a where pp :: a -> Doc

instance PP Type where
  pp x = case x of
    TInt a b -> (text $ if a then "I" else "W") <> integer b
    TFloating a -> text "F" <> integer a
    TArray a b -> text "A" <+> integer a <+> pp b
    TWorld -> text "world"
    
instance PP Double where pp = double
instance PP Lit where
  pp x = pp (ltype x) <+> case x of
    Rat _ b -> pp b
    Undef _ -> text "undef"

instance (PP a, PP b) => PP (Either a b) where pp = either pp pp

-- printPP :: ([(Free, CExp)], [Defn AExp]) -> IO ()
printPP (cexps, defns) = do
  print $ vcat $ map pp cexps ++ map pp defns
  
ppSwitch a bs c = vcat [text "switch" <+> ppParens a, nest 2 $ pp $ bs ++ [c]]

ppWhile a bs c =
  nest 2 $ vcat [ vcat $ map (\(p, (q, _)) -> ppStore p q) bs
                , text "while" <+> ppParens a
                , nest 2 $ vcat $ map (\(p, (_, r)) -> ppStore p r) bs
                , text "return" <+> pp c
                ]

ppParens x = if (any isSpace $ show d) then parens d else d
  where d = pp x

ppStore x y = pp x <+> text ":=" <+> pp y
    
instance PP Exp where pp = pp . unExp

ppCommas = hcat . intersperse (text ", ")

instance PP Integer where pp = integer

instance PP (Ratio Integer) where
  pp x
    | denominator x == 1 = pp (numerator x)
    | otherwise = pp (fromRational x :: Double)

instance PP Free where pp x = pp (ftype x) <+> text (showFid $ fid x)
instance PP User where pp x = pp (utype x) <+> text "u" <> pp (uid x)

showFid x = "f" ++ show x

instance PP Bound where pp x = pp (btype x) <+> text "b" <> pp (bid x)

instance PP Var where
  pp x = case x of
    UVar a -> pp a
    BVar a -> pp a
    FVar a -> pp a

instance PP a => PP (Defn a) where
  pp x = hsep [ pp $ dtype x, text $ did x, parens $ hsep $ map pp $ dbvars x, text "=", ppDefnBody (body x) ]

ppDefnBody :: PP a => Maybe a -> Doc
ppDefnBody = maybe (text "extern") pp

instance PP a => PP (CExpr a) where
  pp x = case x of
    Switch a bs c -> ppSwitch a bs c
    App a bs -> text (did a) <+> hsep (map ppParens bs)
    While _ a bs c -> ppWhile a bs c

-- type information

ltype x = case x of
  Rat a _ -> a
  Undef a -> a

wtype = TInt False
itype = TInt True

booltype = wtype 1

vtype x = case x of
  UVar a -> utype a
  BVar a -> btype a
  FVar a -> ftype a

ctype :: CExp -> Type
ctype = cexprtype atype

cexprtype :: (a -> Type) -> CExpr a -> Type
cexprtype f x = case x of
  App a _ -> dtype a
  Switch _ _ c -> f c
  While _ _ _ c -> btype c

etype :: Exp -> Type
etype = either atype (cexprtype etype) . unExp

atype :: AExp -> Type
atype = either ltype vtype

-- misc
swap (x,y) = (y,x)

singleton a = [a]

unused = error . (++) "unused:"
debug x = trace ("debug:" ++ show x) x

idIdx f x = V.unsafeIndex x . fromIntegral . f

fidIdx :: Vector a -> Free -> a
fidIdx = idIdx fid

data MapR a b = MapR
  { hmapR :: M.HashMap b a
  , next :: a
  } deriving Show

lookupR :: (Hashable b, Eq b) => b -> MapR a b -> Maybe a
lookupR b = M.lookup b . hmapR

insertR :: (Hashable b, Eq b, Enum a) => b -> MapR a b -> (a, MapR a b)
insertR b tbl = case lookupR b tbl of
  Just a -> (a, tbl)
  Nothing -> (a, tbl{ next = succ a, hmapR = M.insert b a $ hmapR tbl })
    where a = next tbl

runCExpMap :: [Def] -> ([Defn AExp], MapR Integer CExp)
runCExpMap = flip runState (MapR M.empty 0) . mapM defToAExp

-- compile :: [Def] -> ([(Defn CExp)], [Defn AExp])
compile xs =
  let
    (defns0, m) = runCExpMap xs
    n = next m
    cexps0 :: [(Integer, CExp)] = sort $ map swap $ M.toList $ hmapR m
    bvs :: Vector [Var] = constructB argsCExp n cexps0
--    defns :: [Defn AExp] = map (updFBVarsDefn bvs) defns0
--    cexps :: [(Free, CExp)] = map (\(p, q) -> (toFree bvs p (ctype q), updFBVarsCExp bvs q)) cexps0
    defns1 = map (\(i, e) -> Defn (showFid i) (V.unsafeIndex bvs (fromIntegral i)) (ctype e) (Just e)) cexps0
  in trace (show bvs) (defns1, defns0)

-- toFree :: Vector [Var] -> Integer -> Type -> Free
-- toFree bvs i t = let v = Free i t $ fidIdx bvs v in v
    
-- updFBVarsAExp :: Vector [Var] -> AExp -> AExp
-- updFBVarsAExp bvs x = case x of
--   Right (FVar a) -> Right $ FVar a{ fbvars = fidIdx bvs a }
--   x -> x

-- updFBVarsCExp :: Vector [Var] -> CExp -> CExp
-- updFBVarsCExp bvs x = case x of
--   App a bs -> App (updFBVarsDefn bvs a) $ map f bs
--   Switch a bs c -> Switch (f a) (map f bs) (f c)
--   While a b cs d -> While a (f b) [ (p, (f q, f r)) | (p, (q, r)) <- cs ] d
--   where
--     f = updFBVarsAExp bvs
  
-- updFBVarsDefn :: Vector [Var] -> Defn AExp -> Defn AExp
-- updFBVarsDefn bvs x = x{ body = fmap (updFBVarsAExp bvs) $ body x }

-- foo :: Vector (Defn CExp) -> AExp -> Expr AExp
-- foo :: Vector (Defn CExp) -> AExp -> Either AExp (CExpr AExp)
-- foo :: Vector (Defn AExp) -> Either t Var -> Either (Either t Var) (CExpr AExp)
-- foo :: Vector (Defn CExp) -> AExp -> Either AExp CExp
-- foo :: Vector (Defn CExp) -> AExp -> Either AExp (CExpr (Either CExp Var))
foo (tbl :: Vector (Defn CExp)) x = case x of
  Right (FVar a) -> Right (did b, dtype b, dbvars b)
    where b = fidIdx tbl a
  _ -> Left x

-- bar :: Vector (Defn CExp) -> CExp -> CExpr (Either AExp CExp)
-- bar :: Vector (Defn CExp) -> CExpr AExp -> CExpr (Either AExp (CExpr (Either CExp Var)))
bar tbl x = case x of
  -- App a bs -> App a{ body = fmap Left $ body a } $ map f bs
  Switch a bs c -> Switch (f a) (map f bs) (f c)
  While a b cs d -> While a (f b) [ (p, (f q, f r)) | (p, (q, r)) <- cs ] d
  where
    f = foo tbl
             
argsAExp :: Vector [Var] -> AExp -> [Var]
argsAExp arr x = case x of
  Right v -> case v of
    FVar a -> fidIdx arr a
    _ -> singleton v
  Left{} -> []

argsCExp :: Vector [Var] -> CExp -> [Var]
argsCExp arr x = sort $ nub $ case x of
  App _ bs -> go bs
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
