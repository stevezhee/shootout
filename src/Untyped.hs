{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module Untyped where

import Control.Applicative
import Control.Monad
import Control.Monad.State
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
  = TInt Bool Integer | TFloating Integer | TArray Integer Type | TWorld | TAgg
  deriving (Show, Eq, Ord, Generic)
instance Hashable Type
                         
data Lit
  = Rat Type Rational
  | Undef Type
  deriving (Show, Eq, Generic, Ord)
instance Hashable Lit

data UId = UId{ uid :: Integer, utype :: Type } deriving (Show, Eq, Ord, Generic)
instance Hashable UId

data FId = FId{ fid :: Integer, ftype :: Type }
          deriving (Show, Eq, Ord, Generic)
instance Hashable FId

data BId = BId{ bid :: Integer, btype :: Type } deriving (Show, Eq, Ord, Generic)
instance Hashable BId

data Var -- don't reorder
  = UVar UId
  | BVar BId
  | FVar FId
  deriving (Show, Eq, Generic, Ord)
instance Hashable Var

type AExp = Either Lit Var

data Defn a
  = Defn{ did :: String, dbvars :: [Var], dtype :: Type, body :: Maybe a }
  | Loop{ lid :: Maybe FId, dtype :: Type, lbody :: a }
  deriving (Show, Eq, Ord, Generic)
instance Hashable a => Hashable (Defn a)

data CExpr a
  = App (Defn a) [a]
  | Switch a [a] a
  | While a a -- 2nd argument is a Lam
  | Lam Integer [BId] [a]
  | From a BId
  deriving (Show, Eq, Ord, Generic)
instance Hashable a => Hashable (CExpr a)

-- BAL: beware when generating code for a while - need to ensure that
-- only the minimal computation is done pre-loop, on first iteration,
-- and on subsequent iterations.

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
    Lam n _ _ -> n
    App (Loop _ _ b) _ -> maxBV b
    App _ bs -> maximumBV bs
    Switch a bs c -> maximumBV (a : c : bs)
    While _ b -> maxBV b
    From a _ -> maxBV a
    
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
switch x ys z = fmap (\(a:bs) -> mkExp $ Switch x bs a) $ listToTree (z : ys)

while :: Tree Exp -> (Tree Exp -> (Exp, Tree Exp)) -> Tree Exp
while x f =
  fmap (mkExp . From (mkExp . App defn $ toList x)) xb
  where
    defn :: Defn Exp = Loop Nothing TAgg $ mkExp $ While e $ mkExp $ Lam (n+m) (toList xb) (toList x1)
    m = fromIntegral $ length x
    n = maximum [maximumBV x, maxBV e, maximumBV x1]
    (e, x1 :: Tree Exp) = f x0
    x0 :: Tree Exp
    x0 = fmap (Exp . bvar) xb
    xb :: Tree BId
    xb = snd $ mapAccumR (\(b:bs) j -> (bs, BId b (etype j))) [n..] x

mkExp = Exp . Right

var :: Var -> Exp
var = Exp . Left . Right

bvar :: BId -> Expr Exp
bvar = Left . Right . BVar
    
rat :: Type -> Rational -> Exp
rat t = Exp . Left . Left . Rat t

undef :: Type -> Exp
undef = Exp . Left . Left . Undef

defn :: String -> Type -> Maybe Exp -> Tree Exp -> Exp
defn s t x e = mkExp $ App (Defn s bvs t x) $ toList e
  where
    bvs = toList $ instantiate $ fmap etype e

instantiate :: Tree Type -> Tree Var
instantiate = snd . mapAccumL f 0
  where
    f x y = (succ x, UVar $ UId x y)

type F a = State (MapR Integer CExp) a

toAExpDefn :: Defn Exp -> F (Defn AExp)
toAExpDefn x = case x of
  Defn a bs c d -> Defn a bs c <$> mapM toAExp d
  Loop a t b -> do
    b'@(Right (FVar v)) <- toAExp b
    return $ Loop (Just v) t b'

toAExp :: Exp -> F AExp
toAExp x0 = do
  x <- toCExp x0
  case x of
    Left a -> return a
    Right e -> do
      tbl <- get
      let (a, tbl') = insertR e tbl
      modify $ \_ -> tbl'
      return $ Right $ FVar $ FId a (either atype ctype x)

toCExp :: Exp -> F (Either AExp CExp)
toCExp x = case unExp x of
  Left a -> return $ Left a
  Right e -> Right <$> case e of
    App a bs -> App <$> toAExpDefn a <*> mapM toAExp bs
    Switch a bs c -> Switch <$> toAExp a <*> mapM toAExp bs <*> toAExp c
    From a b -> flip From b <$> toAExp a
    While a b -> While <$> toAExp a <*> toAExp b
    Lam a bs cs -> Lam a <$> return bs <*> mapM toAExp cs

-- pretty printing

class PP a where pp :: a -> Doc

instance PP Type where
  pp x = case x of
    TInt a b -> (text $ if a then "I" else "W") <> integer b
    TFloating a -> text "F" <> integer a
    TArray a b -> text "A" <+> integer a <+> pp b
    TWorld -> text "world"
    TAgg -> text "agg"
    
instance PP Double where pp = double
instance PP Lit where
  pp x = {- pp (ltype x) <+> -} case x of
    Rat _ b -> pp b
    Undef _ -> text "undef"

instance (PP a, PP b) => PP (Either a b) where pp = either pp pp

-- printPP :: ([(FId, CExp)], [Defn AExp]) -> IO ()
printPP (cexps, defns) = do
  print $ vcat $ map pp cexps ++ map pp defns
  
ppSwitch a bs c = vcat [text "switch" <+> ppParens a, nest 2 $ pp $ bs ++ [c]]

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

instance PP FId where pp x = {- pp (ftype x) <+> -} text (showFid $ fid x)
instance PP UId where pp x = {- pp (utype x) <+> -} text "u" <> pp (uid x)

showFid x = "f" ++ show x

instance PP BId where pp x = {- pp (btype x) <+> -} text "b" <> pp (bid x)

instance PP Var where
  pp x = case x of
    UVar a -> pp a
    BVar a -> pp a
    FVar a -> pp a

instance PP a => PP (Defn a) where
  pp x = case x of
    Defn{} -> hsep [ pp $ dtype x, text $ did x, ppParens $ dbvars x, text "=", ppDefnBody (body x) ]
    Loop{} -> pp $ lbody x

ppDefnBody :: PP a => Maybe a -> Doc
ppDefnBody = maybe (text "extern") pp

instance PP a => PP (CExpr a) where
  pp x = case x of
    Switch a bs c -> ppSwitch a bs c
    App a@Defn{} bs -> text (did a) <+> hsep (map ppParens bs)
    App (Loop (Just v) _ _) bs -> pp v <+> hsep (map ppParens bs)
    App a bs -> pp a <+> hsep (map ppParens bs)
    From a b -> text "from" <+> pp a <+> pp b
    While a b -> hsep [ text "while", pp a, pp b ]
    Lam _ bs cs -> hsep [ text "\\", ppParens bs, text "->", ppParens cs ]

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
  From _ b -> btype b
  While _ b -> f b
  Lam{} -> TAgg
    
etype :: Exp -> Type
etype = either atype (cexprtype etype) . unExp

atype :: AExp -> Type
atype = either ltype vtype

-- misc
swap (x,y) = (y,x)

singleton a = [a]

unused = error . (++) "unused:"
-- debug x = trace ("debug:" ++ show x) x

idIdx f x = V.unsafeIndex x . fromIntegral . f

fidIdx :: Vector a -> FId -> a
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

runCExpMap :: [Defn Exp] -> ([Defn AExp], MapR Integer CExp)
runCExpMap = flip runState (MapR M.empty 0) . mapM toAExpDefn

-- compile :: [Def] -> ([(Defn CExp)], [Defn AExp])
compile xs =
  let
    (defns0, m) = runCExpMap xs
    n = next m
    cexps0 :: [(Integer, CExp)] = sort $ map swap $ M.toList $ hmapR m
    -- bvs :: Vector [Var] = constructB argsCExp n cexps0
--    defns :: [Defn AExp] = map (updFBVarsDefn bvs) defns0
--    cexps :: [(FId, CExp)] = map (\(p, q) -> (toFId bvs p (ctype q), updFBVarsCExp bvs q)) cexps0
   -- defns1 = map (\(i, e) -> Defn (showFid i) (V.unsafeIndex bvs (fromIntegral i)) (ctype e) (Just e)) cexps0
    defns1 = map (\(i, e) -> Defn (showFid i) [] (ctype e) (Just e)) cexps0
    -- (gr, _) = graphFromEdges' $ ((), n, map fid $ catMaybes $ map asdf defns0) : map foo cexps0
  in
   -- trace (show $ indegree gr)
    (defns1, defns0)

asdf :: Defn AExp -> Maybe FId
asdf x = case x of
  Defn _ _ _ (Just a) -> bar a
  _ -> Nothing
  
foo :: (Integer, CExp) -> ((), Integer, [Integer])
foo (i,e) = ((), i, map fid $ blah e)

bar :: AExp -> Maybe FId
bar x = case x of
  Right (FVar a) -> Just a
  _ -> Nothing

blah :: CExp -> [FId]
blah x = nub $ case x of
  App a bs -> case a of
    Loop{} -> catMaybes [lid a] ++ f bs
    Defn{} -> f $ catMaybes [body a] ++ bs
  Switch a bs c -> f (a : c : bs)
  From a _ -> f [a]
  While a b -> f [a, b]
  Lam _ _ cs -> f cs
  where
    f = catMaybes . map bar

-- asdf :: Defn AExp -> Maybe FId
-- asdf x = case x of
--   Defn _ _ _ (Just a) -> bar a
--   _ -> Nothing
  
-- foo :: (Integer, CExp) -> ((), Integer, [Integer])
-- foo (i,e) = ((), i, map fid $ blah e)

-- bar :: AExp -> Maybe FId
-- bar x = case x of
--   Right (FVar a) -> Just a
--   _ -> Nothing

-- blah :: CExp -> [FId]
-- blah x = nub $ case x of
--   App a bs -> case a of
--     Loop{} -> catMaybes [lid a] ++ f bs
--     Defn{} -> f $ catMaybes [body a] ++ bs
--   Switch a bs c -> f (a : c : bs)
--   From a _ -> f [a]
--   While a b -> f [a, b]
--   Lam _ _ cs -> f cs
--   where
--     f = catMaybes . map bar
    
-- toFId :: Vector [Var] -> Integer -> Type -> FId
-- toFId bvs i t = let v = FId i t $ fidIdx bvs v in v
    
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

-- argsAExp :: Vector [Var] -> AExp -> [Var]
-- argsAExp arr x = case x of
--   Right v -> case v of
--     FVar a -> fidIdx arr a
--     _ -> singleton v
--   Left{} -> []

-- argsCExp :: Vector [Var] -> CExp -> [Var]
-- argsCExp arr x = sort $ nub $ case x of
--   App _ bs -> go bs
--   Switch a bs c -> go (a : c : bs)
--   From _ b -> go [b]
--   While _ bs c ds -> go (c : ds) \\ map BVar bs
--   where
--     go = foldr1 union . map (argsAExp arr)

-- constructB :: (Vector a -> b -> a) -> Integer -> [(Integer, b)] -> Vector a
-- constructB f n xs = V.create $ do
--   arr <- VM.new $ fromIntegral n
--   iarr <- V.unsafeFreeze arr
--   mapM_ (\(i,b) -> VM.unsafeWrite arr (fromIntegral i) (f iarr b)) xs
--   return arr
