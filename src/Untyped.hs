{-# OPTIONS -fno-warn-missing-signatures #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
{-# OPTIONS -fno-warn-type-defaults #-}
{-# OPTIONS -fno-warn-unused-imports #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

-- -XOverlappingInstances is deprecated: instead use per-instance pragmas OVERLAPPING/OVERLAPPABLE/OVERLAPS

module Untyped where

import Prelude hiding (foldr, foldr1, mapM, mapM_, sequence, elem, maximum, concat)
import Data.Hashable
import GHC.Generics (Generic)
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint hiding (int, empty)
import Data.Traversable hiding (mapM)
import Data.Word

import Debug.Trace
-- import           Control.Applicative hiding (empty)
-- import           Control.Exception
import Control.Monad.State hiding ()
import Control.Monad hiding ()
import qualified Data.HashMap.Strict as M
import Data.List (sort, intersperse, (\\), nub, union)
--   hiding (insert, lookup, elem, maximum, concatMap, mapAccumR, foldr, concat)
import Data.Maybe
import Data.Char
-- import  Prelude
--   hiding (lookup, elem, maximum, concatMap, mapM, sequence, foldr, concat)
-- import Data.Array
-- import LLVM.General.Module
-- import LLVM.General.Context
-- import Control.Monad.Trans.Except
-- import LLVM.General.AST hiding
--   (Type(..), Terminator(..), Operator(..), Constant(..), Instruction(..))
-- import qualified LLVM.General.AST as A
-- import LLVM.General.AST.Global
-- import qualified LLVM.General.AST.Constant as C
-- import qualified LLVM.General.AST.CallingConvention as CC
-- import qualified LLVM.General.AST.IntegerPredicate as IP
-- import qualified LLVM.General.AST.FloatingPointPredicate as FP
-- import qualified LLVM.General.AST.Float as F
-- import Data.Set (Set)
-- import qualified Data.Set as S
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Data.Foldable hiding (mapM_)
-- import Data.Bits
import Data.Graph hiding (Tree, Node)
-- import Data.GraphViz hiding (Int)
import System.Process hiding (env)
import qualified Data.Text.Lazy.IO as T
import Data.Ratio
import Control.Applicative
-- import Data.Bifunctor

data Tree a = Node [Tree a] | Leaf a deriving (Show, Eq)

instance Foldable Tree where
  foldr f b = \case
    Leaf a -> f a b
    Node ns -> foldr (flip (foldr f)) b ns

instance Traversable Tree where
  traverse f = \case
    Leaf a -> Leaf <$> f a
    Node ns -> Node <$> traverse (traverse f) ns

instance Functor Tree where
  fmap f = \case
    Leaf a -> Leaf $ f a
    Node ns -> Node $ fmap (fmap f) ns
    
instance PP Int where pp = PP.int
instance PP a => PP [a] where pp = vcat . map pp
-- instance PP a => PP (Set a) where pp xs = braces $ ppCommas $ map pp $ S.toList xs
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

instance PPC Type where ppc = pp
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
  pp = \case
    Rat _ b -> pp b
    -- Undef _ -> text "undef"
instance Typed Lit where
  typeof = \case
    Rat a _ -> a
    -- Undef a -> a

instance Typed Float where typeof _ = TFloating 32
instance Typed Double where typeof _ = TFloating 64
instance Typed Int where typeof _ = TInt True 32
instance Typed Bool where typeof _ = TInt False 1 -- BAL: make general for enums
instance Typed Word where typeof _ = TInt False 32

instance PP Integer where pp = integer
instance PP Rational where
  pp x | denominator x == 1 = pp (numerator x)
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
  pp = \case
    UVar a -> pp a
    BVar a -> pp a
    FVar a -> pp a
instance Typed Var where
  typeof = \case
    UVar a -> typeof a
    BVar a -> typeof a
    FVar a -> typeof a

class PPC a where ppc :: a -> Doc

instance PPC Lit where ppc = pp

instance PPC Free where ppc x = pp x <> parens (ppCommas $ map ppc $ fbvars x)
  
instance PPC Var where
  ppc = \case
    FVar a -> ppc a
    x -> pp x

instance (PPC a, PPC b) => PPC (Either a b) where ppc = either ppc ppc

instance (PPC a, Typed a, PP a) => PPC (Expr a) where
  ppc = \case
    AExp a -> ppc a
    App a bs -> ppAppC a bs
    Switch a bs c -> ppSwitchC (ppc a) (map ppc bs) (ppc c)
    While _ a bs c -> ppWhileC (ppc a) [ (ppc (typeof p), (pp p, (ppc q, ppc r)))
                                       | (p, (q, r)) <- bs ] (pp c)
      
type AExp = Either Lit Var

data Defn a = Defn{ did :: String, dbvars :: [User], dtype :: Type, body :: Maybe a }
            deriving (Show, Eq, Ord, Generic)
instance Hashable (Defn AExp)

instance Typed (Defn a) where typeof = dtype

instance PP (Defn a) where pp = text . did
                              
data Expr a -- BAL: turn this into CExp and eliminate the AExp constructor.  Then Exps are Either AExp (CExp Exp)
  = AExp AExp
  | App (Either Op (Defn a)) [a]
  | Switch a [a] a
  | While Integer a [(Bound, (a, a))] Bound
  deriving (Show, Eq, Ord, Generic)
instance Hashable CExp

type CExp = Expr AExp -- BAL: actually we should eliminate the AExp constructor here.
newtype Exp = Exp{ unExp :: Expr Exp } deriving (Show, Eq, Ord)

instance Typed Exp where typeof = typeof . unExp

instance (Typed a, Typed b) => Typed (Either a b) where typeof = either typeof typeof
                                                        
instance Typed a => Typed (Expr a) where
  typeof = \case
    AExp a -> typeof a
    App a _ -> typeof a
    Switch _ _ c -> typeof c
    While _ _ _ c -> typeof c
    
data UOp
  = Add | Mul | Sub | Div | Rem | And | Or | Xor | Shl | Lshr | Ashr | Eq | Ne | Gt
  | Lt | Gte | Lte
  | Abs | Neg | Not | Signum | Sqrt | ExpF | Log | Sin | Cos | Asin | Atan | Acos | Sinh | Cosh
  | Asinh | Atanh | Acosh
  | InsertElement | ExtractElement | ShuffleVector
  | Cast
  deriving (Show, Eq, Ord, Generic)
instance Hashable UOp
instance PP UOp where pp = text . map toLower . show

data Op = Op{ uop :: UOp, otype :: Type  } deriving (Show, Eq, Ord, Generic)
instance Hashable Op
instance Typed Op where typeof = otype
instance PP Op where pp = pp . uop

isBinop = either
  (flip elem [ Add, Mul, Sub, Div, Rem, And, Or, Xor, Shl, Lshr
             , Ashr, Eq, Ne, Gt, Lt, Gte, Lte ] . uop)
  (\_ -> False)

isCast = either ((==) Cast . uop) (\_ -> False)

maximumBV :: (Foldable t, Functor t) => t Exp -> Integer
maximumBV = maximum . fmap maxBV

maxBV :: Exp -> Integer
maxBV x = case unExp x of
  AExp _ -> 0
  -- ^ not a typo
  -- see http://pchiusano.github.io/2014-06-20/simple-debruijn-alternative.html
  -- or http://www.cse.chalmers.se/~emax/documents/axelsson2013using.pdf
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
switch x ys z = fmap (\(a:bs) -> Exp $ Switch x bs a) $ listToTree (z : ys)

while :: Tree Exp -> (Tree Exp -> (Exp, Tree Exp)) -> Tree Exp
while x f =
  fmap (Exp . (While (n+m) e $ sort $ toList $ zipTree xb $ zipTree x x1)) xb
  where
    m = fromIntegral $ length x
    n = maximum [maximumBV x, maxBV e, maximumBV x1]
    (e, x1) = f x0
    x0 = fmap (Exp . bvar) xb
    xb =
      snd $ mapAccumR (\(b:bs) j -> (bs, Bound b (typeof j))) [n..] x

bvar :: Bound -> Expr Exp
bvar = AExp . Right . BVar

app o t = Exp . App (Left $ Op o t)
rat t = Exp . AExp . Left . Rat t

toExp = Exp . AExp

tbool = TInt False 1

defn :: String -> Type -> Maybe Exp -> Tree Exp -> Exp
defn s t x e = Exp $ App (Right $ Defn s bvs t x) $ toList e
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
    AExp a -> return a
    _ -> do
      tbl <- get
      let (a, tbl') = insertR x tbl
      modify $ \_ -> tbl'
      return $ Right $ FVar $ Free a (typeof x) []

toCExpDefn :: Defn Exp -> F (Defn AExp)
toCExpDefn (Defn a bs c d) = Defn a bs c <$> mapM toAExp d

toCExp :: Exp -> F CExp
toCExp x = case unExp x of
  AExp a -> return $ AExp a
  App a bs -> App <$> f <*> mapM toAExp bs
    where
      f = case a of
        Left o -> return $ Left o
        Right d -> Right <$> toCExpDefn d
  Switch a bs c -> Switch <$> toAExp a <*> mapM toAExp bs <*> toAExp c
  While a b cs d -> While a <$> toAExp b <*> mapM f cs <*> return d
    where f (p, (q, r)) = (,) p <$> ((,) <$> toAExp q <*> toAExp r)
  
ppSwitch a bs c = vcat [text "switch" <+> ppParens a, nest 2 $ pp $ bs ++ [c]]

ppReturnC x = text "return" <+> x <> semi

ppBlockC x ys = x $$ (nest 2 $ vcat [ text "{" $+$ (nest 2 $ vcat ys), text "}" ])

ppAltC :: Doc -> Doc -> Doc
ppAltC x y = hsep [ x <> colon, ppReturnC y ]
ppCaseC :: (Int, Doc) -> Doc
ppCaseC (x,y) = ppAltC (text "case" <+> PP.int x) y
ppDefaultC :: Doc -> Doc
ppDefaultC = ppAltC (text "default")
ppSwitchC :: Doc -> [Doc] -> Doc -> Doc
ppSwitchC x ys z = ppBlockC (text "switch" <> parens x) $
  map ppCaseC (zip [0..] ys) ++ [ ppDefaultC z ]

ppWhile a bs c =
  vcat [ pp c <+> text "from"
       , nest 2 $ vcat [ vcat $ map (\(p, (q, _)) -> ppStore p q) bs
                       , text "while" <+> ppParens a
                       , nest 2 $ vcat $ map (\(p, (_, r)) -> ppStore p r) bs
                       ]
       ]

ppAssignC x y = x <+> text "=" <+> y <> semi

ppVarDeclC x = ppc (typeof x) <+> ppc x

ppWhileC a bs c =
  vcat
    [ vcat [ t <+> ppAssignC p q | (t, (p, (q, _))) <- bs ]
    , ppBlockC (text "while" <> parens a) [ ppAssignC p r | (_, (p, (_, r))) <- bs ]
    , ppReturnC c
    ]

ppParens x = if (any isSpace $ show d) then parens d else d
  where d = pp x

ppStore x y = pp x <+> text ":=" <+> pp y
    
instance PP Exp where pp = pp . unExp

ppCommas = hcat . intersperse (text ", ")

instance (PP a, PP b) => PP (Either a b) where pp = either pp pp
                                            
ppAppC :: (PPC a, Typed a, PP a) => Either Op (Defn a) -> [a] -> Doc
ppAppC x ys = ppReturnC $ case () of
  () | isBinop x -> b0 <+> pp x <+> b1
     | isCast x -> parens (ppc (typeof x)) <> b0
     | otherwise -> pp x <> parens (ppCommas bs)
  where
    bs = map ppc ys
    b0:_ = bs
    _:b1:_ = bs

instance (PP a) => PP (Expr a) where
  pp = \case
    Switch a bs c -> ppSwitch a bs c
    App a bs -> pp a <+> hsep (map ppParens bs)
    AExp a -> pp a
    While _ a bs c -> ppWhile a bs c
    -- EPhi a b -> hsep [text "phi", pp a, ppParens b]

unused = error . (++) "unused:"
debug x = trace ("debug:" ++ show x) x

data MapR a b = MapR
  { hmapR :: M.HashMap b a
  , next :: a
  } deriving Show

ppCExpC :: (Free, CExp) -> Doc
ppCExpC (x, y) = ppBlockC (ppCExpSigC x) [ppc y]

ppCExpSigC x = text "static" <+> ppSigC x (pp x) (fbvars x)

ppSigC x y zs = ppc (typeof x) <+> y <> parens (ppCommas $ map ppVarDeclC zs)

uvar = Exp . AExp . Right . UVar
ppDefnSigC x = ppSigC x (text $ did x) (map UVar $ dbvars x)

ppDefnDeclC x = ppDefnSigC x <> semi

ppDefnC :: Defn AExp -> Doc
ppDefnC x = case body x of
  Nothing -> PP.empty
  Just a -> ppBlockC (ppDefnSigC x) [ppReturnC $ ppc a]

runCExpMap :: [Def] -> ([Defn AExp], MapR Integer CExp)
runCExpMap = flip runState (MapR M.empty 0) . mapM defToAExp

updFBVarsCExp :: Vector [Var] -> CExp -> CExp
updFBVarsCExp bvs x = case x of
  AExp a -> AExp $ f a
  App a bs -> App (either Left (Right . updFBVarsDefn bvs) a) $ map f bs
  Switch a bs c -> Switch (f a) (map f bs) (f c)
  While a b cs d -> While a (f b) [ (p, (f q, f r)) | (p, (q, r)) <- cs ] d
  where
    f = updFBVarsAExp bvs
    
updFBVarsAExp :: Vector [Var] -> AExp -> AExp
updFBVarsAExp bvs = \case
  Right (FVar a) -> Right $ FVar a{ fbvars = fidIdx bvs a }
  x -> x
  
updFBVarsDefn :: Vector [Var] -> Defn AExp -> Defn AExp
updFBVarsDefn bvs x = x{ body = fmap (updFBVarsAExp bvs) $ body x }

toFree :: Vector [Var] -> Integer -> Type -> Free
toFree bvs i t = let v = Free i t $ fidIdx bvs v in v

ppDefnBody = maybe (text "extern") pp . body

compile :: String -> [Def] -> ([(Free, CExp)], [Defn AExp])
compile fn xs =
  -- print $ pp xs -- this gets big very quickly due to redundancy
  let
    (defns0, m) = runCExpMap xs
    n = next m
    cexps0 :: [(Integer, CExp)] = sort $ map swap $ M.toList $ hmapR m
    bvs :: Vector [Var] = constructB argsCExp n cexps0
    defns :: [Defn AExp] = map (updFBVarsDefn bvs) defns0
    cexps :: [(Free, CExp)] = map (\(p, q) -> (toFree bvs p (typeof q), updFBVarsCExp bvs q)) cexps0

  in (cexps, defns)

printC :: ([(Free, CExp)], [Defn AExp]) -> IO ()
printC (cexps, defns) = do
  print $ vcat $ map ppDefnDeclC defns ++ map ppCExpC cexps ++ map ppDefnC defns

printPP :: ([(Free, CExp)], [Defn AExp]) -> IO ()
printPP (cexps, defns) = do
  print $ vcat $ map pp cexps ++ map (\x -> parens (pp x <+> ppDefnBody x)) defns

swap (x,y) = (y,x)

singleton a = [a]

fidIdx x y = V.unsafeIndex x $ fromIntegral $ fid y
uidIdx x y = V.unsafeIndex x $ fromIntegral $ uid y

argsAExp :: Vector [Var] -> AExp -> [Var]
argsAExp arr = \case
  Right v -> case v of
    FVar a -> fidIdx arr a
    _ -> singleton v
  Left{} -> []

argsCExp :: Vector [Var] -> CExp -> [Var]
argsCExp arr x = sort $ nub $ case x of
  AExp{} -> unused "argsCExp"
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
