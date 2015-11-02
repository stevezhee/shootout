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

import Data.Hashable
import GHC.Generics (Generic)
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint hiding (int, empty)
import Data.Traversable

import Debug.Trace
-- import           Control.Applicative hiding (empty)
-- import           Control.Exception
import Control.Monad.State hiding (mapM, sequence)
import Control.Monad
import qualified Data.HashMap.Strict as M
import Data.List (sort, intersperse, (\\), nub, union)
--   hiding (insert, lookup, elem, maximum, concatMap, mapAccumR, foldr, concat)
import Data.Maybe
import Data.Char (toLower)
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
import Data.Foldable
-- import Data.Bits
import Data.Graph hiding (Tree, Node)
import Data.GraphViz hiding (Int)
import System.Process hiding (env)
import qualified Data.Text.Lazy.IO as T
import Data.Ratio
import Data.Bifunctor

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
instance (PP a, PP b, PP c) => PP (a,b,c) where pp (a,b,c) = parens (pp a <+> pp b <+> pp c)
                                        
isBinop = either (flip elem [Add, Mul, Sub, Div, Rem, And, Or, Xor, Shl, Lshr, Ashr, Eq, Ne, Gt, Lt, Gte, Lte] . uop) (\_ -> False)

class Typed a where typeof :: a -> Type
class PP a where pp :: a -> Doc

data Type
  = TSInt Integer | TUInt Integer | TFloating Integer | TVector Integer Type -- | TAggregate
  deriving (Show, Eq, Ord, Generic)
instance Hashable Type

instance PPC Type where
  ppc = \case
    TSInt a -> text "int" <> integer a <> text "_t"
    TUInt a -> text "uint" <> integer a <> text "_t"
    TFloating a -> text "float" <> integer a <> text "_t"
    
instance PP Double where pp = double
                         
data Const
  = Rat Type Rational
  -- | Undef Type
  deriving (Show, Eq, Generic, Ord)
instance Hashable Const
instance PP Const where
  pp = \case
    Rat _ b -> pp b
    -- Undef _ -> text "undef"
instance Typed Const where
  typeof = \case
    Rat a _ -> a
    -- Undef a -> a

instance Typed Float where typeof _ = TFloating 32
instance Typed Double where typeof _ = TFloating 64
instance Typed Int where typeof _ = TSInt 32
instance Typed Bool where typeof _ = TUInt 1 -- BAL: make general for enums
instance Typed Word where typeof _ = TUInt 32

instance PP Integer where pp = integer
instance PP Rational where
  pp x | denominator x == 1 = pp (numerator x)
       | otherwise = pp (fromRational x :: Double)

newtype Label = Label{ lid :: Integer } deriving (Show, Eq, Num, Ord, Generic, Enum)
instance Hashable Label
instance PP Label where pp x = text "L" <> pp (lid x)

data User = User{ uid :: Integer, utype :: Type } deriving (Show, Eq, Ord, Generic)
instance Hashable User
instance PP User where pp x = text "U" <> pp (uid x)
instance Typed User where typeof = utype

data Free = Free{ fid :: Integer, ftype :: Type, fbvars :: [Var] } deriving (Show, Eq, Ord, Generic)
instance Hashable Free
instance PP Free where pp x = text "F" <> pp (fid x)
instance Typed Free where typeof = ftype
  
data Bound = Bound{ bid :: Integer, btype :: Type } deriving (Show, Eq, Ord, Generic)
instance Hashable Bound
instance Typed Bound where typeof = btype
instance PP Bound where pp x = text "B" <> pp (bid x)

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

instance PPC Const where ppc = pp

instance PPC Free where ppc x = pp x <> parens (ppCommas $ map ppc $ fbvars x)
  
instance PPC Var where
  ppc = \case
    FVar a -> ppc a
    x -> pp x
    
instance PPC AExp where
  ppc = \case
    CAExp a -> ppc a
    VAExp a -> ppc a
      
instance (PPC a, Typed a) => PPC (Expr a) where
  ppc = \case
    AExp a -> ppc a
    App a bs -> ppAppC a bs
    Switch a bs c -> ppSwitchC (ppc a) (map ppc bs) (ppc c)
    While _ a bs c -> ppWhileC (ppc a) [ (ppc (typeof p), (pp p, (ppc q, ppc r))) | (p, (q, r)) <- bs ] (pp c)
      
data AExp
  = CAExp Const
  | VAExp Var
  deriving (Show, Eq, Generic, Ord)
instance Hashable AExp
instance PP AExp where
  pp = \case
    CAExp a -> pp a
    VAExp a -> pp a
instance Typed AExp where
  typeof = \case
    CAExp a -> typeof a
    VAExp a -> typeof a

app o t = Exp . App (Left $ Op o t)
rat t = Exp . AExp . CAExp . Rat t

data Defn a = Defn{ did :: String, dbvars :: [Var], body :: a } deriving (Show, Eq, Ord, Generic)
instance Hashable (Defn AExp)

instance Typed a => Typed (Defn a) where typeof = typeof . body
                               
data Expr a
  = AExp AExp
  | App (Either Op (Defn a)) [a]
  | Switch a [a] a
  | While Integer a [(Bound, (a, a))] Bound
  deriving (Show, Eq, Ord, Generic)
instance Hashable CExp

type CExp = Expr AExp
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
  = Add | Mul | Sub | Div | Rem | And | Or | Xor | Shl | Lshr | Ashr | Eq | Ne | Gt | Lt | Gte | Lte
  | Abs | Signum | Sqrt | ExpF | Log | Sin | Cos | Asin | Atan | Acos | Sinh | Cosh | Asinh | Atanh | Acosh
  | InsertElement | ExtractElement | ShuffleVector
  | Cast
  deriving (Show, Eq, Ord, Generic)
instance Hashable UOp
instance PP UOp where
  pp = text . \case
    Add -> "+"
    Mul -> "*"
    Sub -> "-"
    Div -> "/"
    Rem -> "%"
    And -> "&"
    Or -> "|"
    Xor -> "^"
    Shl -> "<<"
    Lshr -> ">>"
    Ashr -> "#>>"
    Eq -> "=="
    Ne -> "!="
    Gt -> ">"
    Lt -> "<"
    Gte -> ">="
    Lte -> "<="
    x -> map toLower $ show x

data Op = Op{ uop :: UOp, otype :: Type  } deriving (Show, Eq, Ord, Generic)
instance Hashable Op
instance Typed Op where typeof = otype
instance PP Op where pp = pp . uop

binop :: (Rational -> Rational -> Rational) -> [Rational] -> Rational
binop f = \case
  [a,b] -> f a b
  _ -> error "binop"

debug x = trace ("debug:" ++ show x) x

binopi :: (Integer -> Integer -> Integer) -> [Rational] -> Rational
binopi f = \case
  [a,b] -> toRational $ f (numerator a) (numerator b)
  _ -> error "binop"

binop2 :: Type -> (Rational -> Rational -> Rational) -> (Integer -> Integer -> Integer) -> [Rational] -> Rational
binop2 t f g = case t of
  TFloating{} -> binop f
  _ -> binopi g

cmpop :: (Rational -> Rational -> Bool) -> [Rational] -> Rational
cmpop f = \case
  [a,b] -> toRational $ fromEnum $ f a b
  _ -> error "cmpop"

frem = unused "frem"

optbl :: Type -> [(UOp, [Rational] -> Rational)]
optbl t =
  (Add, binop (+)) :
  (Sub, binop (-)) :
  (Mul, binop (*)) :
  (Div, binop2 t (/) div) :
  (Rem, binop2 t frem rem) :
  (Eq, cmpop (==)) :
  (Ne, cmpop (/=)) :
  (Lt, cmpop (<)) :
  (Gt, cmpop (>)) :
  (Lte, cmpop (<=)) :
  (Gte, cmpop (>=)) :
  []

data ESt = ESt{ env :: [(Var, Rational)] } deriving Show

instance PP ESt where pp = vcat . map pp . env

type Eval a = State ESt a

evalBound b e = evalExp e >>= \v -> modify $ \st -> st{ env = [(BVar b, v)] ++ env st }

evalExp = eval . toExpr

eval :: Expr Exp -> Eval Rational
eval = \case
  AExp a -> case a of
    CAExp b -> case b of
      Rat _ r -> return r
      -- Undef _ -> error "eval:undef"
    VAExp b -> gets env >>= return . fromMaybe (unused "eval:VAExp") . lookup b
  App (Left a) bs -> let f = fromMaybe (unused "eval:App") (lookup (uop a) $ optbl $ otype a) in mapM evalExp bs >>= return . f
  Switch a bs c -> do
    i <- evalExp a >>= return . fromInteger . numerator
    evalExp $ if i < length bs then (bs !! i) else c
  While _ a t c -> do
    mapM_ (\(b, (e,_)) -> evalBound b e) t
    let go = do
          r <- evalExp a >>= return . toEnum . fromInteger . numerator
          if r
             then do
               mapM_ (\(b, (_,e)) -> evalBound b e) t
               go
             else eval $ bvar c
    go

runEval :: Exp -> (Rational, ESt)
runEval x = flip runState (ESt []) $ evalExp x

maximumBV :: (Foldable t, Functor t) => t Exp -> Integer
maximumBV = maximum . fmap maxBV

maxBV :: Exp -> Integer
maxBV x = case toExpr x of
  AExp _ -> 0 -- not a typo, see http://pchiusano.github.io/2014-06-20/simple-debruijn-alternative.html
               -- or http://www.cse.chalmers.se/~emax/documents/axelsson2013using.pdf
  App _ bs -> maximumBV bs
  Switch a bs c -> maximumBV (a : c : bs)
  While n _ _ _ -> n
  
zipWithTree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c -- trees must have the same shape
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
while x f = fmap (Exp . (While (n+m) e $ sort $ toList $ zipTree xb $ zipTree x x1)) xb
  where
    m = fromIntegral $ length x
    n = maximum [maximumBV x, maxBV e, maximumBV x1]
    (e, x1) = f x0
    x0 = fmap (Exp . bvar) xb
    xb =
      snd $ mapAccumR (\(b:bs) j -> (bs, Bound b (typeof j))) [n..] x

bvar :: Bound -> Expr Exp
bvar = AExp . VAExp . BVar

func :: String -> Exp -> Tree Exp -> Exp
func s x e = Exp $ App (Right $ Defn s bvs x) $ toList e
  where
    bvs = [ v | Exp (AExp (VAExp v)) <- toList $ instantiate $ fmap typeof e ]

instantiate :: Tree Type -> Tree Exp
instantiate = snd . mapAccumL uvar 0

uvar :: Integer -> Type -> (Integer, Exp)
uvar x y = (succ x, Exp $ AExp $ VAExp $ UVar $ User x y)

tbool = TUInt 1

swap (x,y) = (y,x)

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
defToAExp (Defn a b c) = Defn a b <$> toAExp c

toAExp :: Exp -> F AExp
toAExp x0 = do
  x <- toCExp x0
  case toExpr x of
    AExp a -> return a
    _ -> do
      tbl <- get
      let (a, tbl') = insertR x tbl
      modify $ \_ -> tbl'
      return $ VAExp $ FVar $ Free a (typeof x) []

toCExpDefn :: Defn Exp -> F (Defn AExp)
toCExpDefn (Defn a bs c) = Defn a bs <$> toAExp c

foo :: Either Op (Defn Exp) -> Either (F Op) (F (Defn AExp))
foo = bimap return toCExpDefn

toCExp :: Exp -> F CExp
toCExp x = case toExpr x of
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
ppSwitchC x ys z = ppBlockC (text "switch" <> parens x) $ map ppCaseC (zip [0..] ys) ++ [ ppDefaultC z ]

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
    
ppParens x = case toExpr x of
  AExp{} -> pp x
  _ -> parens $ pp x

ppStore x y = pp x <+> text ":=" <+> pp y

class IsExpr a where
  toExpr :: a -> Expr Exp

toExp = Exp . AExp

instance IsExpr Exp where toExpr = unExp
instance IsExpr CExp where
  toExpr x = case x of
    AExp a -> AExp a
    App a bs -> App (bimap id (\(Defn c ds e) -> Defn c ds $ toExp e) a) $ map toExp bs
    Switch a bs c -> Switch (toExp a) (map toExp bs) (toExp c)
    While a b cs d -> While a (toExp b) [ (p, (toExp q, toExp r)) | (p, (q,r)) <- cs ] d
    
instance PP CExp where pp = pp . toExpr
instance PP Exp where pp = pp . toExpr

ppCommas = hcat . intersperse (text ", ")

isCast = either ((==) Cast . uop) (\_ -> False)

instance (PP a, PP b) => PP (Either a b) where pp = either pp pp
                                            
ppAppC :: (PPC a, Typed a) => Either Op (Defn a) -> [a] -> Doc
ppAppC x ys = ppReturnC $ case () of
  () | isBinop x -> b0 <+> pp x <+> b1
     | isCast x -> parens (ppc (typeof x)) <> b0
     | otherwise -> pp x <> parens (ppCommas bs)
  where
    bs = map ppc ys
    b0:_ = bs
    _:b1:_ = bs

instance (PP a, IsExpr a) => PP (Expr a) where
  pp = \case
    Switch a bs c -> ppSwitch a bs c
    App a bs
    --   | a == ExtractElement -> pp b0 <> brackets (pp b1)
    --   | a == InsertElement -> pp b0 <> brackets (pp b2) <+> text "<-" <+> pp b1
      | isBinop a -> ppParens b0 <+> pp a <+> ppParens b1
      | otherwise -> pp a <+> hsep (map ppParens bs)
      where
        b0:_ = bs
        _:b1:_ = bs
        _:_:b2:_ = bs
    AExp a -> pp a
    While _ a bs c -> ppWhile a bs c
    -- EPhi a b -> hsep [text "phi", pp a, ppParens b]

unused = error . (++) "unused:"

data MapR a b = MapR
  { hmapR :: M.HashMap b a
  , next :: a
  } deriving Show

ppCExpC :: (Free, CExp) -> Doc
ppCExpC (x, y) = ppProcC (text "static") x (pp x) (fbvars x) (ppc y)

instance PP (Defn a) where pp = text . did
                            
ppDefnC :: Defn AExp -> Doc
ppDefnC x = ppProcC PP.empty x (pp x) (dbvars x) (ppReturnC $ ppc $ body x)

ppProcC :: (Typed a) => Doc -> a -> Doc -> [Var] -> Doc -> Doc
ppProcC pre x y zs a = ppBlockC (pre <+> ppc (typeof x) <+> y <> parens (ppCommas $ map ppVarDeclC zs)) [a]

runCExpMap :: [Def] -> ([Defn AExp], MapR Integer CExp)
runCExpMap = flip runState (MapR M.empty 0) . mapM defToAExp

updFBVarsCExp :: Vector [Var] -> CExp -> CExp
updFBVarsCExp bvs x = case x of
  AExp a -> AExp $ f a
  App a bs -> App (bimap id (updFBVarsDefn bvs) a) $ map f bs
  Switch a bs c -> Switch (f a) (map f bs) (f c)
  While a b cs d -> While a (f b) [ (p, (f q, f r)) | (p, (q, r)) <- cs ] d
  where
    f = updFBVarsAExp bvs
    
updFBVarsAExp :: Vector [Var] -> AExp -> AExp
updFBVarsAExp bvs = \case
  VAExp (FVar a) -> VAExp $ FVar a{ fbvars = fidIdx bvs a }
  x -> x
  
updFBVarsDefn :: Vector [Var] -> Defn AExp -> Defn AExp
updFBVarsDefn bvs x = x{ body = updFBVarsAExp bvs $ body x }

toFree :: Vector [Var] -> Integer -> Type -> Free
toFree bvs i t = let v = Free i t $ fidIdx bvs v in v
  
compile :: String -> [Def] -> IO ()
compile fn xs = do
  -- print $ pp xs -- this gets big very quickly due to redundancy
  let
    (defns0, m) = runCExpMap xs
    n = next m
    cexps0 :: [(Integer, CExp)] = sort $ map swap $ M.toList $ hmapR m
    bvs :: Vector [Var] = constructB argsCExp n cexps0
    defns = map (updFBVarsDefn bvs) defns0
    cexps = map (\(p, q) -> (toFree bvs p (typeof q), updFBVarsCExp bvs q)) cexps0

  print $ vcat $ map ppCExpC cexps ++ map ppDefnC defns      

singleton a = [a]

fidIdx x y = V.unsafeIndex x $ fromIntegral $ fid y
  
argsAExp :: Vector [Var] -> AExp -> [Var]
argsAExp arr = \case
  VAExp v -> case v of
    FVar a -> fidIdx arr a
    _ -> singleton v
  CAExp{} -> []

argsCExp :: Vector [Var] -> CExp -> [Var]
argsCExp arr x = sort $ nub $ case x of
  AExp{} -> unused "argsCExp"
  App a bs -> either (\_ -> []) dbvars a ++ go bs
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
