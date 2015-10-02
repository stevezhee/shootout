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
import qualified Data.HashMap.Strict as M
import Data.List (sort)
--   hiding (insert, lookup, elem, maximum, concatMap, mapAccumR, foldr, concat)
import Data.Maybe
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
import Data.Foldable
-- import Data.Bits
import Data.Graph hiding (Tree, Node)
import Data.GraphViz hiding (Int)
import System.Process hiding (env)
import qualified Data.Text.Lazy.IO as T
import Data.Ratio

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

-- llvmFunction :: (A.Type, String, [User], [BasicBlock]) -> Definition
-- llvmFunction (t, n, us, bs) = GlobalDefinition functionDefaults
--   { returnType = t
--   , name = Name n
--   , parameters = ([Parameter (llvmTypeof u) (llvmName u) [] | u <- us], False)
--   , basicBlocks = bs
--   }

-- llvmSqrt = "llvm.sqrt.f64"

-- llvmIntrinsic (a, b, cs) = (llvmType a, b, [User t i | (t,i) <- zip cs [0..]], [])

-- intrinsics :: [(Type, String, [Type])]
-- intrinsics =
--   [(TDouble, llvmSqrt, [TDouble])]

-- llvmFilename = "t.ll"
-- llvmModule xs = A.defaultModule
--   { moduleName = llvmFilename
--   , moduleDefinitions = map llvmFunction $ bs ++ xs
--   }
--   where
--   bs = map llvmIntrinsic intrinsics

-- ppName :: PP a => a -> Name
-- ppName = Name . show . pp

-- class LLVMName a where llvmName :: a -> Name
-- instance LLVMName Label where  llvmName (Label a) = UnName $ fromIntegral a
-- instance LLVMName Free where llvmName = ppName
-- instance LLVMName Bound where llvmName = ppName
-- instance LLVMName User where llvmName = ppName

-- mapPair f g (x,y) = (f x, g y)

-- llvmPhi :: (Bound, [(AExp, Label)]) -> Named A.Instruction
-- llvmPhi (x, ys) =
--   llvmName x := A.Phi (llvmTypeof x) (map (mapPair llvmOperand llvmName) ys) []
  
-- llvmInsn :: (Free, (Op, [AExp])) -> Named A.Instruction
-- llvmInsn (x, (y, zs)) =
--   llvmName x := (llvmOp (typeof $ head zs) y) (map llvmOperand zs)


-- instance Typed AExp where
--   typeof = \case
--     Int t _ -> t
--     Rat t _ -> t
--     FVar a -> typeof a
--     BVar a -> typeof a
--     UVar a -> typeof a
--     Undef a -> a
    
-- llvmTypeof :: Typed a => a -> A.Type
-- llvmTypeof = llvmType . typeof

-- llvmType = \case
--   TSInt a -> tint a
--   TUInt a -> tint a
--   TDouble -> A.FloatingPointType 64 A.IEEE
--   TVector a b -> A.VectorType (fromInteger a) $ llvmType b
--   where
--     tint = A.IntegerType . fromIntegral
  

-- llvmOp :: Type -> Op -> ([Operand] -> A.Instruction)
-- llvmOp t = \case
--   Add -> binary (nowrap A.Add) (fast A.FAdd)
--   Sub -> binary (nowrap A.Sub) (fast A.FSub)
--   Mul -> binary (nowrap A.Mul) (fast A.FMul)
--   Or -> intop A.Or
--   And -> intop A.And
--   Xor -> intop A.Xor
--   Shl -> intop (wrap A.Shl)
--   Lshr -> intop (notExact A.LShr)
--   Ashr -> intop (notExact A.AShr)
--   Div -> ubinary (exct A.UDiv) (exct A.SDiv) (fast A.FDiv)
--   Rem -> ubinary A.URem A.SRem (fast A.FRem)
--   Eq -> cmp IP.EQ FP.OEQ
--   Ne -> cmp IP.NE FP.ONE
--   Gt -> ucmp IP.UGT IP.SGT FP.OGT
--   Lt -> ucmp IP.ULT IP.SLT FP.OLT
--   Gte -> ucmp IP.UGE IP.SGE FP.OGE
--   Lte -> ucmp IP.ULE IP.SLE FP.OLE
--   Sqrt -> uunary (unused "Sqrt:unsigned") (unused "Sqrt:signed") (call1 llvmSqrt)
--   ExtractElement -> \[a,b] -> A.ExtractElement a b []
--   InsertElement -> \[a,b,c] -> A.InsertElement a b c []
--   ToFP a -> uunary (flip A.UIToFP t') (flip A.SIToFP t') (unused "ToFP:double")
--     where t' = llvmType a
--   _ -> error $ "llvmOp:" ++ show x
--   where
--     intop f = binary f $ unused "llvmOp:intop"
--     notExact f = f False
--     rev o = \[a,b] -> llvmOp t o [b,a]
--     nowrap f = f True True
--     wrap f = f False False
--     fast f = f UnsafeAlgebra
--     exct f = f False
--     binary f g = ubinary f f g
--     uunary f g h = \[a] -> case t of
--       TUInt{} -> f a []
--       TSInt{} -> g a []
--       TDouble -> h a []

--     ubinary f g h = \[a,b] -> case t of
--       TUInt{} -> f a b []
--       TSInt{} -> g a b []
--       TDouble -> h a b []
--     cmp f g = ucmp f f g
--     ucmp f g h = ubinary (A.ICmp f) (A.ICmp g) (A.FCmp h)
--     call n bs =
--       A.Call False CC.C []
--         (Right $ ConstantOperand $ C.GlobalReference (llvmType t) $ Name n)
--         (map (flip pair []) bs) []
--     call1 n b = call n [b]

-- llvmOperand :: AExp -> Operand
-- llvmOperand = \case
--   Int t a -> ConstantOperand $ case t of
--     TUInt b -> C.Int (fromIntegral b) a
--     TSInt b -> C.Int (fromIntegral b) a
--     TDouble -> C.Float $ F.Double $ fromIntegral a
--     _ -> unused $ "llvmOperand:Int:" ++ show t
--   Rat t a -> ConstantOperand $ C.Float $ case t of
--     TDouble -> F.Double $ fromRational a
--     _ -> unused $ "llvmOperand:Rat:" ++ show t
--   FVar a -> ref a
--   BVar a -> ref a
--   UVar a -> ref a
--   Undef a -> ConstantOperand $ C.Undef t
--   where
--     ref a = LocalReference t (llvmName a)
--     t = llvmTypeof x

-- llvmTerminator x = Do $ case x of
--   Jump a -> A.Br (llvmName a) []
--   Switch a bs c ->
--     A.Switch (llvmOperand a) (llvmName c) [ (f n, llvmName l)
--                                            | (n,l) <- zip [0 ..] bs] []
--     where
--       f n = case typeof a of
--         TSInt a -> C.Int (fromIntegral a) n
--         TUInt a -> C.Int (fromIntegral a) n
--         TDouble -> C.Float $ F.Double $ fromIntegral n
--   Return a -> A.Ret (Just $ llvmOperand a) []

-- llvmBlock :: Block -> BasicBlock
-- llvmBlock (Block a bs cs d) =
--   BasicBlock (llvmName a) (map llvmPhi bs ++ map llvmInsn cs) (llvmTerminator d)


instance PP a => PP [a] where pp = vcat . map pp

instance (PP a, PP b) => PP (a,b) where pp (a,b) = parens (pp a <+> pp b)
isBinop = flip elem [Add, Mul, Sub, Div, Rem, And, Or, Xor, Shl, Lshr, Ashr, Eq, Ne, Gt, Lt, Gte, Lte] . uop
-- type NumBV = Integer

class Typed a where typeof :: a -> Type
class PP a where pp :: a -> Doc

data Type
  = TSInt Integer | TUInt Integer | TFloating Integer | TAggregate | TVector Integer Type
  deriving (Show, Eq, Ord, Generic)
instance Hashable Type

instance PP Double where pp = double
                         
data Const
  = Rat Type Rational
  | Undef Type
  deriving (Show, Eq, Generic, Ord)
instance Hashable Const
instance PP Const where
  pp = \case
    Rat _ b -> pp b
    Undef _ -> text "undef"
instance Typed Const where
  typeof = \case
    Rat a _ -> a
    Undef a -> a

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

data User = User{ utype :: Type, uid :: Integer } deriving (Show, Eq, Ord, Generic)
instance Hashable User
instance PP User where pp x = text "U" <> pp (uid x)
instance Typed User where typeof = utype

data Free = Free{ ftype :: Type, fid :: Integer } deriving (Show, Eq, Ord, Generic)
instance Hashable Free
instance PP Free where pp x = text "F" <> pp (fid x)
instance Typed Free where typeof = ftype
  
data Bound = Bound{ btype :: Type, blabel :: Maybe Label, bid :: Integer } deriving (Show, Eq, Ord, Generic)
instance Hashable Bound
instance Typed Bound where typeof = btype
instance PP Bound where
  pp (Bound _ m a) = text "B" <> d <> pp a
    where
      d = maybe PP.empty (\i -> pp (lid i) <> text ".") m

data Var
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

app o t = Exp . App (Op o t)
rat t = Exp . AExp . CAExp . Rat t

data Expr a
  = AExp AExp
  | App Op [a]
  | Switch a [a] a
  | While Integer a [(Bound, (a, a))] Bound
  deriving (Show, Eq, Ord, Generic)
instance Hashable (Expr AExp)

newtype CExp = CExp{ unCExp :: Expr AExp } deriving (Show, Eq, Ord, Generic)
instance Hashable CExp
newtype Exp = Exp{ unExp :: Expr Exp } deriving (Show, Eq, Ord)

-- data Exp
--   = AExp AExp
--   | App Op [Exp]
--   | Switch Exp [Exp] Exp
--   | While Integer Exp [(Bound, (Exp, Exp))] Bound
--   deriving (Show, Eq, Ord)

instance Typed Exp where typeof = typeof . unExp
instance Typed CExp where typeof = typeof . unCExp

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
  | ToFP
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
    x -> show x

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

-- tt = while t tbody

-- tbody = \(Node [Leaf e, Leaf f]) -> (e `lt32` int32 42, Node [Leaf $ add32 (int32 1) e, Leaf $ add32 f e])

-- add32 x y = App (Op Add tsint32) [x,y]
-- lt32 x y = App (Op Lt tbool) [x,y]

-- tsint32 = TSInt 32
-- int32 x = AExp $ CAExp $ Rat tsint32 x

-- t = while (Node [Leaf $ int32 0, Leaf $ int32 4]) tbody

-- ttt = while (Leaf $ int32 0) $ \(Leaf e) -> (e `lt32` int32 42, Leaf $ add32 (int32 1) e)

evalExp = eval . toExpr

eval :: Expr Exp -> Eval Rational
eval = \case
  AExp a -> case a of
    CAExp b -> case b of
      Rat _ r -> return r
      Undef _ -> error "eval:undef"
    VAExp b -> gets env >>= return . fromMaybe (unused "eval:VAExp") . lookup b
  App a bs -> let f = fromMaybe (unused "eval:App") (lookup (uop a) $ optbl $ otype a) in mapM evalExp bs >>= return . f
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
      snd $ mapAccumR (\(b:bs) j -> (bs, Bound (typeof j) Nothing b)) [n..] x

bvar :: Bound -> Expr Exp
bvar = AExp . VAExp . BVar
    
-- -- while :: Tree Exp -> (Tree Exp -> (Exp, Tree Exp)) -> Tree Exp
-- -- while xs f = fmap (\(v, _) -> EPhi v w) t
-- --   -- ^ BAL: identify unused loop variables (remove(warning?) or error)
-- --   where
-- --     w = While (n + m) e t
-- --     t = zipTree vs $ fmap (\(a,b) -> [a,b]) $ zipTree xs ys
-- --     m = genericLength $ toList xs
-- --     (e, ys) = f $ fmap (AExp . BVar) vs
-- --     vs = bound n xs
-- --     n = maximumBV (e : toList xs ++ toList ys)
    
-- -- switch :: Exp -> [Tree Exp] -> Tree Exp -> Tree Exp
-- -- switch x ys z = fmap (flip EPhi $ Switch (n + m) vs x ys z) vs
-- --   where
-- --     m = genericLength $ toList z
-- --     vs = bound n z
-- --     n = maximumBV (x : toList z ++ concatMap toList ys)

  -- | Switch NumBV (Tree Bound) Exp [Tree Exp] (Tree Exp)
  -- | While NumBV Exp (Tree (Phi Exp))
  -- | EPhi Bound Exp

tbool = TUInt 1

-- instance Typed Exp where
--   typeof = \case
--     AExp a -> typeof a
--     App _ a bs -> typeofOp a bs
--     Switch{} -> TAggregate
--     While{} -> TAggregate
--     EPhi a _ -> typeof a

-- instance Typed CExp where typeof = typeof . fromCExp

-- type Phi a = (Bound, [a])

-- data CExp
--   = CAExp AExp
--   | COp Op [AExp]
--   | CSwitch [Bound] AExp [[AExp]] [AExp]
--   | CWhile AExp [Phi AExp]
--   | CPhi Bound AExp
--   deriving (Show, Eq, Generic, Ord)
-- instance Hashable CExp

-- {-
-- -- v = op es
-- <prev computing>
-- v = op es
-- <next computing> with continuation whatever got passed in


-- push (v = op es) onto the stack

-- -}

-- {-
-- -- v = switch a bs b
-- -- if 'a' is a constant then reduce to the appropriate b
-- <prev computing>
-- <compute a> with continuation (switch a (map lbl bs) (lbl b))

-- lbl 0:
-- <compute b0> with continuation (goto lbl end:)

-- ...

-- lbl end:
-- v = phi $ zip (b:bs) (lbl b : map lbl bs)
-- <next computing> with continuation whatever got passed in

-- -}

-- lookupFree :: Free -> N (Maybe CExp)
-- lookupFree x = flip (!) (fid x) <$> gets fvars

-- lookupBound :: Bound -> N (Maybe Label)
-- lookupBound x = flip (!) (bid x) <$> gets bvars

-- pushTerm :: Terminator -> N ()
-- pushTerm x = modify $ \st ->
--   st{ blocks = let b:bs = blocks st in b{ term = x, insns = reverse $ insns b } : bs }

-- pushLabel :: Label -> [(Bound, [(AExp, Label)])] -> N ()
-- pushLabel lbl ps = modify $ \st ->
--   st{ blocks = Block lbl (map f ps) [] (unused "pushLabel") : blocks st }
--   where
--     f (a,b) = (nameBound a $ Just lbl, b)

-- freshLabel :: N Label
-- freshLabel = do
--   lbl <- gets nextLabel
--   modify $ \st -> st{ nextLabel = succ lbl }
--   return lbl

-- -- freshBound :: N Integer
-- -- freshBound = do
-- --   b <- gets nextBound
-- --   modify $ \st -> st{ nextBound = succ b }
-- --   return b

-- currentLabel :: N Label
-- currentLabel = label . head <$> gets blocks

-- sortByFst :: (Ord a) => [(a,b)] -> [(a,b)]
-- sortByFst = sortBy (\a b -> compare (fst a) (fst b))

-- groupByFst :: (Eq a, Ord a) => [(a,b)] -> [(a, [b])]
-- groupByFst =
--   map (\bs -> (fst $ head bs, map snd bs)) .
--   groupBy (\a b -> fst a == fst b) . sortByFst

-- nameBound :: Bound -> Maybe Label -> Bound
-- nameBound x mlbl = x{ blabel = mlbl }

-- pushFree n mx = modify $ \st -> st{ fvars = fvars st // [(fid n, mx)] }
-- pushBounds lbl xs =
--   modify $ \st -> st{ bvars = bvars st // [(bid r, Just lbl) | r <- xs ] }

-- computes :: [AExp] -> N [AExp]
-- computes = mapM compute
  
-- compute :: AExp -> N AExp
-- compute = \case
--   UVar a -> do
--     modify $ \st -> st{ uvars = S.insert a $ uvars st }
--     return x
--   FVar n -> do
--     let ok vx = do
--           pushFree n $ Just $ CAExp vx
--           return vx
--     Just y <- lookupFree n
--     case y of
--       CAExp a -> return a
--       COp a bs -> do
--         vbs <- computes bs
--         modify $ \st -> st{ blocks = let b:bs = blocks st in b{ insns = (n, (a,vbs)) : insns b } : bs }
--         ok x
--       CPhi a b -> do
--         computeStmt b
--         compute $ BVar a
--       _ -> unused "compute:FVar"
--   BVar a -> (BVar . nameBound a) <$> lookupBound a
--   _ -> return x

-- computeStmt :: AExp -> N ()
-- computeStmt = \case
--   FVar n -> do
--     my <- lookupFree n
--     case my of
--       Nothing -> return ()
--       Just y -> do
--         pushFree n Nothing
--         case y of
--           CSwitch vs a bss cs -> do
--             va <- compute a
--             lbl : lbls <- mapM (const freshLabel) (cs : bss)
--             let ps = zip (cs : bss) (lbl : lbls)
--             pushTerm $ Switch va lbls lbl
    
--             end <- freshLabel
--             vpss <- flip mapM ps $ \(bs,l) -> do
--               pushLabel l []
--               vbs <- computes bs -- BAL: need a 'withMap' function
--               l' <- currentLabel
--               pushTerm $ Jump end
--               return $ zip vs $ zip vbs $ repeat l'
    
--             pushBounds end vs
--             pushLabel end $ groupByFst $ concat vpss
    
--           CWhile a bs -> do
--             vbs0 <- computes $ map (head . snd) bs
--             pre <- currentLabel
--             begin <- freshLabel
--             pushTerm $ Jump begin
    
--             pushBounds begin $ map fst bs
--             test <- freshLabel
    
--             pushLabel test []
--             va <- compute a
--             end <- freshLabel
--             body <- freshLabel        
--             pushTerm $ Switch va [end] body
    
--             pushLabel body []
--             vbs1 <- computes $ map (last . snd) bs
--             from <- currentLabel
--             pushTerm $ Jump begin
            
--             pushLabel begin
--               [ (r, [(p, pre), (q, from)])
--               | (r, p, q) <- zip3 (map fst bs) vbs0 vbs1 ]
--             pushTerm $ Jump test

--             pushLabel end []

--   _ -> unused "computeStmt"

-- {-
-- -- v = while a bs c
-- -- 'a' and 'c' must both depend on bs o.w. error
-- -- 'a' must not be constant

-- <prev computing>
-- goto begin:

-- begin:
-- phi bs
-- <compute a> with continuation (switch a [end:, body:])

-- body:
-- <compute each bs> continue from one to the next with the last one having continuation (goto begin:)

-- end:
-- <compute c>
-- <next computing where subst c for v> with continuation whatever got passed in

-- -}

-- fromCExp :: CExp -> Exp
-- fromCExp = \case
--   CAExp a -> AExp a
--   COp a bs -> App 0 a $ map AExp bs
--   CSwitch vs a bss cs ->
--     Switch 0 (listToTree vs) (AExp a) (map (fmap AExp . listToTree) bss) (fmap AExp $ listToTree cs)
--   CWhile a bs ->
--     While 0 (AExp a) (flip fmap (listToTree bs) $ \(v, [p, q]) -> (v, [AExp p, AExp q]))
--   CPhi a b -> EPhi a $ AExp b

-- listToTree :: [a] -> Tree a
-- listToTree = Node . map Leaf

-- instance PP CExp where pp = pp . fromCExp
  
-- -- evalOpRat :: Op -> [Rational] -> Rational
-- -- evalOpRat x ys = case x of
-- --   Add -> a + b
-- --   Sub -> a - b
-- --   Mul -> a * b
-- --   Div -> a / b
-- --   Sqrt -> toRational (sqrt (fromRational a) :: Double)
-- --   _ -> error $ "evalOpRat:" ++ show (x,ys)
-- --   where
-- --     a = head ys
-- --     b = head $ tail ys

-- -- evalOpInt :: Op -> [Integer] -> Integer
-- -- evalOpInt x ys = case x of
-- --   Add -> a + b
-- --   Sub -> a - b
-- --   Mul -> a * b
-- --   Div -> a `div` b
-- --   _ -> error $ "evalOpInt:" ++ show (x,ys)
-- --   where
-- --     [a,b] = ys

-- optimize = True
-- -- optimize = False

-- eqConst x y = case x of
--   Rat _ a -> a == toRational y
--   Int _ a -> a == y
--   _ -> False

-- isConst = \case
--   Int{} -> True
--   Rat{} -> True
--   _ -> False

-- constFold :: Op -> [AExp] -> AExp
-- constFold o xs = case xs of
--   [Int _ a, Int _ b] -> h a b
--   [Rat _ a, Rat _ b] -> i a b
--   [Int _ a] -> f a
--   [Rat _ a] -> g a
--   [a@Rat{}, Int _ b] -> constFold o [a, Rat t $ toRational b]
--   [Int _ a, b@Rat{}] -> constFold o [Rat t $ toRational a, b]
--   _ -> unused "constFold"
--   where
--   t = typeofOp o xs
--   f :: Integer -> AExp
--   f = case o of
--     Abs -> Int t . abs
--     Signum -> Int t . signum
--     ToFP a -> Rat a . toRational
--     _ -> g . fromInteger

--   g :: Rational -> AExp
--   g = case o of
--     Abs -> Rat t . abs
--     Signum -> Rat t . signum
--     Sqrt -> rToR sqrt
--     Exp -> rToR exp
--     Log -> rToR log
--     Sin -> rToR sin
--     Cos -> rToR cos
--     Asin -> rToR asin
--     Atan -> rToR atan
--     Acos -> rToR acos
--     Sinh -> rToR sinh
--     Cosh -> rToR cosh
--     Asinh -> rToR asinh
--     Atanh -> rToR atanh
--     Acosh -> rToR acosh
--     where
--       rToR f = Rat t . toRational . f . fromRational
--   h :: Integer -> Integer -> AExp
--   h = case o of
--     Add -> iToI (+)
--     Mul -> iToI (*)
--     Sub -> iToI (-)
--     Div -> iToI quot
--     Rem -> iToI rem
--     Eq -> toB (==)
--     Ne -> toB (/=)
--     Gt -> toB (>)
--     Lt -> toB (<)
--     Gte -> toB (>=)
--     Lte -> toB (<=)
--     And -> iToI (.&.)
--     Or -> iToI (.|.)
--     Xor -> iToI xor
--     Shl -> shft shiftL -- BAL: correct?
--     Lshr -> shft shiftR -- BAL: correct?
--     Ashr -> shft shiftR -- BAL: correct?  Even need Ashr since we have Lshr?
--     where
--       iToI f x = Int t . f x
--       shft f x y = Int t $ f x (fromInteger y)
--   toB :: (a -> a -> Bool) -> a -> a -> AExp
--   toB f x y = Int t $ toInteger $ fromEnum (f x y)
--   i :: Rational -> Rational -> AExp
--   i = case o of
--     Add -> rToR (+)
--     Mul -> rToR (*)
--     Sub -> rToR (-)
--     Div -> rToR (/)
--     Rem -> unused "Rem" -- BAL:?
--     Eq -> toB (==)
--     Ne -> toB (/=)
--     Gt -> toB (>)
--     Lt -> toB (<)
--     Gte -> toB (>=)
--     Lte -> toB (<=)
--     where
--       rToR f x = Rat t . f x

-- aexp :: Exp -> M AExp
-- aexp x = cexp x >>= toAExp

-- toAExp :: CExp -> M AExp
-- toAExp = \case
--   CAExp a -> return a
--   COp a [b, c] | isConst b && isConst c -> return $ constFold a [b, c]
--   COp Add [b, c] | b `eqConst` 0 -> return c
--   COp Add [b, c] | c `eqConst` 0 -> return b
--   COp Sub [b, c] | b == c -> return zero
--   COp Sub [b, c] | c `eqConst` 0 -> return b
--   COp Mul [b, c] | b `eqConst` 0 || c `eqConst` 0 -> return zero
--   COp Mul [b, c] | b `eqConst` 1 -> return c
--   COp Mul [b, c] | c `eqConst` 1 -> return b
--   COp Div [b, c] | c `eqConst` 0 -> error "divide by zero"
--   COp Div [b, c] | c `eqConst` 1 -> return b
--   COp Div [b, c] | b == c -> return one
--   COp Rem [b, c] | c `eqConst` 0 -> error "remainder by zero"
--   COp Rem [b, c] | c `eqConst` 1 || b == c -> return zero
--   COp a [b, c] | a `elem` [Eq, Gte, Lte] && b == c -> return true
--   COp a [b, c] | a `elem` [Ne, Gt, Lt] && b == c -> return false
--   _ -> ok
--   where
--     zero = Int (typeof x) 0
--     one = Int (typeof x) 1
--     true = one
--     false = zero
--     ok = do
--       tbl <- get
--       let (a, tbl') = insertR (canonCExp x) tbl
--       modify $ \_ -> tbl'
--       return $ FVar $ Free (typeof x) a

-- canonCExp = \case
--   COp a [b, c] | b > c -> case a of
--     Add -> f Add
--     Mul -> f Mul
--     And -> f And
--     Or -> f Or
--     Xor -> f Xor
--     Eq -> f Eq
--     Ne -> f Ne
--     Gt -> f Lt
--     Lt -> f Gt
--     Gte -> f Lte
--     Lte -> f Gte
--     _ -> x
--     where
--     f a' = COp a' [c, b]
--   _ -> x

swap (x,y) = (y,x)

-- instance (Hashable b, Eq b, Num a, PP a, PP b, Ord b, Ord a) => PP (MapR a b) where
--   pp = vcat . map pp . sort . map swap . M.toList . hmapR

lookupR :: (Hashable b, Eq b) => b -> MapR a b -> Maybe a
lookupR b = M.lookup b . hmapR

insertR :: (Hashable b, Eq b, Enum a) => b -> MapR a b -> (a, MapR a b)
insertR b tbl = case lookupR b tbl of
  Just a -> (a, tbl)
  Nothing -> (a, tbl{ next = succ a, hmapR = M.insert b a $ hmapR tbl })
    where a = next tbl

type F a = State (MapR Integer CExp) a

foo x = toAExp x

toAExp :: Exp -> F AExp
toAExp x0 = do
  x <- toCExp x0
  case toExpr x of
    AExp a -> return a
    _ -> do
      tbl <- get
      let (a, tbl') = insertR x tbl
      modify $ \_ -> tbl'
      return $ VAExp $ FVar $ Free (typeof x) a

toCExp :: Exp -> F CExp
toCExp x = CExp <$> case toExpr x of
  AExp a -> return $ AExp a
  App a bs -> App a <$> mapM toAExp bs
  Switch a bs c -> Switch <$> toAExp a <*> mapM toAExp bs <*> toAExp c
  While a b cs d -> While a <$> toAExp b <*> mapM f cs <*> return d
    where f (p, (q, r)) = (,) p <$> ((,) <$> toAExp q <*> toAExp r)

-- cexp :: Exp -> M CExp
-- cexp = \case
--   AExp a -> return $ CAExp a
--   App _ b cs -> COp b <$> mapM aexp cs
--   Switch _ vs b cs d -> do
--     cs' <- mapM (mapM aexp . toList) cs
--     d' <- mapM aexp (toList d)
--     case reverse $ dropWhile ((==) d') $ reverse cs' of
--       [] -> return $ CSwitch (toList vs) (Undef $ TUInt 1) [] d' -- BAL: haven't tested this yet
--       cs'' -> aexp b >>= \b' -> return $ CSwitch (toList vs) b' cs'' d'
    
--   While _ a bs -> CWhile <$> aexp a <*> mapM f (toList bs)
--     where f (v, ps) = pair v <$> mapM aexp ps
--   EPhi a b -> CPhi a <$> aexp b

-- instance PP Block where
--   pp (Block a b c d) = vcat [pp a, nest 2 $ vcat $ map pp b ++ map pp c ++ [pp d]]

-- instance PP [Block] where pp = vcat . map pp

ppSwitch a bs c = vcat [text "switch" <+> ppParens a, nest 2 $ pp $ bs ++ [c]]

ppParens :: (PP a, IsExpr a) => a -> Doc
ppParens x = case toExpr x of
  AExp{} -> pp x
  _ -> parens $ pp x

ppStore x y = pp x <+> text ":=" <+> pp y

class IsExpr a where
  toExpr :: a -> Expr Exp

toExp = Exp . AExp

instance IsExpr Exp where toExpr = unExp
instance IsExpr CExp where
  toExpr x = case unCExp x of
    AExp a -> AExp a
    App a bs -> App a $ map toExp bs
    Switch a bs c -> Switch (toExp a) (map toExp bs) (toExp c)
    While a b cs d -> While a (toExp b) [ (p, (toExp q, toExp r)) | (p, (q,r)) <- cs ] d

instance PP CExp where pp = pp . toExpr
instance PP Exp where pp = pp . toExpr
  
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
    While _ a bs c ->
      vcat [ pp c <+> text "from"
           , nest 2 $ vcat [ vcat $ map (\(p, (q, _)) -> ppStore p q) bs
                           , text "while" <+> ppParens a
                           , nest 2 $ vcat $ map (\(p, (_, r)) -> ppStore p r) bs
                           ]
           ]
    -- EPhi a b -> hsep [text "phi", pp a, ppParens b]

-- instance (Foldable t, PP a) => PP (t a) where pp = pp . toList

-- instance (PP a, PP b, PP c, PP d) => PP (a,b,c,d) where
--   pp (a,b,c,d) = parens (vcat [pp a, pp b, pp c, pp d])

-- maximumBV :: [Exp] -> Integer
-- maximumBV = maximum . map maxBV

-- maxBV :: Exp -> Integer
-- maxBV = \case
--   Switch i _ _ _ _ -> i
--   App i _ _ -> i
--   AExp _ -> 0
--   While i _ _ -> i
--   EPhi _ b -> maxBV b

-- binop :: Op -> Exp -> Exp -> Exp
-- binop o x y = App (maximumBV [x,y]) o [x,y]

-- ternop :: Op -> Exp -> Exp -> Exp -> Exp
-- ternop o x y z = App (maximumBV [x,y,z]) o [x,y,z]

-- unop :: Op -> Exp -> Exp
-- unop o x = App (maxBV x) o [x]

unused = error . (++) "unused:"

-- var t = AExp . UVar . User t
    
-- bound :: Typed a => Integer -> Tree a -> Tree Bound
-- bound n = snd . mapAccumR (\(w:ws) x -> (ws, Bound (typeof x) Nothing $ n + w)) [0..]

-- zipTree :: Tree a -> Tree b -> Tree (a,b)
-- zipTree (Node xs) (Node ys) = Node $ map (uncurry zipTree) $ zip xs ys
-- zipTree (Leaf a) (Leaf b) = Leaf (a,b)
-- zipTree _ _ = Node []

data MapR a b = MapR
  { hmapR :: M.HashMap b a
  , next :: a
  } deriving Show

-- data St = St
--   { blocks :: [Block]
--   , nextLabel :: Label
--   , uvars :: Set User
--   , fvars :: Array Integer (Maybe CExp)
--   , bvars :: Array Integer (Maybe Label)
--   } deriving Show

-- data Terminator
--   = Jump Label
--   | Switch AExp [Label] Label
--   | Return AExp
--   deriving Show

-- instance PP Terminator where
--   pp = \case
--     Jump a -> text "jump" <+> pp a
--     Switch a bs c -> ppSwitch a bs c
--     Return a -> text "return" <+> pp a

-- data Block = Block
--   { label :: Label
--   , phis :: [(Bound, [(AExp, Label)])]
--   , insns :: [(Free, (Op, [AExp]))]
--   , term :: Terminator
--   } deriving Show

-- type N a = State St a
  
-- type M a = State (MapR Integer CExp) a

runCExpMap :: Exp -> (AExp, MapR Integer CExp)
runCExpMap = flip runState (MapR M.empty 0) . toAExp

compile :: Exp -> IO ()
compile x = do
  -- print $ pp x -- this gets big very quickly due to redundancy
  let (a,b) = runCExpMap x
  let n = next b
  let bs = map swap $ M.toList $ hmapR b
  -- print $ pp a
  -- print a
  viz n bs
-- {-
--   let (us, bs) = runBlocks (maxBV x) a n bs
--   -- print $ pp bs
--   let blcks = map llvmBlock bs
--   -- print blcks
--   llvmAsm [(llvmTypeof $ fst a, "foo", us, blcks)]
-- -}

-- llvmAsm xs = do
--   eab <- withContext $ \cxt ->
--     runExceptT $ withModuleFromAST cxt (llvmModule xs) moduleLLVMAssembly
--   either error (\s -> writeFile llvmFilename s) eab
  
-- sortByCompare f = sortBy $ \a b -> compare (f a) (f b)

-- runBlocks :: Integer -> AExp -> Integer -> [(Integer, CExp)] -> ([User], [Block])
-- runBlocks nbv x n ys = (sort $ S.toList $ uvars st, sortByCompare label $ blocks st)
--   where
--     st = execState (compute x >>= pushTerm . Return) St
--       { blocks = [Block 0 [] [] $ unused "runBlocks"]
--       , nextLabel = 1
--       , uvars = S.empty
--       , fvars = array (0, pred n) $ map (\(a,b) -> (a, Just b)) ys
--       , bvars = array (0, pred nbv) $ zip [0 .. pred nbv] $ repeat Nothing
--       }

depsAExp = \case
  VAExp (FVar a) -> Just $ fid a
  _ -> Nothing

depsCExp :: CExp -> [Integer]
depsCExp x = catMaybes $ map depsAExp $ case unCExp x of
  AExp a -> [a]
  App _ bs -> bs
  Switch b cs d -> b : d : cs
  While _ a bs _ -> a : concatMap (\(_,(p,q)) -> [p,q]) bs

mkCExpNode :: (Integer, CExp) -> ((Integer, String), [(Integer, Integer, ())])
mkCExpNode (x,y) = ((x, "F" ++ show x ++ ": " ++ show (pp y)), [(x, a, ()) | a <- depsCExp y])

viz :: Integer -> [(Integer, CExp)] -> IO ()
viz n xs = do
  let (bs, css) = unzip $ [((n, "Start"), [(n, n - 1, ())])] ++ map mkCExpNode xs

  let params = nonClusteredParams{ fmtNode = singleton . toLabel . snd }
  let s = printDotGraph $ graphElemsToDot params bs $ concat css
  T.writeFile "t.dot" s
  _ <- system "dot -Gordering=out -Tsvg t.dot > t.dot.svg"
  return ()  

singleton a = [a]
