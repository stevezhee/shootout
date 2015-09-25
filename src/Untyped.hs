{-# OPTIONS -fno-warn-missing-signatures #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
{-# OPTIONS -fno-warn-type-defaults #-}
{-# OPTIONS -fno-warn-unused-imports #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- -XOverlappingInstances is deprecated: instead use per-instance pragmas OVERLAPPING/OVERLAPPABLE/OVERLAPS

module Untyped where

import           Data.Hashable
import           GHC.Generics (Generic)
import qualified Text.PrettyPrint as PP
import           Text.PrettyPrint hiding (int, empty)

-- import Debug.Trace
-- import           Control.Applicative hiding (empty)
-- import           Control.Exception
-- import           Control.Monad.State hiding (mapM, sequence)
-- import qualified Data.HashMap.Strict as M
-- import  Data.List
--   hiding (insert, lookup, elem, maximum, concatMap, mapAccumR, foldr, concat)
-- import           Data.Maybe
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
-- import Data.Foldable
-- import Data.Traversable
-- import Data.Bits
-- import Data.Graph hiding (Tree, Node)
-- import Data.GraphViz hiding (Int)
-- import System.Process
-- import qualified Data.Text.Lazy.IO as T

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
--   typeof x = case x of
--     Int t _ -> t
--     Rat t _ -> t
--     FVar a -> typeof a
--     BVar a -> typeof a
--     UVar a -> typeof a
--     Undef a -> a
    
-- llvmTypeof :: Typed a => a -> A.Type
-- llvmTypeof = llvmType . typeof

-- llvmType x = case x of
--   TSInt a -> tint a
--   TUInt a -> tint a
--   TDouble -> A.FloatingPointType 64 A.IEEE
--   TVector a b -> A.VectorType (fromInteger a) $ llvmType b
--   where
--     tint = A.IntegerType . fromIntegral
  

-- llvmOp :: Type -> Op -> ([Operand] -> A.Instruction)
-- llvmOp t x = case x of
--   Add -> binary (nowrap A.Add) (fast A.FAdd)
--   Sub -> binary (nowrap A.Sub) (fast A.FSub)
--   Mul -> binary (nowrap A.Mul) (fast A.FMul)
--   Or -> intop A.Or
--   And -> intop A.And
--   Xor -> intop A.Xor
--   Shl -> intop (wrap A.Shl)
--   Lshr -> intop (notExact A.LShr)
--   Ashr -> intop (notExact A.AShr)
--   Quot -> ubinary (exct A.UDiv) (exct A.SDiv) (fast A.FDiv)
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
-- llvmOperand x = case x of
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


-- instance PP a => PP [a] where pp = parens . hsep . map pp
  
-- instance (PP a, PP b) => PP (a,b) where pp (a,b) = parens (pp a <+> pp b)



  


-- isBinop = flip elem [Add, Mul, Sub, Quot, Rem, And, Or, Xor, Shl, Lshr, Ashr, Eq, Ne, Gt, Lt, Gte, Lte]


-- type NumBV = Integer

class Typed a where typeof :: a -> Type
class PP a where pp :: a -> Doc

data Type
  = TSInt Integer | TUInt Integer | TDouble | TAggregate | TVector Integer Type
  deriving (Show, Eq, Ord, Generic)
instance Hashable Type

data Const
  = Int Type Integer
  | Rat Type Rational
  | Undef Type
  deriving (Show, Eq, Generic, Ord)
instance Hashable Const
instance PP Const where
  pp x = case x of
    Int _ b -> pp b
    Rat _ b -> pp b
    Undef _ -> text "undef"

instance PP Integer where pp = integer
instance PP Rational where pp = rational
                          
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
  pp x = case x of
    UVar a -> pp a
    BVar a -> pp a
    FVar a -> pp a

data AExp
  = CAExp Const
  | VAExp Var
  deriving (Show, Eq, Generic, Ord)
instance Hashable AExp
instance PP AExp where
  pp x = case x of
    CAExp a -> pp a
    VAExp a -> pp a

data UOp
  = Add | Mul | Sub | Quot | Rem | And | Or | Xor | Shl | Lshr | Ashr | Eq | Ne | Gt | Lt | Gte | Lte
  | Abs | Signum | Sqrt | Exp | Log | Sin | Cos | Asin | Atan | Acos | Sinh | Cosh | Asinh | Atanh | Acosh
  | InsertElement | ExtractElement | ShuffleVector
  | ToFP
  deriving (Show, Eq, Ord, Generic)
instance Hashable UOp
instance PP UOp where
  pp x = text $ case x of
    Add -> "+"
    Mul -> "*"
    Sub -> "-"
    Quot -> "/"
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
    _ -> show x

data Op = Op{ otype :: Type, uop :: UOp } deriving (Show, Eq, Ord, Generic)
instance Hashable Op
instance Typed Op where typeof = otype
instance PP Op where pp = pp . uop

data Exp
  = EAExp AExp
  | EOp Op [Exp]
  | ESwitch Exp [Exp] Exp
  | EWhile Exp (Tree (Bound, (Exp, Exp))) Bound
  deriving (Show, Eq)

maximumBV :: (Foldable t, Functor t) => t Exp -> Integer
maximumBV = maximum . fmap maxBV

maxBV :: Exp -> Integer
maxBV x = case x of
  EAExp a -> case a of
    VAExp (BVar v) -> bid v
    _ -> 0
  EOp _ bs -> maximumBV bs
  ESwitch a bs c -> maximumBV (a : c : bs)

zipWithTree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c -- trees must have the same shape
zipWithTree f x y =
  case (x,y) of
   (Leaf a, Leaf b) -> Leaf $ f a b
   (Node bs, Node cs) -> Node $ map (uncurry $ zipWithTree f) $ zip bs cs
   _ -> unused "zipWithTree"
  
listToTree :: [Tree a] -> Tree [a]
listToTree xs@(b:_) = foldr (zipWithTree (:)) (fmap (\_ -> []) b) xs

pairTree :: Tree a -> Tree b -> Tree (a,b)
pairTree = zipWithTree (,)

switch :: Exp -> [Tree Exp] -> Tree Exp -> Tree Exp
switch x ys z = fmap (\(a:bs) -> ESwitch x bs a) $ listToTree (z : ys)

bvar :: Bound -> Exp
bvar = EAExp . VAExp . BVar

while :: Tree Exp -> (Tree Exp -> (Exp, Tree Exp)) -> Tree Exp
while x f = fmap (EWhile e $ pairTree b $ pairTree x x1) b
  where
    x0 :: Tree Exp = fmap bvar b
    b :: Tree Bound = undefined
    (e, x1) = f x0
    n = maximum [maximumBV x, maxBV e, maximumBV x1]

-- while :: Tree Exp -> (Tree Exp -> (Exp, Tree Exp)) -> Tree Exp
-- while xs f = fmap (\(v, _) -> EPhi v w) t
--   -- ^ BAL: identify unused loop variables (remove(warning?) or error)
--   where
--     w = EWhile (n + m) e t
--     t = zipTree vs $ fmap (\(a,b) -> [a,b]) $ zipTree xs ys
--     m = genericLength $ toList xs
--     (e, ys) = f $ fmap (EAExp . BVar) vs
--     vs = bound n xs
--     n = maximumBV (e : toList xs ++ toList ys)
    
-- switch :: Exp -> [Tree Exp] -> Tree Exp -> Tree Exp
-- switch x ys z = fmap (flip EPhi $ ESwitch (n + m) vs x ys z) vs
--   where
--     m = genericLength $ toList z
--     vs = bound n z
--     n = maximumBV (x : toList z ++ concatMap toList ys)

  -- | ESwitch NumBV (Tree Bound) Exp [Tree Exp] (Tree Exp)
  -- | EWhile NumBV Exp (Tree (Phi Exp))
  -- | EPhi Bound Exp

-- tbool = TUInt 1

-- instance Typed Exp where
--   typeof x = case x of
--     EAExp a -> typeof a
--     EOp _ a bs -> typeofOp a bs
--     ESwitch{} -> TAggregate
--     EWhile{} -> TAggregate
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
-- compute x = case x of
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
-- computeStmt x = case x of
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
-- fromCExp x = case x of
--   CAExp a -> EAExp a
--   COp a bs -> EOp 0 a $ map EAExp bs
--   CSwitch vs a bss cs ->
--     ESwitch 0 (listToTree vs) (EAExp a) (map (fmap EAExp . listToTree) bss) (fmap EAExp $ listToTree cs)
--   CWhile a bs ->
--     EWhile 0 (EAExp a) (flip fmap (listToTree bs) $ \(v, [p, q]) -> (v, [EAExp p, EAExp q]))
--   CPhi a b -> EPhi a $ EAExp b

-- listToTree :: [a] -> Tree a
-- listToTree = Node . map Leaf

-- instance PP CExp where pp = pp . fromCExp
  
-- aexp :: Exp -> M AExp
-- aexp x = cexp x >>= toAExp

-- -- evalOpRat :: Op -> [Rational] -> Rational
-- -- evalOpRat x ys = case x of
-- --   Add -> a + b
-- --   Sub -> a - b
-- --   Mul -> a * b
-- --   Quot -> a / b
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
-- --   Quot -> a `div` b
-- --   _ -> error $ "evalOpInt:" ++ show (x,ys)
-- --   where
-- --     [a,b] = ys

-- optimize = True
-- -- optimize = False

-- eqConst x y = case x of
--   Rat _ a -> a == toRational y
--   Int _ a -> a == y
--   _ -> False

-- isConst x = case x of
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
--     Quot -> iToI quot
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
--     Quot -> rToR (/)
--     Rem -> unused "Rem" -- BAL:?
--     Eq -> toB (==)
--     Ne -> toB (/=)
--     Gt -> toB (>)
--     Lt -> toB (<)
--     Gte -> toB (>=)
--     Lte -> toB (<=)
--     where
--       rToR f x = Rat t . f x

-- toAExp :: CExp -> M AExp
-- toAExp x = case x of
--   CAExp a -> return a
--   COp a [b, c] | isConst b && isConst c -> return $ constFold a [b, c]
--   COp Add [b, c] | b `eqConst` 0 -> return c
--   COp Add [b, c] | c `eqConst` 0 -> return b
--   COp Sub [b, c] | b == c -> return zero
--   COp Sub [b, c] | c `eqConst` 0 -> return b
--   COp Mul [b, c] | b `eqConst` 0 || c `eqConst` 0 -> return zero
--   COp Mul [b, c] | b `eqConst` 1 -> return c
--   COp Mul [b, c] | c `eqConst` 1 -> return b
--   COp Quot [b, c] | c `eqConst` 0 -> error "divide by zero"
--   COp Quot [b, c] | c `eqConst` 1 -> return b
--   COp Quot [b, c] | b == c -> return one
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

-- canonCExp x = case x of
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

-- swap (x,y) = (y,x)
-- pair x y = (x,y)

-- instance (Hashable b, Eq b, Num a, PP a, PP b, Ord b, Ord a) => PP (MapR a b) where
--   pp = vcat . map pp . sort . map swap . M.toList . hmapR

-- lookupR :: (Hashable b, Eq b) => b -> MapR a b -> Maybe a
-- lookupR b = M.lookup b . hmapR

-- insertR :: (Hashable b, Eq b, Enum a) => b -> MapR a b -> (a, MapR a b)
-- insertR b tbl = case lookupR b tbl of
--   Just a -> (a, tbl)
--   Nothing -> (a, tbl{ next = succ a, hmapR = M.insert b a $ hmapR tbl })
--     where a = next tbl
    
-- cexp :: Exp -> M CExp
-- cexp x = case x of
--   EAExp a -> return $ CAExp a
--   EOp _ b cs -> COp b <$> mapM aexp cs
--   ESwitch _ vs b cs d -> do
--     cs' <- mapM (mapM aexp . toList) cs
--     d' <- mapM aexp (toList d)
--     case reverse $ dropWhile ((==) d') $ reverse cs' of
--       [] -> return $ CSwitch (toList vs) (Undef $ TUInt 1) [] d' -- BAL: haven't tested this yet
--       cs'' -> aexp b >>= \b' -> return $ CSwitch (toList vs) b' cs'' d'
    
--   EWhile _ a bs -> CWhile <$> aexp a <*> mapM f (toList bs)
--     where f (v, ps) = pair v <$> mapM aexp ps
--   EPhi a b -> CPhi a <$> aexp b

-- instance PP Block where
--   pp (Block a b c d) = vcat [pp a, nest 2 $ vcat $ map pp b ++ map pp c ++ [pp d]]

-- instance PP [Block] where pp = vcat . map pp

-- ppSwitch a bs c = hsep $ text "switch" : pp a : map pp bs ++ [pp c]

-- ppParens x = case x of
--   EAExp{} -> pp x
--   _ -> parens $ pp x

-- instance PP Exp where
--   pp x = case x of
--     ESwitch _ _ a bs c -> ppSwitch a bs c
--     EOp _ a bs
--       | a == ExtractElement -> pp b0 <> brackets (pp b1)
--       | a == InsertElement -> pp b0 <> brackets (pp b2) <+> text "<-" <+> pp b1
--       | isBinop a -> ppParens b0 <+> pp a <+> ppParens b1
--       | otherwise -> pp a <+> hsep (map ppParens bs)
--       where
--         b0:_ = bs
--         _:b1:_ = bs
--         _:_:b2:_ = bs
--     EAExp a -> pp a
--     EWhile _ a bs -> vcat [text "while", nest 2 $ vcat [ppParens a, pp bs]]
--     EPhi a b -> hsep [text "phi", pp a, ppParens b]

-- instance (Foldable t, PP a) => PP (t a) where pp = pp . toList

    
-- instance (PP a, PP b, PP c, PP d) => PP (a,b,c,d) where
--   pp (a,b,c,d) = parens (vcat [pp a, pp b, pp c, pp d])

-- maximumBV :: [Exp] -> Integer
-- maximumBV = maximum . map maxBV

-- maxBV :: Exp -> Integer
-- maxBV x = case x of
--   ESwitch i _ _ _ _ -> i
--   EOp i _ _ -> i
--   EAExp _ -> 0
--   EWhile i _ _ -> i
--   EPhi _ b -> maxBV b

-- binop :: Op -> Exp -> Exp -> Exp
-- binop o x y = EOp (maximumBV [x,y]) o [x,y]

-- ternop :: Op -> Exp -> Exp -> Exp -> Exp
-- ternop o x y z = EOp (maximumBV [x,y,z]) o [x,y,z]

-- unop :: Op -> Exp -> Exp
-- unop o x = EOp (maxBV x) o [x]

unused = error . (++) "unused:"

-- var t = EAExp . UVar . User t
    
-- bound :: Typed a => Integer -> Tree a -> Tree Bound
-- bound n = snd . mapAccumR (\(w:ws) x -> (ws, Bound (typeof x) Nothing $ n + w)) [0..]

-- zipTree :: Tree a -> Tree b -> Tree (a,b)
-- zipTree (Node xs) (Node ys) = Node $ map (uncurry zipTree) $ zip xs ys
-- zipTree (Leaf a) (Leaf b) = Leaf (a,b)
-- zipTree _ _ = Node []

-- data MapR a b = MapR
--   { hmapR :: M.HashMap b a
--   , next :: a
--   } deriving Show

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
--   pp x = case x of
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

-- runCExpMap :: Exp -> (AExp, MapR Integer CExp)
-- runCExpMap = flip runState (MapR M.empty 0) . aexp

-- compile :: Exp -> IO ()
-- compile x = do
--   -- print $ pp x -- this gets big very quickly due to redundancy
--   let (a,b) = runCExpMap x
--   let n = next b
--   let bs = map swap $ M.toList $ hmapR b
--   -- print $ pp a
--   -- print a
--   viz n bs
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

-- depsAExp x = case x of
--   FVar a -> Just $ fid a
--   _ -> Nothing

-- depsCExp x = catMaybes $ map depsAExp $ case x of
--   CAExp a -> [a]
--   COp _ bs -> bs
--   CSwitch _ b css ds -> b : ds ++ concat css
--   CWhile a bs -> a : concat (map snd bs)
--   CPhi _ b -> [b]

-- mkCExpNode :: (Integer, CExp) -> ((Integer, String), [(Integer, Integer, ())])
-- mkCExpNode (x,y) = ((x, "F" ++ show x ++ ": " ++ show (pp y)), [(x, a, ()) | a <- depsCExp y])

-- viz :: Integer -> [(Integer, CExp)] -> IO ()
-- viz n xs = do
--   let (bs, css) = unzip $ [((n, "Start"), [(n, n - 1, ())])] ++ map mkCExpNode xs

--   let params = nonClusteredParams{ fmtNode = singleton . toLabel . snd }
--   let s = printDotGraph $ graphElemsToDot params bs $ concat css
--   T.writeFile "t.dot" s
--   _ <- system "dot -Gordering=out -Tsvg t.dot > t.dot.svg"
--   return ()  

-- singleton a = [a]
