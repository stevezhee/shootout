{-# OPTIONS -fno-warn-missing-signatures #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
{-# OPTIONS -fno-warn-type-defaults #-}
{-# OPTIONS -fno-warn-unused-imports #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Untyped where

import           Control.Applicative hiding (empty)
import           Control.Exception
import           Control.Monad.State hiding (mapM, sequence)
import qualified Data.HashMap.Strict as M
import           Data.Hashable
import           Data.List hiding (insert, lookup, elem, maximum, concatMap, mapAccumR, foldr, concat)
import           Data.Maybe
import           GHC.Generics (Generic)
import           Prelude hiding (lookup, elem, maximum, concatMap, mapM, sequence, foldr, concat)
import qualified Text.PrettyPrint as PP
import           Text.PrettyPrint hiding (int, empty)
import Data.Array
import LLVM.General.Module
import LLVM.General.Context
import Control.Monad.Trans.Except
import LLVM.General.AST hiding
  (Type(..), Terminator(..), Operator(..), Constant(..), Instruction(..))
import qualified LLVM.General.AST as A
import LLVM.General.AST.Global
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.CallingConvention as CC
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.Float as F
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable
import Data.Traversable

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

llvmFunction (t, n, us, bs) = GlobalDefinition functionDefaults
  { returnType = t
  , name = Name n
  , parameters = ([Parameter (llvmTypeof u) (llvmName u) [] | u <- us], False)
  , basicBlocks = bs
  }

llvmFilename = "t.ll"
llvmModule xs = A.defaultModule
  { moduleName = llvmFilename
  , moduleDefinitions = map llvmFunction xs
  }

ppName :: PP a => a -> Name
ppName = Name . show . pp

class LLVMName a where llvmName :: a -> Name
instance LLVMName Label where  llvmName (Label a) = UnName $ fromIntegral a
instance LLVMName Free where llvmName = ppName
instance LLVMName Bound where llvmName = ppName
instance LLVMName User where llvmName = ppName
instance (LLVMName a, LLVMName b) => LLVMName (Either a b) where
  llvmName = either llvmName llvmName

mapPair f g (x,y) = (f x, g y)

llvmPhi :: (Either Free Bound, [(AExp, Label)]) -> Named A.Instruction
llvmPhi (x, ys) =
  llvmName x := A.Phi (llvmTypeof x) (map (mapPair llvmOperand llvmName) ys) []
  
llvmInsn (x, (y, zs)) = llvmName x := (llvmOp (typeof x) y) (map llvmOperand zs)

class Typed a where typeof :: a -> Type

instance (Typed a, Typed b) => Typed (Either a b) where typeof = either typeof typeof
instance Typed Bound where typeof (Bound a _) = a
instance Typed User where typeof (User a _) = a
instance Typed Free where typeof (Free a _) = a
instance Typed AExp where
  typeof x = case x of
    Int t _ -> t
    Rat t _ -> t
    FVar a -> typeof a
    BVar a -> typeof a
    UVar a -> typeof a
    
llvmTypeof :: Typed a => a -> A.Type
llvmTypeof = llvmType . typeof

llvmType x = case x of
  TSInt a -> tint a
  TUInt a -> tint a
  TDouble -> A.FloatingPointType 64 A.IEEE
  where
    tint = A.IntegerType . fromIntegral
  
data Type = TSInt Integer | TUInt Integer | TDouble | TAggregate deriving (Show, Eq, Ord, Generic)
instance Hashable Type

llvmOp :: Type -> Op -> ([Operand] -> A.Instruction)
llvmOp t x = case x of
  Add -> binary (nowrap A.Add) (fast A.FAdd)
  Sub -> binary (nowrap A.Sub) (fast A.FSub)
  Mul -> binary (nowrap A.Mul) (fast A.FMul)
  Quot -> ubinary (exct A.UDiv) (exct A.SDiv) (fast A.FDiv)
  Rem -> ubinary A.URem A.SRem (fast A.FRem)
  Eq -> cmp IP.EQ FP.OEQ
  Ne -> cmp IP.NE FP.ONE
  Gt -> ucmp IP.UGT IP.SGT FP.OGT
  Lt -> ucmp IP.ULT IP.SLT FP.OLT
  Gte -> ucmp IP.UGE IP.SGE FP.OGE
  Lte -> ucmp IP.ULE IP.SLE FP.OLE
  Sqrt -> uunary (unused "Sqrt:unsigned") (unused "Sqrt:signed") (call1 "llvm.sqrt")
  _ -> error $ "llvmOp:" ++ show x
  where
    nowrap f = f True True
    fast f = f UnsafeAlgebra
    exct f = f False
    binary f g = ubinary f f g
    uunary f g h = \[a] -> case t of
      TUInt{} -> f a []
      TSInt{} -> g a []
      TDouble -> h a []

    ubinary f g h = \[a,b] -> case t of
      TUInt{} -> f a b []
      TSInt{} -> g a b []
      TDouble -> h a b []
    cmp f g = ucmp f f g
    ucmp f g h = ubinary (A.ICmp f) (A.ICmp g) (A.FCmp h)
    call n bs = A.Call False CC.C [] (Right $ LocalReference (llvmType t) $ Name n) (map (flip pair []) bs) []
    call1 n b = call n [b]

llvmOperand :: AExp -> Operand
llvmOperand x = case x of
  Int t a -> ConstantOperand $ case t of
    TUInt b -> C.Int (fromIntegral b) a
    TSInt b -> C.Int (fromIntegral b) a
    TDouble -> C.Float $ F.Double $ fromIntegral a
    _ -> unused $ "llvmOperand:Int:" ++ show t
  Rat t a -> ConstantOperand $ C.Float $ case t of
    TDouble -> F.Double $ fromRational a
    _ -> unused $ "llvmOperand:Rat:" ++ show t
  FVar a -> ref a
  BVar a -> ref a
  UVar a -> ref a
  where
    ref a = LocalReference (llvmTypeof x) (llvmName a)

llvmTerminator x = Do $ case x of
  Jump a -> A.Br (llvmName a) []
  Switch a bs c ->
    A.Switch (llvmOperand a) (llvmName c) [ (f n, llvmName l)
                                           | (n,l) <- zip [0 ..] bs] []
    where
      f n = case typeof a of
        TSInt a -> C.Int (fromIntegral a) n
        TUInt a -> C.Int (fromIntegral a) n
        TDouble -> C.Float $ F.Double $ fromIntegral n
  Return a -> A.Ret (Just $ llvmOperand a) []

llvmBlock :: Block -> BasicBlock
llvmBlock (Block a bs cs d) =
  BasicBlock (llvmName a) (map llvmPhi bs ++ map llvmInsn cs) (llvmTerminator d)

class PP a where pp :: a -> Doc

instance PP a => PP [a] where pp = parens . hsep . map pp
  
instance (PP a, PP b) => PP (a,b) where pp (a,b) = parens (pp a <+> pp b)

data Free = Free{ ftype :: Type, fid :: Integer } deriving (Show, Eq, Ord, Generic)

instance Hashable Free
instance PP Free where pp (Free _ a) = text "F" <> integer a

data User = User Type Integer deriving (Show, Eq, Ord, Generic)
instance Hashable User
instance PP User where pp (User _ a) = text "U" <> integer a

data Bound = Bound Type Integer deriving (Show, Eq, Ord, Generic)
instance Hashable Bound
instance PP Bound where pp (Bound _ a) = text "B" <> integer a
  
newtype Label = Label Integer deriving (Show, Eq, Num, Ord, Generic, Enum)
instance Hashable Label
instance PP Label where pp (Label a) = text "L" <> integer a

data Op
  = Add | Sub | Mul | Quot | Rem
  | Eq | Ne
  | Gt | Lt | Gte | Lte
  | Abs | Signum
  | Sqrt | Exp | Log | Sin | Cos | Asin | Atan | Acos | Sinh | Cosh | Asinh | Atanh
  | Acosh
  deriving (Show, Eq, Ord, Generic, Enum)
instance Hashable Op
instance PP Op where pp = text . show

instance PP Integer where pp = integer
instance PP Rational where pp = rational

type NumBV = Integer

data AExp
  = Int Type Integer
  | Rat Type Rational
  | FVar Free
  | BVar Bound
  | UVar User
  deriving (Show, Eq, Generic, Ord)
instance Hashable AExp

typeofOp x y
  | x `elem` [Eq, Ne, Gt, Lt, Gte, Lte] = TUInt 1
  | otherwise = typeof y
  
instance Typed Exp where
  typeof x = case x of
    EAExp a -> typeof a
    EOp _ a bs -> typeofOp a $ head bs
    ESwitch{} -> TAggregate
    EWhile{} -> TAggregate
    EPhi a _ -> typeof a

data Exp
  = EAExp AExp
  | EOp NumBV Op [Exp]
  | ESwitch NumBV (Tree Bound) Exp [Tree Exp] (Tree Exp)
  | EWhile NumBV Exp (Tree (Phi Exp))
  | EPhi Bound Exp
  deriving (Show, Eq)

instance Typed CExp where typeof = typeof . fromCExp

type Phi a = (Bound, [a])

data CExp
  = CAExp AExp
  | COp Op [AExp]
  | CSwitch [Bound] AExp [[AExp]] [AExp]
  | CWhile AExp [Phi AExp]
  | CPhi Bound AExp
  deriving (Show, Eq, Generic, Ord)
instance Hashable CExp

{-
-- v = op es
<prev computing>
v = op es
<next computing> with continuation whatever got passed in


push (v = op es) onto the stack

-}

{-
-- v = switch a bs b
-- if 'a' is a constant then reduce to the appropriate b
<prev computing>
<compute a> with continuation (switch a (map lbl bs) (lbl b))

lbl 0:
<compute b0> with continuation (goto lbl end:)

...

lbl end:
v = phi $ zip (b:bs) (lbl b : map lbl bs)
<next computing> with continuation whatever got passed in

-}

lookupFVar :: Free -> N CExp
lookupFVar x = flip (!) (fid x) <$> gets fvars

pushInsn :: (Free, (Op, [AExp])) -> N ()
pushInsn x =
  modify $ \st -> st{ blocks = let b:bs = blocks st in b{ insns = x : insns b } : bs }

pushTerm :: Terminator -> N ()
pushTerm x = modify $ \st ->
  st{ blocks = let b:bs = blocks st in b{ term = x, insns = reverse $ insns b } : bs }

pushLabel :: Label -> [(Either Free Bound, [(AExp, Label)])] -> N ()
pushLabel lbl ps = modify $ \st -> st{ blocks = Block lbl ps [] (unused "pushLabel") : blocks st }

freshLabel :: N Label
freshLabel = do
  lbl <- gets nextLabel
  modify $ \st -> st{ nextLabel = succ lbl }
  return lbl

currentLabel :: N Label
currentLabel = label . head <$> gets blocks

groupByFst :: (Eq a, Ord a) => [(a,b)] -> [(a, [b])]
groupByFst = map (\bs -> (fst $ head bs, map snd bs)) . groupBy (\a b -> fst a == fst b) . sortBy (\a b -> compare (fst a) (fst b))

compute :: AExp -> N AExp
compute x = case x of
  FVar n -> do
    let ok vx = do
          modify $ \st -> st{ fvars = fvars st // [(fid n, CAExp vx)] }
          return vx
    y <- lookupFVar n
    case y of
      CAExp a -> return a
      COp a bs -> do
        vbs <- mapM compute bs
        pushInsn (n, (a,vbs))
        ok x
      CSwitch vs a bss cs -> do
        va <- compute a
        lbl : lbls <- mapM (const freshLabel) (cs : bss)
        let ps = zip (cs : bss) (lbl : lbls)
        end <- freshLabel
        pushTerm $ Switch va lbls lbl
        vpss <- flip mapM ps $ \(bs,l) -> do
          pushLabel l []
          vbs <- mapM compute bs -- BAL: need a 'withMap' function
          l' <- currentLabel
          pushTerm $ Jump end
          return $ zip (map Right vs) $ zip vbs $ repeat l'
        pushLabel end $ groupByFst $ concat vpss
        ok $ Int (TSInt 42) 42 -- the value here doesn't matter, just prevents recomputation
      CWhile a bs -> do
        vbs0 <- mapM compute $ map (head . snd) bs
        pre <- currentLabel
        [begin, body, end] <- sequence $ replicate 3 freshLabel
        pushTerm $ Jump begin
        
        pushLabel body []
        vbs1 <- mapM compute $ map (last . snd) bs
        from <- currentLabel
        pushTerm $ Jump begin
        
        pushLabel begin [ (Right r, [(p, pre), (q, from)])
                        | (r, p, q) <- zip3 (map fst bs) vbs0 vbs1
                        ]
        va <- compute a
        pushTerm $ Switch va [end] body

        pushLabel end []
        ok $ Int (TSInt 42) 42 -- the value here doesn't matter, just prevents recomputation
      CPhi a b -> do
        _ <- compute b
        ok $ BVar a
      
  UVar a -> do
    modify $ \st -> st{ uvars = S.insert a $ uvars st }
    return x
  _ -> return x
  
{-
-- v = while a bs c
-- 'a' and 'c' must both depend on bs o.w. error
-- 'a' must not be constant

<prev computing>
goto begin:

begin:
phi bs
<compute a> with continuation (switch a [end:, body:])

body:
<compute each bs> continue from one to the next with the last one having continuation (goto begin:)

end:
<compute c>
<next computing where subst c for v> with continuation whatever got passed in

-}

fromCExp :: CExp -> Exp
fromCExp x = case x of
  CAExp a -> EAExp a
  COp a bs -> EOp 0 a $ map EAExp bs
  CSwitch vs a bss cs ->
    ESwitch 0 (listToTree vs) (EAExp a) (map (fmap EAExp . listToTree) bss) (fmap EAExp $ listToTree cs)
  CWhile a bs ->
    EWhile 0 (EAExp a) (flip fmap (listToTree bs) $ \(v, [p, q]) -> (v, [EAExp p, EAExp q]))
  CPhi a b -> EPhi a $ EAExp b

listToTree :: [a] -> Tree a
listToTree = Node . map Leaf

instance PP CExp where pp = pp . fromCExp
  
aexp :: Exp -> M AExp
aexp x = cexp x >>= toAExp

isConst :: AExp -> Bool
isConst x = case x of
  Int{} -> True
  Rat{} -> True
  _ -> False

isRat :: AExp -> Bool
isRat x = case x of
  Rat{} -> True
  _ -> False

evalOpRat :: Op -> [Rational] -> Rational
evalOpRat x ys = case x of
  Add -> a + b
  Sub -> a - b
  Mul -> a * b
  Quot -> a / b
  Sqrt -> toRational (sqrt (fromRational a) :: Double)
  _ -> error $ "evalOpRat:" ++ show (x,ys)
  where
    a = head ys
    b = head $ tail ys

evalOpInt :: Op -> [Integer] -> Integer
evalOpInt x ys = case x of
  Add -> a + b
  Sub -> a - b
  Mul -> a * b
  Quot -> a `div` b
  _ -> error $ "evalOpInt:" ++ show (x,ys)
  where
    [a,b] = ys

optimize = True
-- optimize = False

toAExp :: CExp -> M AExp
toAExp x = case x of
  CAExp a -> return a
  -- COp a bs | optimize -> case (a,bs) of
  --   _ | all isConst bs -> return $ case any isRat bs of
  --                          True -> Rat $ evalOpRat a $ map toRational bs
  --                          False -> Int $ evalOpInt a $ map toInteger bs
  --   (Mul, [Rat 1, p]) -> return p
  --   (Mul, [Int 1, p]) -> return p
  --   (Mul, [p, Rat 1]) -> return p
  --   (Mul, [p, Int 1]) -> return p
  --   (Mul, [Rat 0, _]) -> return $ Rat 0
  --   (Mul, [Int 0, _]) -> return $ Int 0
  --   (Mul, [_, Rat 0]) -> return $ Rat 0
  --   (Mul, [_, Int 0]) -> return $ Int 0
  --   (Add, [Rat 0, p]) -> return p
  --   (Add, [Int 0, p]) -> return p
  --   (Add, [p, Rat 0]) -> return p
  --   (Add, [p, Int 0]) -> return p
  --   (Sub, [p, Rat 0]) -> return p
  --   (Sub, [p, Int 0]) -> return p
    -- _ -> ok
  _ -> ok
  where
    ok = do
      tbl <- get
      let (a, tbl') = insertR x tbl
      modify $ \_ -> tbl'
      return $ FVar $ Free (typeof x) a

swap (x,y) = (y,x)
pair x y = (x,y)

instance (Hashable b, Eq b, Num a, PP a, PP b, Ord b, Ord a) => PP (MapR a b) where
  pp = vcat . map pp . sort . map swap . M.toList . hmapR

lookupR :: (Hashable b, Eq b) => b -> MapR a b -> Maybe a
lookupR b = M.lookup b . hmapR

insertR :: (Hashable b, Eq b, Enum a) => b -> MapR a b -> (a, MapR a b)
insertR b tbl = case lookupR b tbl of
  Just a -> (a, tbl)
  Nothing -> (a, tbl{ next = succ a, hmapR = M.insert b a $ hmapR tbl })
    where a = next tbl
    
cexp :: Exp -> M CExp
cexp x = case x of
  EAExp a -> return $ CAExp a
  EOp _ b cs -> COp b <$> mapM aexp cs
  ESwitch _ vs b cs d -> CSwitch (toList vs) <$> aexp b <*> mapM (mapM aexp . toList) cs <*> mapM aexp (toList d)
  EWhile _ a bs -> CWhile <$> aexp a <*> mapM f (toList bs)
    where f (v, ps) = pair v <$> mapM aexp ps
  EPhi a b -> CPhi a <$> aexp b

instance PP Block where
  pp (Block a b c d) = vcat [pp a, nest 2 $ vcat $ map pp b ++ map pp c ++ [pp d]]

instance PP [Block] where pp = vcat . map pp
  
ppSwitch a bs c = parens $ hsep $ text "switch" : pp a : map pp bs ++ [pp c]
  
instance PP Exp where
  pp x = case x of
    ESwitch _ _ a bs c -> ppSwitch a bs c
    EOp _ a bs -> parens (pp a <+> hsep (map pp bs))
    EAExp a -> pp a
    EWhile _ a bs -> parens $ vcat [text "while", nest 2 $ vcat [pp a, pp bs]]
    EPhi a b -> parens (pp a <+> pp b)

instance (Foldable t, PP a) => PP (t a) where pp = pp . toList

instance PP AExp where
  pp x = case x of
    FVar a -> pp a
    BVar a -> pp a
    UVar a -> pp a
    Int _ a -> pp a
    Rat _ a -> pp a
    
instance (PP a, PP b, PP c, PP d) => PP (a,b,c,d) where
  pp (a,b,c,d) = parens (vcat [pp a, pp b, pp c, pp d])

maximumBV :: [Exp] -> Integer
maximumBV = maximum . map maxBV

maxBV :: Exp -> Integer
maxBV x = case x of
  ESwitch i _ _ _ _ -> i
  EOp i _ _ -> i
  EAExp _ -> 0
  EWhile i _ _ -> i
  EPhi _ b -> maxBV b

binop :: Op -> Exp -> Exp -> Exp
binop o x y = EOp (maximumBV [x,y]) o [x,y]

unop :: Op -> Exp -> Exp
unop o x = EOp (maxBV x) o [x]

unused = error . (++) "unused:"

var t = EAExp . UVar . User t

switch :: Exp -> [Tree Exp] -> Tree Exp -> Tree Exp
switch x ys z = fmap (flip EPhi $ ESwitch (n + m) vs x ys z) vs
  where
    m = genericLength $ toList z
    vs = bound n z
    n = maximumBV (x : toList z ++ concatMap toList ys)
    
while :: Tree Exp -> (Tree Exp -> (Exp, Tree Exp)) -> Tree Exp
while xs f = fmap (\(v, _) -> EPhi v $ EWhile (n + m) e t) t
  -- ^ BAL: identify unused loop variables (remove(warning?) or error)
  where
    t = zipTree vs $ fmap (\(a,b) -> [a,b]) $ zipTree xs ys
    m = genericLength $ toList xs
    (e, ys) = f $ fmap (EAExp . BVar) vs
    vs = bound n xs
    n = maximumBV (e : toList xs ++ toList ys)

bound :: Typed a => Integer -> Tree a -> Tree Bound
bound n = snd . mapAccumR (\(w:ws) x -> (ws, Bound (typeof x) $ n + w)) [0..]

zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree (Node xs) (Node ys) = Node $ map (uncurry zipTree) $ zip xs ys
zipTree (Leaf a) (Leaf b) = Leaf (a,b)
zipTree _ _ = Node []

instance (PP a, PP b) => PP (Either a b) where
  pp x = case x of
    Left a -> pp a
    Right b -> pp b

data MapR a b = MapR
  { hmapR :: M.HashMap b a
  , next :: a
  } deriving Show

data St = St
  { blocks :: [Block]
  , nextLabel :: Label
  , uvars :: Set User
  , fvars :: Array Integer CExp
  } deriving Show

data Terminator
  = Jump Label
  | Switch AExp [Label] Label
  | Return AExp
  deriving Show

instance PP Terminator where
  pp x = case x of
    Jump a -> text "jump" <+> pp a
    Switch a bs c -> ppSwitch a bs c
    Return a -> text "return" <+> pp a

data Block = Block
  { label :: Label
  , phis :: [(Either Free Bound, [(AExp, Label)])]
  , insns :: [(Free, (Op, [AExp]))]
  , term :: Terminator
  } deriving Show

type N a = State St a
  
type M a = State (MapR Integer CExp) a

runCExpMap = flip runState (MapR M.empty 0) . aexp

compile :: Exp -> IO ()
compile x = do
  let a = runCExpMap x
  print $ pp a
  let (us, bs) = runBlocks a
  print $ pp bs
  llvmAsm [(llvmTypeof $ fst a, "foo", us, map llvmBlock bs)]

llvmAsm xs = do
  eab <- withContext $ \cxt ->
    runExceptT $ withModuleFromAST cxt (llvmModule xs) moduleLLVMAssembly
  either error (\s -> putStrLn s >> writeFile llvmFilename s) eab
  
sortByCompare f = sortBy $ \a b -> compare (f a) (f b)

runBlocks :: (AExp, MapR Integer CExp) -> ([User], [Block])
runBlocks (x, y) = (sort $ S.toList $ uvars st, sortByCompare label $ blocks st)
  where
    st = execState (compute x >>= pushTerm . Return) $
           St [Block 0 [] [] $ unused "runBlocks"] 1 S.empty $
           array (0, pred $ next y) $ map swap $ M.toList $ hmapR y
