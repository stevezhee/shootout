{-# OPTIONS -fno-warn-missing-signatures #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
{-# OPTIONS -fno-warn-type-defaults #-}
{-# OPTIONS -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}

module NBody
  ( -- main, t, debug_print
  (>.), (<.), (>=.), (<=.), (==.), (/=.), (+=)
  ) where

-- import           Data.IntMap hiding (lookup, map, adjust)
-- import Language.C
-- import Language.C.Syntax

import           Control.Applicative hiding (Const, empty)
import           Control.Exception
import           Control.Monad.State
import           Data.Generics hiding (Generic, empty, cast)
import           Data.Hashable
import           Data.List
import           Data.Maybe
import           Data.Word
import           GHC.Generics (Generic)
import           Language.C.DSL hiding ((#), (.=), for, while, var, int)
import           Text.PrettyPrint hiding (int)
import qualified Data.HashMap.Strict as M
import qualified Language.C.DSL as C

undef = error . (++) "undefined:"
{-

data Constant
  = CInt Integer
  | CRat Rational
  deriving (Show, Typeable, Data)
             
newtype Address = Address String deriving (Show, Eq, Generic, Typeable, Data)
instance Hashable Address

newtype Register = Register Integer deriving Show
newtype Label = Label Integer deriving (Show, Eq, Generic, Ord)


data Exp
  = EConst Constant
  | EApply Address [Exp]
  | EAddr Address
  deriving (Show, Typeable, Data)

ppBlock (lbl, (instrs, ctrl)) = vcat $ map text $ [show lbl] ++ map show instrs ++ [show ctrl]

-- toBlockMap :: Program -> M.HashMap Label ([Instr], Control)
toBlockMap prog = mapM_ (print . ppBlock) $ sortByFst $ M.toList $ blockMap $ execState (mapM_ stmt prog >> lastBlock) $ St (Register 0) (Label 1) M.empty (Label 0) [] M.empty


    

type Program = [Stmt]

t = toBlockMap
  [ Store n $ int 10
  , While (EApply (Address "gt") [EAddr n, int 0])
    [ Print $ EAddr n
    , Store m $ int 5
    , While (EApply (Address "gt") [EAddr n, int 0])
      [ Print $ EAddr m
      , Store m $ EApply (Address "sub") [EAddr m, int 1]
      ]
    , Store n $ EApply (Address "sub") [EAddr n, int 1]
    ]
  , Print $ int 42
  ]
  where
    n = Address "n"
    m = Address "m"
    int = EConst . CInt

tt = toBlockMap
  [ Store n $ int 10
  , While (EApply (Address "gt") [EAddr n, int 0])
    [ Print $ EAddr n
    , Store n $ EApply (Address "sub") [EAddr n, int 1]
    ]
  , Print $ int 42
  ]
  where
    n = Address "n"
    int = EConst . CInt

-- start:
--   n := 10
--   while n > 0
--     print n
--     n = n - 1

-}
{- The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/
  
   contributed by Christoph Bauer
   modified by Brett Letner
-}

-- IntConst String Signed Integer !SrcLoc	 
-- LongIntConst String Signed Integer !SrcLoc	 
-- LongLongIntConst String Signed Integer !SrcLoc	 
-- FloatConst String Rational !SrcLoc	 
-- DoubleConst String Rational !SrcLoc	 
-- LongDoubleConst String Rational !SrcLoc	 
-- CharConst String Char !SrcLoc	 
-- StringConst [String] String !SrcLoc
  
un = undefNode

instance CNum Word where
  cnum _ x =
    CConst $ CIntConst (CInteger x DecRepr $ setFlag FlagUnsigned noFlags) un

cdouble :: Double -> CExpr
cdouble x = CConst $ CFloatConst (CFloat $ show x) un

class Typed a => CNum a where cnum :: a -> Integer -> CExpr
class Typed a => CFractional a where cfractional :: a -> Rational -> CExpr
instance CNum Double where cnum _ = cdouble . fromIntegral
instance CFractional Double where cfractional _ = cdouble . fromRational

unused = error "unused"

cvar x = CVar (cident x) un

cident = internalIdent

cbvar :: BVar -> String
cbvar (BVar a) = "v" ++ show a

data U
  = I Integer Type
  | R Rational Type
  | FV FVar
  | BV BVar Type
  | Load U
  | Apps [U]
  deriving Show
           
icase :: (Word -> Integer -> b) -> (Double -> Integer -> b) -> Integer -> Type -> b
icase f g x t = case () of
    () | t == tword -> f (fromInteger x) x
       | t == tdouble -> g (fromInteger x) x
       | otherwise -> undef "icase"

rcase :: (Double -> Rational -> b) -> Rational -> Type -> b
rcase g x t = case () of
    () | t == tdouble -> g (fromRational x) x
       | otherwise -> undef "rcase"
                      
ppE :: E a -> Doc
ppE x = case unE x of
  BV (BVar a) _ -> text $ "%" ++ show a
  FV (FVar a) -> text a
  I a t -> text $ icase (\_ -> show) (\_ -> show) a t
  R a t -> text $ rcase (\_ -> show) a t
  Apps bs -> parens $ hcat $ map (ppE . E) bs
    
cexpr :: E a -> CExpr
cexpr (x :: E a) = case unE x of
  I a t -> icase cnum cnum a t
  R a t -> rcase cfractional a t
  FV (FVar a) -> cvar a
  BV a (TRef t) -> f $ cvar $ cbvar a
    where
      f = case t of
        TName{} -> caddrof
        TArray{} -> id
  Load a -> cload $ cexpr $ E a
  Apps bs -> case (s, lookup s cbuiltins) of
    ('.':fld, _) -> cunaryf (cfield fld) bs'
    (_, Just f) -> f bs'
    _ -> a C.# bs'
    where
      a@(CVar v _) : bs' = map (cexpr . E) bs
      s = identToString v

-- cexprs :: E a -> [CExpr]
-- cexprs x = case x of
--   App a b -> cexprs a ++ [cexpr b]
--   _ -> [cexpr x]

-- toCExpr :: Exp -> M CExpr
-- toCExpr x = case x of
--   EI a -> return $ cnum (unused :: Word) a -- BAL: handle polymorphism
--   ER a -> return $ cfractional (unused :: Double) a -- BAL: handle polymorphism
--   EFV a -> return $ cvar a
--   EBV a t -> return $ f $ cvar $ cbvar a
--     where
--       f = case t of
--         TName{} -> caddrof
--         TArray{} -> id
--   EApp us -> do
--     a@(CVar v _) : bs <- mapM toCExprArg us
--     let s = identToString v
--     return $ case (s, lookup s cbuiltins) of
--       ('.':fld, _) -> cunaryf (cfield fld) bs
--       (_, Just f) -> f bs
--       _ -> a C.# bs

-- toCExprArg :: Key -> M CExpr
-- toCExprArg k = do
--   mR <- gets keyMap
--   case M.lookup k mR of
--     Nothing -> undef "toCExprArg"
--     Just a -> case a of
--       EApp{} -> return $ cvar $ "k" ++ show k -- BAL: fixme
--       _ -> toCExpr a

-- runM :: M a -> (a, St)
-- runM = flip runState initSt

-- baz :: M () -> IO ()
-- baz m = mapM_ (print . pretty) es
--   where
--   (es, st) = runM $ do
--     _ <- toKey $ bar (execM m) (FV "world")
--     mR <- gets keyMap
--     liftM catMaybes $ mapM foof $ M.toList mR

-- foof :: (Key, Exp) -> M (Maybe CExpr)
-- foof (x,y@EApp{}) = do
--   e <- toCExpr y
--   return $ Just $ CAssign CAssignOp (cvar $ "k" ++ show x) e un
-- foof _ = return Nothing

ccode :: M () -> IO ()
-- ccode = writeFile "gen.c" . show . pretty . everywhere (mkT elimCAdrOp) . cblock . execM
ccode m = print $ pretty $ everywhere (mkT elimCAdrOp) $ ccompound $ map bvarcstat bvs ++ map cstat ss
  where
    (bvs, ss) = execM m

ccompound :: [CBlockItem] -> CStat
ccompound xs = CCompound [] xs un

sortByFst = sortBy (\a b -> compare (fst a) (fst b))

lastBlock :: B ()
lastBlock = newBlock Exit (Label (-1)) []

newBlock :: Control -> Label -> [Label] -> B ()
newBlock x y zs = do
  modify $ \st ->
    st{ blockMap =
           M.insert (currentLabel st) (reverse $ currentInstrs st, x) $
           blockMap st
      , currentInstrs = []
      , currentLabel = y
      }

initBSt = BSt (Register 0) (Label 1) (Label 0) [] M.empty M.empty

execB :: ([(BVar, Type)], [Stmt]) -> BSt
execB (_bvs, ss) = execState (mapM_ toBBlocks ss >> lastBlock) initBSt

execM :: M () -> ([(BVar, Type)], [Stmt])
execM x = (sortByFst $ bvars st, ss)
  where
    (ss, st) = runState (x >> popBlock) initSt

cbinary o a b = CBinary o a b un
  
cbuiltins :: [(String, [CExpr] -> CExpr)]
cbuiltins =
  [ ("+", cbinaryf $ cbinary CAddOp)
  , ("*", cbinaryf $ cbinary CMulOp)
  , ("-", cbinaryf $ cbinary CSubOp)
  , ("/", cbinaryf $ cbinary CDivOp)
  , ("==", cbinaryf $ cbinary CNeqOp)
  , ("/=", cbinaryf $ cbinary CEqOp)
  , ("<=", cbinaryf $ cbinary CLeqOp)
  , (">=", cbinaryf $ cbinary CGeqOp)
  , (">", cbinaryf $ cbinary CGrOp)
  , ("<", cbinaryf $ cbinary CLeOp)
  , ("ix", cbinaryf $ \a b -> caddrof $ CIndex a b un)
  ]

cunaryf f = \[x] -> f x

cbinaryf f = \[x,y] -> f x y

type Var = Either BVar FVar

data E a = E{ unE :: U }

load :: E (Ref a) -> E a
load = E . Load . unE

bvar :: Typed a => BVar -> E (Ref a)
bvar x = let e = E $ BV x $ typeof e in e

fvar :: FVar -> E a
fvar = E . FV

global :: String -> E a
global = fvar . FVar

int :: (Num a, Show a, CNum a) => Integer -> E a
int x = let e = E $ I x $ typeof e in e

rat :: (Show a, Fractional a, CFractional a) => Rational -> E a
rat x = let e = E $ R x $ typeof e in e

unop :: String -> E a -> E b
unop v = app (global v)

binop :: String -> E a -> E b -> E c
binop v a b = app (unop v a) b

app :: E (b -> a) -> E b -> E a
app x y = case unE x of
  Apps bs -> E $ Apps $ bs ++ [unE y]
  a -> app (E $ Apps [a]) y

tdouble = TName "double"
tword = TName "uint32_t"

class Typed a where typeof :: E a -> Type
instance Typed Double where typeof _ = tdouble
instance Typed Word where typeof _ = tword
instance Typed Body where typeof _ = TName "body_t"

data Stmt
  = While (E Bool) Block
  | forall a . Store (E (Ref a)) (E a)
  | forall a . Print (E a)

data AExp
  = Reg Register
  | Int Integer
  | Rat Rational
    deriving Show

var :: E a -> B Var
var x = case x of
  -- BV a -> return $ Left a
  -- FV a -> return $ Right a
  _ -> undef $ "var:" ++ show x

aexp :: E a -> B AExp
aexp x = case x of
  _ -> undefined
  -- I a -> return $ Int a
  -- R a -> return $ Rat a
  -- Load a -> do
  --   v <- var a
  --   m <- gets varMap
  --   case M.lookup v m of
  --     Nothing -> do
  --       r <- newRegister
  --       instr $ ILoad r v
  --       let e = Reg r
  --       modify $ \st -> st{ varMap = M.insert v e m }
  --       return e
  --     Just a -> return a
  -- App{} -> do
  --   (a,bs) <- aexps x
  --   r <- newRegister
  --   instr $ ILet r $ Call a $ reverse bs
  --   return $ Reg r

-- aexps :: E a -> B (FVar, [AExp])
-- aexps x = case x of
--   FV a -> return (a, [])
--   App a b -> do
--     (a', bs) <- aexps a
--     b' <- aexp b
--     return (a', b':bs)
--   BV{} -> undef "aexps:BV"
--   I{} -> undef "aexps:I"
--   R{} -> undef "aexps:R"
  
newRegister :: B Register
newRegister = do
  next@(Register i) <- gets nextRegister
  modify $ \st -> st{ nextRegister = Register $ succ i }
  return next

instr :: Instr -> B ()
instr x = modify $ \st -> st{ currentInstrs = x : currentInstrs st }

isStore Store{} = True
isStore _ = False

phiNodes :: [Stmt] -> B [(Var, AExp, Register)]
phiNodes xs = do
  m <- gets varMap
  ks <- nub <$> sequence [ var a | Store a _ <- undefined ] -- listify isStore xs ]
  let
    f :: Var -> B (Maybe (Var, AExp, Register))
    f a = case M.lookup a m of
      Nothing -> return Nothing
      Just e -> do
        r <- newRegister
        return $ Just (a, e, r)
  catMaybes <$> mapM f ks

phiInstr m lbl0 lbl1 (a, e0, r) = case M.lookup a m of
  Nothing -> undef "phiInstr"
  Just e1 -> do
    modify $ \st -> st{ varMap = M.insert a (Reg r) $ varMap st }
    instr $ ILet r $ Phi [(e0, lbl0), (e1, lbl1)]

newLabel = do
  next@(Label i) <- gets nextLabel
  modify $ \st -> st{ nextLabel = Label $ succ i }
  return next

toBBlocks :: Stmt -> B ()
toBBlocks x = case x of
  Print a -> IPrint <$> aexp a >>= instr
  Store a b -> do
    v <- var a
    e <- aexp b
    -- BAL: don't do store if lookup reveals value is already set to the correct value
    modify $ \st -> st{ varMap = M.insert v e $ varMap st }
  --   instr $ IStore a e
  While a (Block bs) -> do
    -- BAL: don't do a new block if cond exp is a constant
    prev <- gets currentLabel
    cond <- newLabel
    body <- newLabel
    phis <- phiNodes bs
    newBlock (Jump cond) body [cond]
    modify $ \st -> st{ varMap = foldr (\(k, _, v) -> M.insert k $ Reg v) (varMap st) phis }
    mapM_ toBBlocks bs
    newBlock (Jump cond) cond [prev, body]
    gets varMap >>= \m -> mapM_ (phiInstr m prev body) phis
    Reg r <- aexp a
    done <- newLabel
    newBlock (Cond r body done) done [cond]

-- t = execB $ execM $ do
--   let n = global "n"
--   let printw :: E Word -> M () = printf
--   printw 42
--   printw $ load n
--   m <- new 12
--   printw $ load m
--   m .= 13
--   printw $ load m
  
data CExp
  = Call FVar [AExp]
  | Phi [(AExp, Label)]
    deriving Show

data Instr
  = ILet Register CExp
  | IPrint AExp
  | ILoad Register Var
  -- | IStore Var AExp
  deriving Show
    
newtype Block = Block [Stmt]

data Type
  = TName String
  | TArray Type Word
  | TRef Type
  deriving (Show, Eq, Ord, Generic)
instance Hashable Type
  
data Ref a
data Array a b

type M a = State St a

newtype BVar = BVar Integer deriving  (Show, Eq, Generic, Ord)
instance Hashable BVar
newtype FVar = FVar String deriving  (Show, Eq, Generic, Ord)
instance Hashable FVar
newtype Register = Register Integer deriving  (Show, Eq, Generic, Ord)
newtype Label = Label Integer deriving (Show, Eq, Generic, Ord)
instance Hashable Label

data Control
  = Jump Label
  | Cond Register Label Label
  | Exit
    deriving Show

data St = St
  { nextBVar :: BVar
  , stmts :: [[Stmt]]
  , bvars :: [(BVar, Type)]
  -- , expMap :: M.HashMap Exp Word -- BAL: should be in separate monad state -- BAL: bimap?
  -- , keyMap :: M.HashMap Word Exp -- BAL: should be in separate monad state -- BAL: bimap?
  } deriving Show

initSt :: St
initSt = St (BVar 0) [[]] [] -- M.empty M.empty

type B a = State BSt a

data BSt = BSt
  { nextRegister :: Register
  , nextLabel :: Label
  , currentLabel :: Label
  , currentInstrs :: [Instr]
  , blockMap :: M.HashMap Label ([Instr], Control) -- BAL: add prev labels for llvm comment?
  , varMap :: M.HashMap Var AExp
  } deriving Show

-- type Key = Word

-- data Exp
--   = EBV Word Type
--   | EFV String
--   | EI Integer
--   | ER Rational
--   | EApp [Key]
--   | EIf Key Key Key
--     deriving (Show, Eq, Generic, Ord)
-- instance Hashable Exp

-- data World
  
-- while = \x y z -> if x then (y (while x y z)) else z
-- while :: E Bool -> ((World -> World) -> (World -> World)) -> (World -> World) -> (World -> World)
-- store :: E (Ref a) -> E a -> (World -> World)
-- print :: E a -> (World -> World)
-- alloc :: E Word -> E (Ref a) -> (World -> World)

-- foo :: Stmt -> E World -> E World
-- foo x y = case x of
--   Print a -> binop "printf" a y
--   Store a b -> ternop "store" a b y
--   While a b -> undefined

-- bar :: Block -> E World -> E World
-- bar (Block xs) y = foldl' (flip foo) y xs

while :: E Bool -> M () -> M ()
while x y = do
  pushBlock
  y
  ss <- popBlock
  stmt $ While x $ Block ss

infix 4 .=
(.=) :: E (Ref a) -> E a -> M ()
(.=) x y = stmt $ Store x y
    
printf :: E a -> M ()
printf x = stmt $ Print x

alloc :: Typed a => M (E (Ref a))
alloc = do
  bv@(BVar i) <- gets nextBVar
  let v = bvar bv
  modify $ \st -> st{ nextBVar = BVar $ succ i, bvars = (bv, typeof v) : bvars st }
  return v

stmt s = do
  ss:bs <- gets stmts
  modify $ \st -> st{ stmts = (s:ss):bs }

-- toKey :: E a -> M Key
-- toKey x = case x of
--   BV a -> nameExp $ EBV a $ typeof x
--   FV a -> nameExp $ EFV a
--   I a -> nameExp $ EI a
--   R a -> nameExp $ ER a
--   App{} -> liftM EApp (toKeys x) >>= nameExp

-- toKeys :: E a -> M [Key]
-- toKeys x = case x of
--   App a b -> do
--     k <- toKey b
--     ks <- toKeys a
--     return $ ks ++ [k]
--   _ -> toKey x >>= \k -> return [k]

-- nameExp :: Exp -> M Key
-- nameExp k = do
--   m <- gets expMap
--   case M.lookup k m of
--     Nothing -> do
--       i <- gets unique
--       modify $ \st -> st{ unique = succ i, expMap = M.insert k i m, keyMap = M.insert i k $ keyMap st }
--       return i
--     Just v -> return v
    
instance Show (E a) where show = show . ppE

ppBlock (Block ss) = vcat $ map ppS ss

instance Show Block where show = show . ppBlock
                          
ppS :: Stmt -> Doc
ppS x = case x of
  While a bs -> hang (text "while" <+> ppE a) 2 $ ppBlock bs
  Store a b -> hsep [ppE a, text ".=", ppE b]
  Print a -> hsep [text "print", ppE a]

instance Show Stmt where show = show . ppS

-- #include <math.h>
-- #include <stdio.h>
-- #include <stdlib.h>

-- #define solar_mass (4 * pi * pi)
solar_mass = 4*pi^2
-- #define days_per_year 365.24
days_per_year = 365.24

-- struct planet {
--   double x, y, z;
--   double vx, vy, vz;
--   double mass;
-- };
data Body = Body
  { _x :: E Double
  , _y :: E Double
  , _z :: E Double
  , _vx :: E Double
  , _vy :: E Double
  , _vz :: E Double
  , _mass :: E Double
  }
  deriving Show

infixl 8 ##
(##) :: E (Ref a) -> E (Ref a -> Ref b) -> E b
(##) x = load . (#) x

instance (Typed a) => Typed (Ref a) where
  typeof (_ :: E (Ref a)) = TRef $ typeof (unused :: E a)

instance (Typed a, Count b) => Typed (Array a b) where
  typeof _ = TArray (typeof (unused :: E a)) (countof (unused :: b))
                           
new :: Typed a => E a -> M (E (Ref a))
new x = alloc >>= \p -> p .= x >> return p

ix :: E (Ref (Array a b)) -> E Word -> E (Ref a)
ix = binop "ix"

pushBlock = modify $ \st -> st{ stmts = [] : stmts st }
popBlock = do
  b:bs <- gets stmts
  modify $ \st -> st{ stmts = bs }
  return $ reverse b

cfield :: String -> CExpr -> CExpr
cfield x y = caddrof $ CMember y (cident x) True un

cload :: CExpr -> CExpr
cload x = CUnary CIndOp x un

caddrof x = CUnary CAdrOp x un

elimCAdrOp :: CExpr -> CExpr
elimCAdrOp x = case x of
  CMember (CUnary CAdrOp a _) b True c -> CMember a b False c
  CUnary CIndOp (CUnary CAdrOp x _) _ -> x
  _ -> x

cestmt x = CBlockStmt $ CExpr (Just x) un
cstring x = CConst $ CStrConst (CString x False) un
           
cstat :: Stmt -> CBlockItem
cstat x = case x of
  While a (Block b) -> CBlockStmt $ CWhile (cexpr a) (ccompound $ map cstat b) False un
  Store a b -> cestmt $ CAssign CAssignOp (cload $ cexpr a) (cexpr b) un
  Print a -> cestmt $ CCall (cvar "printf") [cstring "%.9f\n", cexpr a] un -- BAL: do based on type

bvarcstat :: (BVar, Type) -> CBlockItem
bvarcstat (b, TRef ty) = case ty of
  TArray (TName t) n -> cdecl t [CArrDeclr [] (CArrSize False $ cexpr ((fromIntegral n) :: E Word)) un]
  TName t -> cdecl t []
  _ -> undef "cstat:TArray"
  where
    cdecl t cs =
      CBlockDecl $ decl (CTypeSpec $ CTypeDef (cident t) un) (CDeclr (Just $ cident $ cbvar b) cs Nothing [] un) Nothing

infix 4 <.
(<.) :: (Ord a, Typed a) => E a -> E a -> E Bool
(<.) = binop "<"

infix 4 >.
(>.) :: (Ord a, Typed a) => E a -> E a -> E Bool
(>.) = binop ">"

infix 4 <=.
(<=.) :: (Ord a, Typed a) => E a -> E a -> E Bool
(<=.) = binop "<="

infix 4 >=.
(>=.) :: (Ord a, Typed a) => E a -> E a -> E Bool
(>=.) = binop ">="

infix 4 ==.
(==.) :: (Ord a, Typed a) => E a -> E a -> E Bool
(==.) = binop "=="

infix 4 /=.
(/=.) :: (Ord a, Typed a) => E a -> E a -> E Bool
(/=.) = binop "/="

loop :: E Word -> (E Word -> M ()) -> M ()
loop = loopNM 0

each :: Count b => E (Ref (Array a b)) -> (E (Ref a) -> E Word -> M ()) -> M ()
each = eachN 0

eachN :: Count b => E Word -> E (Ref (Array a b)) -> (E (Ref a) -> E Word -> M ()) -> M ()
eachN i arr f = loopNM i (count arr) $ \i -> f (ix arr i) i

loopNM :: E Word -> E Word -> (E Word -> M ()) -> M ()
loopNM x y f = do
  i <- new x
  while (load i <. y) $ do
    f $ load i
    inc i

inc x = x += 1

infix 4 +=
(+=) :: (Show a, Num a, CNum a) => E (Ref a) -> E a -> M ()
(+=) = adjust (+)

infix 4 -=
(-=) :: (Num a, Show a, CNum a) => E (Ref a) -> E a -> M ()
(-=) = adjust (-)

adjust f x y = x .= f (load x) y

infixl 9 #
(#) :: E b -> E (b -> a) -> E a
(#) = flip app
  
x :: E (Ref Body -> Ref Double)
x = global ".x"
y :: E (Ref Body -> Ref Double)
y = global ".y"
z :: E (Ref Body -> Ref Double)
z = global ".z"
vx :: E (Ref Body -> Ref Double)
vx = global ".vx"
vy :: E (Ref Body -> Ref Double)
vy = global ".vy"
vz :: E (Ref Body -> Ref Double)
vz = global ".vz"
mass :: E (Ref Body -> Ref Double)
mass = global ".mass"

count :: Count b => E (Ref (Array a b)) -> E Word
count (_ :: E (Ref (Array a b))) = fromIntegral $ countof (unused :: b)

class Count a where
  countof :: a -> Word

instance Count Five where countof _ = 5

data Five = Five

-- void advance(int nbodies, struct planet * bodies, double dt)
-- {
--   int i, j;
advance :: Count b => E (Ref (Array Body b)) -> E Double -> M ()
advance bodies dt = do
--   for (i = 0; i < nbodies; i++) {
--     struct planet * b = &(bodies[i]);
--     for (j = i + 1; j < nbodies; j++) {
--       struct planet * b2 = &(bodies[j]);
--       double dx = b->x - b2->x;
--       double dy = b->y - b2->y;
--       double dz = b->z - b2->z;
--       double distance = sqrt(dx * dx + dy * dy + dz * dz);
--       double mag = dt / (distance * distance * distance);
--       b->vx -= dx * b2->mass * mag;
--       b->vy -= dy * b2->mass * mag;
--       b->vz -= dz * b2->mass * mag;
--       b2->vx += dx * b->mass * mag;
--       b2->vy += dy * b->mass * mag;
--       b2->vz += dz * b->mass * mag;
--     }
--   }

  each bodies $ \b i -> do
    eachN (i + 1) bodies $ \b2 _ -> do
      let
        f p = b##p - b2##p
        dx = f x
        dy = f y
        dz = f z
        distance = sqrt $ dx^2 + dy^2 + dz^2
        mag = dt / distance^3
      let f p q = b#p -= q * b2##mass * mag
      f vx dx
      f vy dy
      f vz dz
      let f p q = b2#p += q * b##mass * mag
      f vx dx
      f vy dy
      f vz dz
      
--   for (i = 0; i < nbodies; i++) {
--     struct planet * b = &(bodies[i]);
--     b->x += dt * b->vx;
--     b->y += dt * b->vy;
--     b->z += dt * b->vz;
--   }
-- }
  each bodies $ \b _ -> do
    let f p q = b#p += (dt * (b##q))
    f x vx
    f y vy
    f z vz

-- ternop :: String -> E a -> E b -> E c -> E d
-- ternop v a b c = app (binop v a b) c

binopE :: Typed a => String -> (Rational -> Rational -> Rational) -> (Integer -> Integer -> Integer) -> E a -> E a -> E a
binopE v _f _g x y = case (x,y) of
  -- (R a, R b) -> R $ f a b
  -- (I a, I b) -> I $ g a b
  -- (I a, R _) -> binopE v f g (R $ fromIntegral a) y
  -- (R _, I b) -> binopE v f g x (R $ fromIntegral b)
  _ -> binop v x y
    
instance (Show a, Num a, CNum a) => Num (E a) where
  (+) = binopE "+" (+) (+)
  (*) = binopE "*" (*) (*)
  abs = unop "abs"
  signum = unop "signum"
  fromInteger = int
  (-) = binopE "-" (-) (-)
  
instance (Show a, Fractional a, CNum a, CFractional a) => Fractional (E a) where
  fromRational = rat
  (/) = binopE "/" (/) (undef "/")
  
instance (Show a, Floating a, CNum a, CFractional a) => Floating (E a) where
  -- #define pi 3.141592653589793
  pi = 3.141592653589793
  exp = unop "exp"
  log = unop "log"
  sin = unop "sin"
  cos = unop "cos"
  asin = unop "asin"
  atan = unop "atan"
  acos = unop "acos"
  sinh = unop "sinh"
  cosh = unop "cosh"
  asinh = unop "asinh"
  atanh = unop "atanh"
  acosh = unop "acosh"
  
-- double energy(int nbodies, struct planet * bodies)
-- {
--   double e;
--   int i, j;

--   e = 0.0;
--   for (i = 0; i < nbodies; i++) {
--     struct planet * b = &(bodies[i]);
--     e += 0.5 * b->mass * (b->vx * b->vx + b->vy * b->vy + b->vz * b->vz);
--     for (j = i + 1; j < nbodies; j++) {
--       struct planet * b2 = &(bodies[j]);
--       double dx = b->x - b2->x;
--       double dy = b->y - b2->y;
--       double dz = b->z - b2->z;
--       double distance = sqrt(dx * dx + dy * dy + dz * dz);
--       e -= (b->mass * b2->mass) / distance;
--     }
--   }
--   return e;
-- }

energy :: Count b => E (Ref (Array Body b)) -> M (E Double)
energy bodies = do
  e <- new 0
  each bodies $ \b i -> do
    e += 0.5 * b##mass * ((b##vx)^2 + (b##vy)^2 + (b##vz)^2)
    eachN (i + 1) bodies $ \b2 _ -> do
      let
        f p = b##p - b2##p
        dx = f x
        dy = f y
        dz = f z
        distance = sqrt $ dx^2 + dy^2 + dz^2
      e -= (b##mass * b2##mass) / distance
      printf $ load e
  return $ load e

-- t = printf $ 3 + (2 :: E Word)

-- void offset_momentum(int nbodies, struct planet * bodies)
-- {
--   double px = 0.0, py = 0.0, pz = 0.0;
--   int i;
--   for (i = 0; i < nbodies; i++) {
--     px += bodies[i].vx * bodies[i].mass;
--     py += bodies[i].vy * bodies[i].mass;
--     pz += bodies[i].vz * bodies[i].mass;
--   }
--   bodies[0].vx = - px / solar_mass;
--   bodies[0].vy = - py / solar_mass;
--   bodies[0].vz = - pz / solar_mass;
-- }

offset_momentum :: E (Ref (Array Body Five)) -> M ()
offset_momentum bodies = do
  px <- new 0
  py <- new 0
  pz <- new 0
  each bodies $ \b _ -> do
    let f p q = p += b##q * b##mass
    f px vx
    f py vy
    f pz vz
  let f p q = ix bodies 0#p .= - load q / solar_mass
  f vx px
  f vy py
  f vz pz
  
-- #define NBODIES 5
-- struct planet bodies[NBODIES] = {
--   {                               /* sun */
--     0, 0, 0, 0, 0, 0, solar_mass
--   },
--   {                               /* jupiter */
--     4.84143144246472090e+00,
--     -1.16032004402742839e+00,
--     -1.03622044471123109e-01,
--     1.66007664274403694e-03 * days_per_year,
--     7.69901118419740425e-03 * days_per_year,
--     -6.90460016972063023e-05 * days_per_year,
--     9.54791938424326609e-04 * solar_mass
--   },
--   {                               /* saturn */
--     8.34336671824457987e+00,
--     4.12479856412430479e+00,
--     -4.03523417114321381e-01,
--     -2.76742510726862411e-03 * days_per_year,
--     4.99852801234917238e-03 * days_per_year,
--     2.30417297573763929e-05 * days_per_year,
--     2.85885980666130812e-04 * solar_mass
--   },
--   {                               /* uranus */
--     1.28943695621391310e+01,
--     -1.51111514016986312e+01,
--     -2.23307578892655734e-01,
--     2.96460137564761618e-03 * days_per_year,
--     2.37847173959480950e-03 * days_per_year,
--     -2.96589568540237556e-05 * days_per_year,
--     4.36624404335156298e-05 * solar_mass
--   },
--   {                               /* neptune */
--     1.53796971148509165e+01,
--     -2.59193146099879641e+01,
--     1.79258772950371181e-01,
--     2.68067772490389322e-03 * days_per_year,
--     1.62824170038242295e-03 * days_per_year,
--     -9.51592254519715870e-05 * days_per_year,
--     5.15138902046611451e-05 * solar_mass
--   }
-- };

indices :: [E Word]
indices = map fromIntegral [0 :: Word .. ]

assertM x = assert x $ return ()

newArray :: (Typed a, Count cnt) => cnt -> (E (Ref a) -> b -> M ()) -> [b] -> M (E (Ref (Array a cnt)))
newArray cnt f xs = do
  let n = fromIntegral $ length xs
  assertM (countof cnt == n)
  arr <- alloc
  sequence_ [ f (ix arr i) x | (x, i) <- zip xs indices ]
  return arr

initBody :: E (Ref Body) -> (E Double, E Double, E Double, E Double, E Double, E Double, E Double) -> M ()
initBody body (a, b, c, d, e, f, g) = do
  body#x .= a
  body#y .= b
  body#z .= c
  body#vx .= d * days_per_year
  body#vy .= e * days_per_year
  body#vz .= f * days_per_year
  body#mass .= g * solar_mass
  
newBodies = newArray Five initBody
  [ (0, 0, 0, 0, 0, 0, 1) {- sun -}
  , {- jupiter -}
    ( 4.84143144246472090e+00,
     -1.16032004402742839e+00,
     -1.03622044471123109e-01,
     1.66007664274403694e-03,
     7.69901118419740425e-03,
     -6.90460016972063023e-05,
     9.54791938424326609e-04
    )
  , {- saturn -}
    ( 8.34336671824457987e+00,
      4.12479856412430479e+00,
     -4.03523417114321381e-01,
     -2.76742510726862411e-03,
      4.99852801234917238e-03,
      2.30417297573763929e-05,
      2.85885980666130812e-04
    )
  , {- uranus -}
    ( 1.28943695621391310e+01,
     -1.51111514016986312e+01,
     -2.23307578892655734e-01,
      2.96460137564761618e-03,
      2.37847173959480950e-03,
     -2.96589568540237556e-05,
      4.36624404335156298e-05
    )
  , {- neptune -}
    ( 1.53796971148509165e+01,
     -2.59193146099879641e+01,
      1.79258772950371181e-01,
      2.68067772490389322e-03,
      1.62824170038242295e-03,
     -9.51592254519715870e-05,
      5.15138902046611451e-05
     )
  ]

main = ccode main_

debug_print :: Count b => E (Ref (Array Body b)) -> M ()
debug_print bodies = do
  each bodies $ \b _ -> do
    let f p = printf (b##p)
    f x
    f y
    f z
    f vx
    f vy
    f vz
    f mass
  energy bodies >>= printf

main_ :: M ()
main_ = do
  bodies <- newBodies
  -- debug_print bodies
  offset_momentum bodies
  -- debug_print bodies
  let f = energy bodies >>= printf
  f
  loop (global "n") $ \_ -> advance bodies 0.01
  -- debug_print bodies
  f
  
-- int main(int argc, char ** argv)
-- {
--   int n = atoi(argv[1]);
--   int i;

--   offset_momentum(NBODIES, bodies);
--   printf ("%.9f\n", energy(NBODIES, bodies));
--   for (i = 1; i <= n; i++)
--     advance(NBODIES, bodies, 0.01);
--   printf ("%.9f\n", energy(NBODIES, bodies));
--   return 0;
-- }

-- /*
-- make, command-line, and program output logs
-- Thu, 24 Apr 2014 03:05:09 GMT

-- MAKE:
-- /usr/bin/gcc -pipe -Wall -O3 -fomit-frame-pointer -march=native -mfpmath=sse -msse3 nbody.c -o nbody.gcc_run -lm
-- rm nbody.c
-- 0.18s to complete and log all make actions

-- COMMAND LINE:
-- ./nbody.gcc_run 50000000

-- PROGRAM OUTPUT:
-- -0.169075164
-- -0.169059907
-- */
