{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module NBody where

import Data.Word
import Control.Applicative
import Control.Monad.State
import Language.C.DSL hiding ((#), (.=), for, while)
import qualified Language.C.DSL as C

-- import Language.C
-- import Language.C.Syntax
import Text.PrettyPrint

undef = error . (++) "undefined:"

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

class CNum a where
  cnum :: a -> Integer -> CExpr

class CFractional a where
  cfractional :: a -> Rational -> CExpr
  
un = undefNode

instance CNum Word where
  cnum _ x =
    CConst $ CIntConst (CInteger x DecRepr $ setFlag FlagUnsigned noFlags) un

cdouble :: Double -> CExpr
cdouble x = CConst $ CFloatConst (CFloat $ show x) un

instance CNum Double where cnum _ = cdouble . fromIntegral
instance CFractional Double where cfractional _ = cdouble . fromRational

unused = error "unused"

cvar x = CVar (cident x) un

cident = internalIdent

cfield :: String -> CExpr -> CExpr
cfield x y = CMember y (cident x) True un

cbvar :: Word -> String
cbvar a = "v" ++ show a

cexpr :: E a -> CExpr
cexpr (x :: E a) = case x of
  I a -> cnum (unused :: a) a
  R a -> cfractional (unused :: a) a
  FV a -> cvar a
  BV a -> cvar $ cbvar a
  App{} -> case (s, lookup s cbuiltins) of
    ('.':fld, _) -> cunaryf (cfield fld) bs
    (_, Just f) -> f bs
    _ -> a C.# bs
    where
      a@(CVar v _) : bs = cexprs x
      s = identToString v

ccode = writeFile "gen.c" . show . pretty . cblock

cbinary o a b = CBinary o a b un
  
cbuiltins =
  [ ("load", cunaryf cload)
  , ("+", cbinaryf $ cbinary CAddOp)
  , ("*", cbinaryf $ cbinary CMulOp)
  , ("-", cbinaryf $ cbinary CSubOp)
  , ("/", cbinaryf $ cbinary CDivOp)
  , ("==", cbinaryf $ cbinary CNeqOp)
  , ("/=", cbinaryf $ cbinary CEqOp)
  , ("<=", cbinaryf $ cbinary CLeqOp)
  , (">=", cbinaryf $ cbinary CGeqOp)
  , (">", cbinaryf $ cbinary CGrOp)
  , ("<", cbinaryf $ cbinary CLeOp)
  , ("ix", cbinaryf $ \a b -> CIndex a b un)
  ]

cunaryf f = \[x] -> f x

cbinaryf f = \[x,y] -> f x y

cexprs :: E a -> [CExpr]
cexprs x = case x of
  App a b -> cexprs a ++ [cexpr b]
  _ -> [cexpr x]

data E a where
  BV :: Word -> E a
  FV :: String -> E a
  I :: (Num a, Show a, CNum a) => Integer -> E a
  R :: (Show a, Fractional a, CFractional a) => Rational -> E a
  App :: E (b -> a) -> E b -> E a

ppE :: E a -> Doc
ppE (x :: E a) = case x of
  BV a -> text $ "%" ++ show a
  FV a -> text a
  I a -> text $ show ((fromInteger a) :: a)
  R a -> text $ show ((fromRational a) :: a)
  App a b -> parens $ ppE a <+> ppE b
    
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
      
alloc :: E Word -> M (E (Ref a))
alloc sz = do
  i <- gets unique
  modify $ \st -> st{ unique = succ i }
  let v = BV i
  stmt $ Alloc sz v
  return v

mk :: E a -> M (E (Ref a))
mk x = alloc 1 >>= \p -> p .= x >> return p

ix :: E (Ref (Array a)) -> E Word -> E (Ref a)
ix = binop "ix"

load :: E (Ref a) -> E a
load = unop "load"

pushBlock = modify $ \st -> st{ stmts = [] : stmts st }
popBlock = do
  b:bs <- gets stmts
  modify $ \st -> st{ stmts = bs }
  return b

stmt s = do
  ss:bs <- gets stmts
  modify $ \st -> st{ stmts = (s:ss):bs }

while :: E Bool -> M () -> M ()
while x y = do
  pushBlock
  y
  ss <- popBlock
  stmt $ While x $ mkBlock ss

newtype Block =  Block [Stmt]

-- CStat
-- CBlockItem

cload x = CUnary CIndOp x un
cestmt x = CBlockStmt $ CExpr (Just x) un
cstring x = CConst $ CStrConst (CString x False) un

cstat :: Stmt -> CBlockItem
cstat x = case x of
  While a b -> CBlockStmt $ CWhile (cexpr a) (cblock b) False un
  Store a b -> cestmt $ CAssign CAssignOp (cload $ cexpr a) (cexpr b) un
  Print a -> cestmt $ CCall (cvar "printf") [cstring "%.9f\n", cload $ cexpr a] un
  Alloc a (BV b) -> CBlockDecl $ decl (CTypeSpec $ CTypeDef (cident "asdf") un) (CDeclr (Just $ cident $ cbvar b) [CArrDeclr [] (CArrSize False $ cexpr a) un] Nothing [] un) Nothing
    
cblock :: Block -> CStat
cblock (Block xs) = CCompound [] (map cstat xs) un

mkBlock :: [Stmt] -> Block
mkBlock = Block . reverse

data Stmt where
  While :: E Bool -> Block -> Stmt
  Store :: E (Ref a) -> E a -> Stmt
  Print :: E a -> Stmt
  Alloc :: E Word -> E (Ref a) -> Stmt
    
printf :: E a -> M ()
printf x = stmt $ Print x

infix 4 .=
(.=) :: E (Ref a) -> E a -> M ()
(.=) x y = stmt $ Store x y

infix 4 <.
(<.) :: Ord a => E a -> E a -> E Bool
(<.) = binop "<"

infix 4 >.
(>.) :: Ord a => E a -> E a -> E Bool
(>.) = binop ">"

infix 4 <=.
(<=.) :: Ord a => E a -> E a -> E Bool
(<=.) = binop "<="

infix 4 >=.
(>=.) :: Ord a => E a -> E a -> E Bool
(>=.) = binop ">="

infix 4 ==.
(==.) :: Ord a => E a -> E a -> E Bool
(==.) = binop "=="

infix 4 /=.
(/=.) :: Ord a => E a -> E a -> E Bool
(/=.) = binop "/="

loop :: E Word -> (E Word -> M ()) -> M ()
loop = loopNM 0

each :: E (Ref (Array a)) -> (E (Ref a) -> M ()) -> M ()
each = eachN 0

eachN :: E Word -> E (Ref (Array a)) -> (E (Ref a) -> M ()) -> M ()
eachN i arr f = loopNM i (arr##count) $ f . ix arr

loopNM :: E Word -> E Word -> (E Word -> M ()) -> M ()
loopNM x y f = do
  i <- mk x
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
  
data Ref a
data Array a

type M a = State St a

data St = St{ unique :: Word, stmts :: [[Stmt]] }

initSt = St 0 [[]]

execM :: M () -> Block
execM x = let [b] = stmts $ execState x initSt in mkBlock b
  
infixl 9 #
(#) :: E (Ref a) -> E (Ref a -> Ref b) -> E (Ref b)
(#) = flip App
  
x :: E (Ref Body -> Ref Double)
x = FV ".x"
y :: E (Ref Body -> Ref Double)
y = FV ".y"
z :: E (Ref Body -> Ref Double)
z = FV ".z"
vx :: E (Ref Body -> Ref Double)
vx = FV ".vx"
vy :: E (Ref Body -> Ref Double)
vy = FV ".vy"
vz :: E (Ref Body -> Ref Double)
vz = FV ".vz"
mass :: E (Ref Body -> Ref Double)
mass = FV ".mass"

count :: E (Ref (Array a) -> Ref Word)
count = FV ".count"

-- void advance(int nbodies, struct planet * bodies, double dt)
-- {
--   int i, j;
advance :: E (Ref (Array Body)) -> E Double -> M ()
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

  each bodies $ \b -> do
    eachN 1 bodies $ \b2 -> do
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
  each bodies $ \b -> do
    let f p q = b#p += (dt * (b##q))
    f x vx
    f y vy
    f z vz

unop :: String -> E a -> E b
unop v = App (FV v)

binop :: String -> E a -> E b -> E c
binop v a b = App (unop v a) b

instance (Show a, Num a, CNum a) => Num (E a) where
  (+) = binop "+"
  (*) = binop "*"
  abs = unop "abs"
  signum = unop "signum"
  fromInteger = I
  (-) = binop "-"
  
instance (Show a, Fractional a, CNum a, CFractional a) => Fractional (E a) where
  fromRational = R
  (/) = binop "/"
  
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

energy bodies = do
  e <- mk 0
  each bodies $ \b -> do
    e += 0.5 * b##mass * ((b##vx)^2 + (b##vy)^2 + (b##vz)^2)
    eachN 1 bodies $ \b2 -> do
      let f p = b##p - b2##p
          dx = f x
          dy = f y
          dz = f z
          distance = sqrt $ dx^2 + dy^2 + dz^2
      e -= (b##mass * b2##mass) / distance
  return e
  
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

offset_momentum bodies = do
  px <- mk 0
  py <- mk 0
  pz <- mk 0
  each bodies $ \b -> do
    let f p q = p += b##vx * b##mass
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

initBody :: E (Ref Body) -> (E Double, E Double, E Double, E Double, E Double, E Double, E Double) -> M ()
initBody body (a, b, c, d, e, f, g) = do
  body#x .= a
  body#y .= b
  body#z .= c
  body#vx .= d * days_per_year
  body#vy .= e * days_per_year
  body#vz .= f * days_per_year
  body#mass .= g * solar_mass

indices :: [E Word]
indices = map fromIntegral [0 :: Word .. ]

mkArray :: (E (Ref a) -> b -> M ()) -> [b] -> M (E (Ref (Array a)))
mkArray f xs = do
  arr <- alloc $ fromIntegral $ length xs
  sequence_ [ f (ix arr i) x | (x, i) <- zip xs indices ]
  return arr
  
mkBodies = mkArray initBody
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

main_ n = execM $ do
  bodies <- mkBodies
  offset_momentum bodies
  let f = energy bodies >>= printf
  f
  loop n $ \_ -> advance bodies 0.01
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
