{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module Typed
  ( module Typed,
    PP(..),
    ESt(..),
    compile
  )
where

import qualified Untyped as U
import Untyped (unused, Op(..), UOp(..), Type(..), Typed(..), Tree(..), Exp(..), Const(..), AExp(..), PP(..), ESt(..), Expr(..), Def, compile)
import qualified Prelude as P
-- import Data.Word
-- import Data.List
import Prelude (Bool, Double, Int, Word, Float, Integer, Rational, fst, snd, (.), map, ($), id, IO, undefined, (<$>), (<*>), (>>=), fromIntegral, return, String, (++), Either(..))
import Control.Monad.State hiding (mapM, sequence)

instance PP (E a) where pp = pp . unE
  
fastpow :: (Typed a, Arith a) => E a -> E a -> E a
fastpow b e =
  snd $ while ((b, e), 1) $ \((b, e), r) ->
    (e > 0, ((b * b, e / 2), if' ((e % 2) /= 0) (r * b) r))

class Boolean bool where
--  ifThenElse :: bool -> a -> a -> a
  
class (Typed a, Boolean b) => Cmp a b | a -> b where
  (==) :: a -> a -> b
  (/=) :: a -> a -> b
  (>) :: a -> a -> b
  (<) :: a -> a -> b
  (>=) :: a -> a -> b
  (<=) :: a -> a -> b

instance Boolean (E Bool) where
  -- ifThenElse 

if' a b c = agg $ U.switch (unE a) [unAgg c] (unAgg b)

instance Boolean Bool where
--   ifThenElse = P.ifThenElse

instance Typed a => Cmp (E a) (E Bool) where
  (==) = binop Eq
  (/=) = binop Ne
  (>) = binop Gt
  (<) = binop Lt
  (>=) = binop Gte
  (<=) = binop Lte
  
class Arith a where
  (*) :: a -> a -> a
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (/) :: a -> a -> a
  (%) :: a -> a -> a
  fromInteger :: Integer -> a
  abs :: a -> a

instance Floating Double
instance Floating Float
instance Arith Double
instance Arith Float
instance Arith Word
instance Arith Integer where
  fromInteger = id
  
instance Arith Int where
  fromInteger = P.fromInteger
  (+) = (P.+)
  
class Typed a => Floating a where
  fromRational :: Rational -> a
  pi :: a
  pi = 3.141592653589793 -- BAL: use prelude value of pi?
  sqrt :: a -> a
  exp :: a -> a
  log :: a -> a
  sin :: a -> a
  cos :: a -> a
  asin :: a -> a
  atan :: a -> a
  acos :: a -> a
  sinh :: a -> a
  cosh :: a -> a
  asinh :: a -> a
  atanh :: a -> a
  acosh :: a -> a

instance Floating a => Floating (E a) where
  fromRational x = let v = E $ U.rat (typeof v) x in v
  acosh = unop Acosh
  atanh = unop Atanh
  asinh = unop Asinh
  sqrt = unop Sqrt
  exp = unop ExpF
  log = unop Log
  sin = unop Sin
  cos = unop Cos
  asin = unop Asin
  atan = unop Atan
  acos = unop Acos
  sinh = unop Sinh
  cosh = unop Cosh

instance (Typed a, Arith a) => Arith (E a) where
  (*) = binop Mul
  (+) = binop Add
  (-) = binop Sub
  (/) = binop Div
  (%) = binop Rem
  abs = unop Abs
  fromInteger x = let v = E $ U.rat (typeof v) (P.toRational x) in v

instance Cmp Float Bool
instance Cmp Double Bool
instance Cmp Int Bool
instance Cmp Word Bool
instance (Count c, Cmp a Bool) => Cmp (V c a) Bool

instance Typed a => Typed (E a) where
  typeof (_ :: E a) = typeof (unused "Typed (E a)" :: a)
  
instance (Count c, Typed a) => Typed (V c a) where
  typeof (_ :: V c a) =
    TVector (countof (unused "Typed (V c a):c" :: c))
            (typeof (unused "Typed (V c a):a" :: a))

data E a = E{ unE :: Exp }
  
data V c a

-- class Typed a where etypeof :: a -> Type

-- instance Typed a => Typed (E a) where typeof (_ :: E a) = etypeof (unused "etypeof:E a" :: a)

class Count c where countof :: c -> Integer

count :: Count c => c -> E Word
count = fromInteger . countof

-- class Counted a where countof :: a -> E Word

-- instance (Count c) => Counted (E (V c a)) where
--   countof (_ :: E (V c a)) = fromIntegral $ ecountof (unused "countof:E (V c a)" :: c)

-- assert s b a = if b then a else error $ "assert:" ++ s

-- undef :: (Typed a) => E a
-- undef = let a = E $ U.EAExp $ U.Undef $ typeof a in a

-- vec :: (Count c, Typed a) => [E a] -> E (V c a)
-- vec (xs :: [E a]) = f (unused "vec")
--   where
--     f :: (Count c, Typed a) => c -> E (V c a)
--     f c = assert "vec:length mismatch" (not (null bs) && length bs == cnt) $
--           foldl' ins undef $ zip bs [0 .. ]
--       where
--       cnt = fromIntegral $ ecountof c
--       bs = take cnt xs

-- ex :: (Count c, Typed a) => E (V c a) -> E Word -> E a
-- ex = binop U.ExtractElement

-- ins :: (Count c, Typed a) => E (V c a) -> (E a, E Word) -> E (V c a)
-- ins x (y, z) = ternop U.InsertElement x y z

-- vupd :: (Count c, Typed a) => E (V c a) -> (E a -> E a, E Word) -> E (V c a)
-- vupd x (f, z) = vupdi x (\_ -> f, z)

-- vupdi :: (Count c, Typed a) => E (V c a) ->
--   (E Word -> E a -> E a, E Word) -> E (V c a)
-- vupdi x (f, z) = ins x (f z $ ex x z, z)

-- vmap :: (Count c, Typed a) => (E a -> E a) -> E (V c a) -> E (V c a)
-- vmap f = vmapi $ \_ -> f

-- vmapi :: (Count c, Typed a) => (E Word -> E a -> E a) -> E (V c a) -> E (V c a)
-- vmapi f xs = snd $ while (0, xs) $ \(i, xs) ->
--   ( i `lt` countof xs
--   , (i + 1, vupdi xs (f, i))
--   )

-- vfold :: (Count c, Typed a, Agg b) => (b -> E a -> b) -> b -> E (V c a) -> b
-- vfold f = vfoldi $ \_ -> f

-- vrepeat :: (Count c, Typed a) => E a -> E (V c a)
-- vrepeat = vunfoldi_ . const

-- vunfoldi_ :: (Count c, Typed a) => (E Word -> E a) -> E (V c a)
-- vunfoldi_ f = vunfoldi (\i _ -> (f i, b)) b
--   where b :: E Word = undef -- BAL: b = undefined this should work, but doesn't.  being too strict somewhere unused "vunfoldi_"

-- vunfoldi :: (Count c, Agg b, Typed a) =>
--   (E Word -> b -> (E a, b)) -> b -> E (V c a)
-- vunfoldi f b = snd $ while ((0, b), undef) $ \((i, b), v) ->
--   ( i `lt` countof v
--   , let (a,b') = f i b in ((i + 1, b'), ins v (a, i))
--   )

-- vfoldi :: (Count c, Typed a, Agg b) =>
--   (E Word -> b -> E a -> b) -> b -> E (V c a) -> b
-- vfoldi f x ys = snd $ while (0, x) $ \(i, x) ->
--   ( i `lt` countof ys
--   , (i + 1, f i x $ ex ys i)
--   )

uerr s = P.error $ "user error:" ++ s

func :: (Agg a, Typed b) => String -> (a -> E b) -> (a -> E b)
func s f = \a -> E $ U.func s (unE $ f a) $ unAgg a

def :: (Agg a, Typed b) => (a -> E b) -> Def
def f = case unExp $ unE $ f instantiate of
  App (Right a) _ -> a
  _ -> uerr "unable to create definition (not a procedure)"

instantiate :: Agg a => a
instantiate = let v = agg $ U.instantiate $ typeofAgg v in v

-- false :: E Bool
-- false = E $ U.EAExp $ U.Int U.tbool 0

-- true :: E Bool
-- true = E $ U.EAExp $ U.Int U.tbool 1

-- eq :: (Typed a, Ord a) => E a -> E a -> E Bool
-- eq = binop Eq
-- band :: (Typed a, Integral a) => E a -> E a -> E a
-- band = binop And
-- bor :: (Typed a, Integral a) => E a -> E a -> E a
-- bor = binop Or
-- xor :: (Typed a, Integral a) => E a -> E a -> E a
-- xor = binop Xor
-- lshr :: (Typed a, Integral a) => E a -> E a -> E a
-- lshr = binop Lshr
-- ashr :: (Typed a, Integral a) => E a -> E a -> E a
-- ashr = binop Ashr
-- shl :: (Typed a, Integral a) => E a -> E a -> E a
-- shl = binop Shl

-- ne :: (Typed a, Ord a) => E a -> E a -> E Bool
-- ne = binop Ne
-- gt :: (Typed a, Ord a) => E a -> E a -> E Bool
-- gt = binop Gt
-- lt :: (Typed a, Ord a) => E a -> E a -> E Bool
-- lt = binop Lt
-- gte :: (Typed a, Ord a) => E a -> E a -> E Bool
-- gte = binop Gte
-- lte :: (Typed a, Ord a) => E a -> E a -> E Bool
-- lte = binop Lte

-- type Unique a = State Integer a

-- newUnique :: Unique Integer
-- newUnique = undefined

class Agg a where
  agg :: Tree Exp -> a
  unAgg :: a -> Tree Exp
  typeofAgg :: a -> Tree Type

instance Typed a => Agg (E a) where
  agg (Leaf x) = E x
  unAgg (E x) = Leaf x
  typeofAgg = Leaf . typeof

instance Agg () where
  agg (Node []) = ()
  unAgg () = Node []
  typeofAgg _ = Node []
  
instance (Agg a, Agg b) => Agg (a, b) where
  agg (Node [a,b]) = (agg a, agg b)
  unAgg (a,b) = Node [unAgg a, unAgg b]
  typeofAgg (_ :: (a,b)) = Node [ typeofAgg (unused "Agg (a,b)" :: a), typeofAgg (unused "Agg (a,b)" :: b) ]
    
instance (Agg a, Agg b, Agg c) => Agg (a, b, c) where
  agg (Node [a,b,c]) = (agg a, agg b, agg c)
  unAgg (a,b,c) = Node [unAgg a, unAgg b, unAgg c]
  typeofAgg (_ :: (a,b,c)) = Node [ typeofAgg (unused "Agg (a,b,c)" :: a), typeofAgg (unused "Agg (a,b,c)" :: b), typeofAgg (unused "Agg (a,b,c)" :: c) ]

-- instance (Agg a) => Agg [a] where
--   agg (Node xs) = map agg xs
--   unAgg xs = Node $ map unAgg xs
--   newAgg = replicate(,,) <$> newAgg <*> newAgg <*> newAgg

-- switch :: (Typed a, Agg b) => E a -> [b] -> b -> b
-- switch a bs c = agg $ U.switch (unE a) (map unAgg bs) (unAgg c)

-- emax x y = eif (x `gte` y) x y

-- reps :: (Agg b) => E Word -> b -> (b -> b) -> b
-- reps n b f = snd $ while (n,b) $ \(n,b) ->
--   (n `gt` 0
--   , (n - 1, f b)
--   )

-- eif :: (Typed a) => E Bool -> E a -> E a -> E a
-- eif x y z = switch x [z] y

app :: Typed a => UOp -> [Exp] -> E a
app o xs = let v = E $ U.app o (typeof v) xs in v
  
ternop :: (Typed a, Typed b, Typed c, Typed d) => UOp -> E a -> E b -> E c -> E d
ternop o x y z = app o [unE x, unE y, unE z]

binop :: (Typed a, Typed b, Typed c) => UOp -> E a -> E b -> E c
binop o x y = app o [unE x, unE y]

unop :: (Typed a, Typed b) => UOp -> E a -> E b
unop o x = app o [unE x]

cast :: (Typed a, Typed b) => E a -> E b
cast = unop Cast

-- instance (Num a, Typed a) => Num (E a) where

-- instance (Typed a, Integral a) => Integral (E a) where
--   quotRem x y = (binop Quot x y, binop Rem x y) -- BAL: do these match llvm div, rem?
--   toInteger = unused "toInteger"

-- instance (Typed a, Real a) => Real (E a) where toRational = unused "toRational"
-- instance (Typed a, Ord a) => Ord (E a) where compare = unused "compare"
-- instance (Typed a, Eq a) => Eq (E a) where (==) = unused "(==)"

while :: Agg a => a -> (a -> (E Bool, a)) -> a
while x f = agg $ U.while (unAgg x) g
  where g = \bs -> let (a, b) = f (agg bs) in (unE a, unAgg b)

runEval = U.runEval . unE

-- if' :: Agg a => E Bool -> a -> a -> a
-- if' 

switch :: Agg a => E Word -> [a] -> a -> a
switch a bs c = agg $ U.switch (unE a) (map unAgg bs) (unAgg c)

-- instance (Typed a, Enum a, Num a) => Enum (E a) where
--   toEnum = fromInteger . fromIntegral
--   fromEnum x = case unE x of
--     EAExp (CAExp (Int _ i)) -> fromInteger i
--     _ -> unused "Enum (E a):fromEnum"

-- fastpow :: (Typed a, Typed b, Num a, Integral b, Ord b, Real b) => E a -> E b -> E a
-- fastpow b e =
--   snd $ while ((b, e), 1) $ \((b, e), r) ->
--     (e `gt` 0, ((b * b, e `div'` 2), eif ((e `mod'` 2) `ne` 0) (r * b) r))

-- class Foo a where
--   (%) :: a -> a -> a
--   (/
-- instance (Typed a, Fractional a) => Fractional (E a) where
--   (/) = binop Quot

dbl x = x + x

-- instance (Typed a, Floating a) => Floating (E a) where

-- -- N-Body

-- days_per_year = 365.24
-- solar_mass = 4 * pi^2

-- type Body = [E Double]

-- bodies :: [Body]
-- bodies = map (\[a,b,c,d,e,f,g] -> [a,b,c,d*days_per_year,e*days_per_year,f*days_per_year,g*solar_mass])
--   [
--     [                               -- sun */
--       0, 0, 0, 0, 0, 0, 1
--     ],
--     [                               -- jupiter
--       4.84143144246472090e+00,
--       -1.16032004402742839e+00,
--       -1.03622044471123109e-01,
--       1.66007664274403694e-03,
--       7.69901118419740425e-03,
--       -6.90460016972063023e-05,
--       9.54791938424326609e-04
--     ],
--     [                               -- saturn
--       8.34336671824457987e+00,
--       4.12479856412430479e+00,
--       -4.03523417114321381e-01,
--       -2.76742510726862411e-03,
--       4.99852801234917238e-03,
--       2.30417297573763929e-05,
--       2.85885980666130812e-04
--     ],
--     [                               -- uranus
--       1.28943695621391310e+01,
--       -1.51111514016986312e+01,
--       -2.23307578892655734e-01,
--       2.96460137564761618e-03,
--       2.37847173959480950e-03,
--       -2.96589568540237556e-05,
--       4.36624404335156298e-05
--     ],
--     [                               -- neptune
--       1.53796971148509165e+01,
--       -2.59193146099879641e+01,
--       1.79258772950371181e-01,
--       2.68067772490389322e-03,
--       1.62824170038242295e-03,
--       -9.51592254519715870e-05,
--       5.15138902046611451e-05
--     ]
--   ]

-- updFld fld f xs = bs ++ f c : cs
--   where (bs, c:cs) = splitAt fld xs

-- _px = 0
-- _py = 1
-- _pz = 2
-- _vx = 3
-- _vy = 4
-- _vz = 5
-- _mass = 6

-- px_ = updFld _px
-- py_ = updFld _py
-- pz_ = updFld _pz
-- vx_ = updFld _vx
-- vy_ = updFld _vy
-- vz_ = updFld _vz

-- getFld = flip (!!)
-- mass = getFld _mass
-- vx = getFld _vx
-- vy = getFld _vy
-- vz = getFld _vz
-- px = getFld _px
-- py = getFld _py
-- pz = getFld _pz

-- advance :: E Double -> [Body] -> [Body]
-- advance dt bs0 = map h $ adv' [] bs0
--   where
--     h b = px_ (f vx) $ py_ (f vy) $ pz_ (f vz) b
--       where
--         f v = (+) (dt * v b)
--     adv' xs [] = reverse xs
--     adv' xs (b:bs) = let (b':bs') = adv [] b bs in adv' (b':xs) bs'
--     adv xs b [] = b : reverse xs
--     adv xs b (b2:bs) = adv (b2' : xs) b' bs
--       where
--       f d v = v - d * mass b2 * mag
--       g d v = v + d * mass b * mag
--       b' = vx_ (f dx) $ vy_ (f dy) $ vz_ (f dz) b
--       b2' = vx_ (g dx) $ vy_ (g dy) $ vz_ (g dz) b2
--       dx = px b - px b2
--       dy = py b - py b2
--       dz = pz b - pz b2
--       distance = sqrt(dx^2 + dy^2 + dz^2)
--       mag = dt / (distance^3)

-- energy :: [Body] -> E Double
-- energy = sum . map enrgy . init . tails
--   where
--     enrgy (b:bs) = 0.5 * mass b * ((vx b)^2 + (vy b)^2 + (vz b)^2) - sum (map f bs)
--       where
--         f b2 = (mass b * mass b2) / (sqrt ((g px)^2 + (g py)^2 + (g pz)^2))
--           where g h = h b - h b2

-- offset_momentum :: [Body] -> [Body]
-- offset_momentum bs@([a,b,c,_,_,_,d]:_) = [a, b, c, f vx, f vy, f vz, d] : tail bs
--   where
--     f g = -((sum $ map (\b -> g b * mass b) bs) / solar_mass)
    
-- nbody :: E Int -> E Double
-- nbody n = energy $ snd $
--   while (0, offset_momentum bodies) $ \(i, xs) ->
--     ( i `lt` n
--     , (i + 1, advance 0.01 xs)
--     )

-- --------------------------
-- type V16W4 = Word64

-- type Perm = (E V16W4, (E V16W4, E Word64))

-- nelems = 16
-- shl4 x v = shl v (4*x)
-- lshr4 x v = lshr v (4*x)

-- maskMerge x y mask = x `xor` ((x `xor` y) `band` mask) -- from graphics.stanford.edu bithacks.html

-- ix v i = lshr4 i v `band` 0xf

-- setix v i x = maskMerge v (shl4 i x) (shl4 i 0xf)

-- updix v i f = setix v i $ f $ ix v i

-- maxV16W4 :: E V16W4
-- maxV16W4 = 0xffffffffffffffff

-- factorial :: (Typed a, Integral a) => E a -> E a
-- factorial n0 = snd $ while (n0,1) $ \(n,r) -> (n `gt` 1, (n - 1, r * n))

-- fkRotate :: E V16W4 -> E Word64 -> E V16W4
-- fkRotate v n = maskMerge (maskMerge v1 v2 (shl4 (n - 1) 0xf)) v mask
--   where
--     v1 = lshr4 1 v
--     v2 = shl4 (n - 1) v
--     mask = shl4 n maxV16W4

-- fkFlip :: (Typed a, Num a) => E V16W4 -> E a
-- fkFlip v0 = snd $ while (v0, 0) $ \(v,n) ->
--   ( ix v 0 `gt` 1
--   , (fkReverse v, n + 1)
--   )

-- fkReverse :: E V16W4 -> E V16W4
-- fkReverse v0 = maskMerge (rev v r) v0 (shl4 n0 maxV16W4)
--   where
--     n0 = v0 `ix` 0
--     rev v r = r `bor` (v `band` 0xf)
--     (_, (v,r)) = while (n0, (v0, 0)) $ \(n, (v, r)) ->
--       ( n `gt` 1
--       , (n - 1, (lshr4 1 v, shl4 1 $ rev v r))
--       )

-- fkPerm :: Perm -> Perm
-- fkPerm pci = (fkRotate p i, (updix c i (+ 1), 2))
--   where
--   (p, (c, i)) = while pci $ \(p,(c,i)) ->
--     ( ix c i `gte` i
--     , (fkRotate p i, (setix c i 1, i + 1))
--     )

-- fkMain :: (Typed a, Integral a) => E a -> (E a, E a)
-- fkMain n =
--   fst $ while ((0,0), (factorial n, perm0)) $ \((max_flips, checksum),(n,pci@(p,_))) ->
--     ( n `gt` 0
--     , let flips_count = fkFlip p in
--         ((emax max_flips flips_count, -1 * (checksum + flips_count)), (n - 1, fkPerm pci))
--     )
--   where
--     perm0 = (0xfedcba987654321, (0x1111111111111111, 2))


-- -- spectral-norm
-- type EvalF = E Word -> E Word -> E Double

-- -- evalA :: E Word -> E Word -> E Double
-- -- evalA i0 j0 = 1.0/((i+j)*(i+j+1)/2+i+1) where (i,j) = (tofp i0, tofp j0)

-- -- evalATimesUF :: Count c => (EvalF -> EvalF) -> E (V c Double) -> E (V c Double)
-- -- evalATimesUF f u = vunfoldi_ $ \i -> (vfoldi (\j b a -> b + (f evalA) j i * a) 0 u)

-- -- evalATimesU :: Count c => E (V c Double) -> E (V c Double)
-- -- evalATimesU = evalATimesUF id

-- -- evalAtTimesU :: Count c => E (V c Double) -> E (V c Double)
-- -- evalAtTimesU = evalATimesUF flip

-- -- evalAtATimesU :: Count c => E (V c Double) -> E (V c Double)
-- -- evalAtATimesU = evalAtTimesU . evalATimesU

-- -- spctMain :: Count c => c -> E Double
-- -- spctMain (c :: c) = sqrt(vBv/vv)
-- --   where
-- --     (u, v) = reps 10 (vrepeat 1 :: E (V c Double), undef) $ \(u,_) ->
-- --              let v = evalAtATimesU u in (v, evalAtATimesU v)
-- --     (vBv,vv) = vfoldi  (\i (vBv, vv) vi -> (u `ex` i * vi, vi^2)) (0,0) v

-- -- BAL: tickles bug
-- spctMain :: Count c => c -> E Double
-- spctMain (c :: c) = vfoldi  (\i _ vi -> u `ex` i * vi) 0 v
--   where
--     (u, v) = (vrepeat 3 :: E (V c Double), vrepeat 2 :: E (V c Double))
