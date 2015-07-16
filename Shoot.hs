{-# LANGUAGE FlexibleInstances #-}
module Shoot where

import qualified Untyped as U
import Untyped (unused, Op(..), Type(..), Typed(..), Tree(..), Exp(..))
import Prelude
import Data.Word

data E a = E{ unE :: U.Exp }

instance EType Word where etypeof _ = TUInt 32
instance EType Int where etypeof _ = TSInt 32
instance EType Double where etypeof _ = TDouble
instance EType Bool where etypeof _ = TUInt 1

class EType a where  etypeof :: E a -> Type

instance EType a => Typed (E a) where typeof = etypeof

var :: EType a => Integer -> E a
var x = let v = E $ U.var (typeof v) x in v

compile (E x) = U.compile x

eq :: (EType a, Ord a) => E a -> E a -> E Bool
eq = binop Eq
ne :: (EType a, Ord a) => E a -> E a -> E Bool
ne = binop Ne
gt :: (EType a, Ord a) => E a -> E a -> E Bool
gt = binop Gt
lt :: (EType a, Ord a) => E a -> E a -> E Bool
lt = binop Lt
gte :: (EType a, Ord a) => E a -> E a -> E Bool
gte = binop Gte
lte :: (EType a, Ord a) => E a -> E a -> E Bool
lte = binop Lte

switch :: (EType a, EType b) => E a -> [E b] -> E b -> E b
switch a bs c = E $ U.switch (unE a) (map unE bs) (unE c)

class Aggregate a where
  agg :: Tree U.Exp -> a
  unAgg :: a -> Tree U.Exp

instance Aggregate (E a) where
  agg (Leaf x) = E x
  unAgg (E x) = Leaf x

instance (Aggregate a, Aggregate b) => Aggregate (a, b) where
  agg (Node [a,b]) = (agg a, agg b)
  unAgg (a,b) = Node [unAgg a, unAgg b]

instance (Aggregate a, Aggregate b, Aggregate c) => Aggregate (a, b, c) where
  agg (Node [a,b,c]) = (agg a, agg b, agg c)
  unAgg (a,b,c) = Node [unAgg a, unAgg b, unAgg c]

instance (Aggregate a) => Aggregate [a] where
  agg (Node xs) = map agg xs
  unAgg xs = Node $ map unAgg xs

while :: (Aggregate a) => a -> (a -> (E Bool, a)) -> a
while x f = agg $ U.while (unAgg x) g
  where g = \bs -> let (a, b) = f (agg bs) in (unE a, unAgg b)

ife :: (EType a) => E Bool -> E a -> E a -> E a
ife x y z = switch x [z] y

instance (EType a, Num a) => Num (E a) where
  fromInteger x = let v = E $ U.EAExp $ U.Int (typeof v) x in v
  (*) = binop Mul
  (+) = binop Add
  (-) = binop Sub
  abs = unop Abs
  signum = unop Signum
  
-- instance (EType a, Integral a) => Integral (E a) where
--   quotRem x y = (binop Quot x y, binop Rem x y) -- BAL: do these match llvm div, rem?
--   toInteger = error "toInteger"

-- instance (EType a, Real a) => Real (E a) where toRational = error "toRational"
-- instance (EType a, Ord a) => Ord (E a) where compare = error "compare"
-- instance (EType a, Eq a) => Eq (E a) where (==) = error "(==)"
instance (EType a, Enum a, Num a) => Enum (E a) where
  toEnum = fromInteger . fromIntegral
  fromEnum = error "fromEnum"

div' :: Integral a => E a -> E a -> E a
div' = binop Quot
mod' :: Integral a => E a -> E a -> E a
mod' = binop Rem

fastpow :: (EType a, EType b, Num a, Integral b, Ord b, Real b) => E a -> E b -> E a
fastpow b e =
  snd $ while ((b, e), 1) $ \((b, e), r) ->
    (e `gt` 0, ((b * b, e `div'` 2), ife ((e `mod'` 2) `ne` 0) (r * b) r))

dbl x = x + x

binop :: Op -> E a -> E b -> E c
binop o (E x) (E y) = E $ U.binop o x y
  
unop :: Op -> E a -> E b
unop o (E x) = E $ U.unop o x

instance (EType a, Fractional a) => Fractional (E a) where
  fromRational x = let v = E $ U.EAExp $ U.Rat (typeof v) x in v
  (/) = binop Quot

instance (EType a, Floating a) => Floating (E a) where
  pi = 3.141592653589793 -- BAL: use prelude value of pi?
  sqrt = unop Sqrt
  exp = unop Exp
  log = unop Log
  sin = unop Sin
  cos = unop Cos
  asin = unop Asin
  atan = unop Atan
  acos = unop Acos
  sinh = unop Sinh
  cosh = unop Cosh
  asinh = unop Asinh
  atanh = unop Atanh
  acosh = unop Acosh

-- days_per_year = 365.24

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

-- type Body = [Exp]

-- updFld fld f xs = bs ++ f c : cs
--   where (bs, c:cs) = splitAt fld xs

-- _px = 0
-- _py = 1
-- _pz = 2
-- _vx = 3
-- _vy = 4
-- _vz = 5
-- _mass = 6

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

-- advance :: Exp -> [Body] -> [Body]
-- advance dt = unfoldr (\bs -> if null bs then Nothing else Just $ loop [] (head bs) (tail bs))
--   where
--     loop rs a [] = (a, reverse rs)
--     loop rs a (b:bs) = let (a', r) = adv a b in loop (r:rs) a' bs
--     adv b b2 =
--       ( vx_ (g dx b2 -) $ vy_ (g dy b2 -) $ vz_ (g dz b2 -) b
--       , vx_ (g dx b +) $ vy_ (g dy b +) $ vz_ (g dz b +) b2
--       )
--       where
--         mag = dt / distance^3
--         distance = sqrt (dx^2 + dy^2 + dz^2)
--         dx = f px
--         dy = f py
--         dz = f pz
--         f g = g b - g b2
--         g a b = a * mass b * mag

-- energy :: [Body] -> Exp
-- energy = sum . map enrgy . init . tails
--   where
--     enrgy (b:bs) = 0.5 * mass b * (vx b)^2 * (vy b)^2 * (vz b)^2 - sum (map f bs)
--       where
--         f b2 = (mass b * mass b2) / (sqrt ((g px)^2 + (g py)^2 + (g pz)^2))
--           where g h = h b - h b2

-- offset_momentum :: [Body] -> [Body]
-- offset_momentum bs@([a,b,c,_,_,_,d]:_) = [a, b, c, f vx, f vy, f vz, d] : tail bs
--   where
--     f g = -((sum $ map (\b -> g b * mass b) bs) / solar_mass)
    
-- unbodies :: [Body] -> [Exp]
-- unbodies = concat

-- mkbodies :: [Exp] -> [Body]
-- mkbodies = unfoldr (\bs -> if null bs then Nothing else Just $ splitAt 7 bs)

-- nbody n =
--   while (0 : unbodies (offset_momentum bodies)) $ \(x:xs) ->
--     ( x `lt` n
--     , x + 1 : unbodies (advance 0.01 $ mkbodies xs)
--     , energy $ mkbodies xs
--     )

solar_mass = 4 * pi^2
