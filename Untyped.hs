{-# OPTIONS -fno-warn-missing-signatures #-}
{-# OPTIONS -fno-warn-name-shadowing #-}
{-# OPTIONS -fno-warn-type-defaults #-}
{-# OPTIONS -fno-warn-unused-imports #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module T where

import           Control.Applicative hiding (empty)
import           Control.Exception
import           Control.Monad.State
-- import           Data.Graph
import qualified Data.HashMap.Strict as M
import           Data.Hashable
import           Data.List hiding (insert, lookup)
import           Data.Maybe
import           GHC.Generics (Generic)
import           Prelude hiding (lookup)
import qualified Text.PrettyPrint as PP
import           Text.PrettyPrint hiding (int, empty)
import Data.Array

class PP a where pp :: a -> Doc

instance PP a => PP [a] where pp = parens . hsep . map pp
  
instance (PP a, PP b) => PP (a,b) where pp (a,b) = parens (pp a <+> pp b)

newtype Free = Free Integer deriving (Show, Eq, Num, Ord, Generic, Enum, Ix)
instance Hashable Free
instance PP Free where pp (Free a) = text "F" <> integer a

newtype User = User Integer deriving (Show, Eq, Num, Ord, Generic, Enum)
instance Hashable User
instance PP User where pp (User a) = text "U" <> integer a

newtype Bound = Bound Integer deriving (Show, Eq, Num, Ord, Generic, Enum)
instance Hashable Bound
instance PP Bound where pp (Bound a) = text "B" <> integer a
  
newtype Label = Label Integer deriving (Show, Eq, Num, Ord, Generic, Enum)
instance Hashable Label
instance PP Label where pp (Label a) = text "L" <> integer a

data Op
  = Add | Sub | Mul | Quot | Rem | Eq | Ne | Gt | Lt | Gte | Lte | Abs | Signum
  | Sqrt | Exp | Log | Sin | Cos | Asin | Atan | Acos | Sinh | Cosh | Asinh | Atanh | Acosh
  deriving (Show, Eq, Ord, Generic, Enum)
instance Hashable Op
instance PP Op where pp = text . show

instance PP Integer where pp = integer
instance PP Rational where pp = rational

type NumBV = Integer

data AExp
  = Int Integer
  | Rat Rational
  | FVar Free
  | BVar Bound
  | UVar User
  deriving (Show, Eq, Generic, Ord)
instance Hashable AExp

data Exp
  = EAExp AExp
  | EOp NumBV Op [Exp]
  | ESwitch NumBV Exp [Exp] Exp
  | EWhile NumBV Exp [Phi Exp] Exp
  deriving (Show, Eq)

type Phi a = (Bound, (a, a))

data CExp
  = CAExp AExp
  | COp Op [AExp]
  | CSwitch AExp [AExp] AExp
  | CWhile AExp [Phi AExp] AExp
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
lookupFVar x = flip (!) x <$> gets fvars

pushInsn :: (Free, (Op, [AExp])) -> N ()
pushInsn x =
  modify $ \st -> st{ blocks = let b:bs = blocks st in b{ insns = x : insns b } : bs }

pushTerm :: Terminator -> N ()
pushTerm x = modify $ \st -> st{ blocks = let b:bs = blocks st in b{ term = x, insns = reverse $ insns b } : bs }

pushLabel :: Label -> [(Either Free Bound, [(AExp, Label)])] -> N ()
pushLabel lbl ps = modify $ \st -> st{ blocks = Block lbl ps [] Exit : blocks st }

freshLabel :: N Label
freshLabel = do
  lbl <- gets nextLabel
  modify $ \st -> st{ nextLabel = succ lbl }
  return lbl

currentLabel :: N Label
currentLabel = label . head <$> gets blocks

compute :: AExp -> N AExp
compute x = case x of
  FVar n -> do
    let ok vx = do
          modify $ \st -> st{ fvars = fvars st // [(n, CAExp vx)] }
          return vx
    y <- lookupFVar n
    case y of
      CAExp a -> return a
      COp a bs -> do
        vbs <- mapM compute bs
        pushInsn (n, (a,vbs))
        ok x
      CSwitch a bs c -> do
        va <- compute a
        lbl : lbls <- mapM (const freshLabel) $ c : bs
        let ps = zip (c : bs) (lbl : lbls)
        end <- freshLabel
        pushTerm $ Switch va lbls lbl
        vps <- flip mapM ps $ \(b,l) -> do
          pushLabel l []
          vb <- compute b -- BAL: need a 'withMap' function
          pushTerm $ Jump end
          return (vb, l)
        pushLabel end [(Left n, vps)]
        ok x
      CWhile a bs c -> do
        vbs0 <- mapM compute $ map (fst . snd) bs
        pre <- currentLabel
        [begin, body, end] <- sequence $ replicate 3 freshLabel
        pushTerm $ Jump begin
        
        pushLabel body []
        vbs1 <- mapM compute $ map (snd . snd) bs
        pushTerm $ Jump begin
        
        pushLabel begin [ (Right r, [(p, pre),(q, body)])
                        | (r, p, q) <- zip3 (map fst bs) vbs0 vbs1 ]
        va <- compute a
        pushTerm $ Switch va [end] body

        pushLabel end []
        compute c
        ok c
        
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
  CSwitch a bs c -> ESwitch 0 (EAExp a) (map EAExp bs) (EAExp c)
  CWhile a bs c ->
    EWhile 0 (EAExp a) [ (v, (EAExp p, EAExp q)) | (v, (p, q)) <- bs ] (EAExp c)

instance PP CExp where pp = pp . fromCExp
  
aexp :: Exp -> M AExp
aexp x = cexp x >>= name

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

name :: CExp -> M AExp
name x = case x of
  CAExp a -> return a
  COp a bs | optimize -> case (a,bs) of
    _ | all isConst bs -> return $ case any isRat bs of
                           True -> Rat $ evalOpRat a $ map toRational bs
                           False -> Int $ evalOpInt a $ map toInteger bs
    (Mul, [Rat 1, p]) -> return p
    (Mul, [Int 1, p]) -> return p
    (Mul, [p, Rat 1]) -> return p
    (Mul, [p, Int 1]) -> return p
    (Mul, [Rat 0, _]) -> return $ Rat 0
    (Mul, [Int 0, _]) -> return $ Int 0
    (Mul, [_, Rat 0]) -> return $ Rat 0
    (Mul, [_, Int 0]) -> return $ Int 0
    (Add, [Rat 0, p]) -> return p
    (Add, [Int 0, p]) -> return p
    (Add, [p, Rat 0]) -> return p
    (Add, [p, Int 0]) -> return p
    (Sub, [p, Rat 0]) -> return p
    (Sub, [p, Int 0]) -> return p
    _ -> ok
  _ -> ok
  where
    ok = do
      tbl <- gets cexps
      let (a, tbl') = insertR x tbl
      modify $ \st -> st{ cexps = tbl' }
      return $ FVar a

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
  ESwitch _ b cs d -> CSwitch <$> aexp b <*> mapM aexp cs <*> aexp d
  EWhile _ a bs c -> CWhile <$> aexp a <*> mapM f bs <*> aexp c
    where f (v, (p, q)) = pair v <$> (pair <$> aexp p <*> aexp q)

instance PP Block where
  pp (Block a b c d) = vcat [pp a, nest 2 $ vcat $ map pp b ++ map pp c ++ [pp d]]

instance PP St2 where
  pp st = vcat $ map pp $ blocks st
  
ppSwitch a bs c = parens $ hsep $ text "switch" : pp a : map pp bs ++ [pp c]
  
instance PP Exp where
  pp x = case x of
    ESwitch _ a bs c -> ppSwitch a bs c
    EOp _ a bs -> parens (pp a <+> hsep (map pp bs))
    EAExp a -> pp a
    EWhile _ a bs c -> parens $ vcat [text "while", nest 2 $ vcat [pp a, pp bs, pp c]]

instance PP AExp where
  pp x = case x of
    FVar a -> pp a
    BVar a -> pp a
    UVar a -> pp a
    Int a -> pp a
    Rat a -> pp a
    
instance (PP a, PP b, PP c, PP d) => PP (a,b,c,d) where
  pp (a,b,c,d) = parens (vcat [pp a, pp b, pp c, pp d])

maximumBV :: [Exp] -> Integer
maximumBV = maximum . map maxBV

maxBV :: Exp -> Integer
maxBV x = case x of
  ESwitch i _ _ _ -> i
  EOp i _ _ -> i
  EAExp _ -> 0
  EWhile i _ _ _ -> i

binop :: Op -> Exp -> Exp -> Exp
binop o x y = EOp (maximumBV [x,y]) o [x,y]

unop :: Op -> Exp -> Exp
unop o x = EOp (maxBV x) o [x]

unused = error "unused"

instance Real Exp where toRational = unused
instance Num AExp where
  (+) = unused
  (*) = unused
  (-) = unused
  abs = unused
  signum = unused
  fromInteger = unused
  
instance Real AExp where
  toRational x = case x of
    Rat a -> a
    Int a -> toRational a
    _ -> unused

instance Integral AExp where
  toInteger x = case x of
    Int a -> a
    _ -> unused
  quotRem = unused
  
instance Enum AExp where
  toEnum = unused
  fromEnum = unused
  
instance Ord Exp where compare = unused
instance Enum Exp where
  toEnum = fromInteger . fromIntegral
  fromEnum = unused
  
instance Integral Exp where
  quotRem x y = (binop Quot x y, binop Rem x y)
  toInteger = unused
  
eq = binop Eq
ne = binop Ne
gt = binop Gt
lt = binop Lt
gte = binop Gte
lte = binop Lte

switch :: Exp -> [Exp] -> Exp -> Exp
switch x ys z = ESwitch (maximumBV $ x : z : ys) x ys z

var = EAExp . UVar . User

ife x y z = switch x [z] y

instance Fractional Exp where
  fromRational = EAExp . Rat
  (/) = div

instance Num Exp where
  fromInteger = EAExp . Int
  (*) = binop Mul
  (+) = binop Add
  (-) = binop Sub
  abs = unop Abs
  signum = unop Signum

instance (PP a, PP b) => PP (Either a b) where
  pp x = case x of
    Left a -> pp a
    Right b -> pp b
    
while :: [Exp] -> ([Exp] -> (Exp, [Exp], Exp)) -> Exp
while xs f =
  assert (length xs == length ys) $ EWhile (n + m) e (zip vs $ zip xs ys) r
  -- ^ BAL: identify unused loop variables (remove(warning?) or error)
  where
    m = genericLength xs
    (e, ys, r) = f $ map (EAExp . BVar) vs
    vs = map (Bound . (+) n) [0 .. m - 1]
    n = maximumBV (e : xs ++ ys)

fastpow b e =
  while [b, e, 1] $ \[b, e, r] ->
    (e `gt` 0, [ b * b, e `div` 2, ife ((e `mod` 2) `ne` 0) (r * b) r ], r)

days_per_year = 365.24

bodies :: [Body]
bodies = map (\[a,b,c,d,e,f,g] -> [a,b,c,d*days_per_year,e*days_per_year,f*days_per_year,g*solar_mass])
  [
    [                               -- sun */
      0, 0, 0, 0, 0, 0, 1
    ],
    [                               -- jupiter
      4.84143144246472090e+00,
      -1.16032004402742839e+00,
      -1.03622044471123109e-01,
      1.66007664274403694e-03,
      7.69901118419740425e-03,
      -6.90460016972063023e-05,
      9.54791938424326609e-04
    ],
    [                               -- saturn
      8.34336671824457987e+00,
      4.12479856412430479e+00,
      -4.03523417114321381e-01,
      -2.76742510726862411e-03,
      4.99852801234917238e-03,
      2.30417297573763929e-05,
      2.85885980666130812e-04
    ],
    [                               -- uranus
      1.28943695621391310e+01,
      -1.51111514016986312e+01,
      -2.23307578892655734e-01,
      2.96460137564761618e-03,
      2.37847173959480950e-03,
      -2.96589568540237556e-05,
      4.36624404335156298e-05
    ],
    [                               -- neptune
      1.53796971148509165e+01,
      -2.59193146099879641e+01,
      1.79258772950371181e-01,
      2.68067772490389322e-03,
      1.62824170038242295e-03,
      -9.51592254519715870e-05,
      5.15138902046611451e-05
    ]
  ]

type Body = [Exp]

updFld fld f xs = bs ++ f c : cs
  where (bs, c:cs) = splitAt fld xs

_px = 0
_py = 1
_pz = 2
_vx = 3
_vy = 4
_vz = 5
_mass = 6

vx_ = updFld _vx
vy_ = updFld _vy
vz_ = updFld _vz

getFld = flip (!!)
mass = getFld _mass
vx = getFld _vx
vy = getFld _vy
vz = getFld _vz
px = getFld _px
py = getFld _py
pz = getFld _pz

advance :: Exp -> [Body] -> [Body]
advance dt = unfoldr (\bs -> if null bs then Nothing else Just $ loop [] (head bs) (tail bs))
  where
    loop rs a [] = (a, reverse rs)
    loop rs a (b:bs) = let (a', r) = adv a b in loop (r:rs) a' bs
    adv b b2 =
      ( vx_ (g dx b2 -) $ vy_ (g dy b2 -) $ vz_ (g dz b2 -) b
      , vx_ (g dx b +) $ vy_ (g dy b +) $ vz_ (g dz b +) b2
      )
      where
        mag = dt / distance^3
        distance = sqrt (dx^2 + dy^2 + dz^2)
        dx = f px
        dy = f py
        dz = f pz
        f g = g b - g b2
        g a b = a * mass b * mag
        
instance Floating Exp where
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

energy :: [Body] -> Exp
energy = sum . map enrgy . init . tails
  where
    enrgy (b:bs) = 0.5 * mass b * (vx b)^2 * (vy b)^2 * (vz b)^2 - sum (map f bs)
      where
        f b2 = (mass b * mass b2) / (sqrt ((g px)^2 + (g py)^2 + (g pz)^2))
          where g h = h b - h b2

offset_momentum :: [Body] -> [Body]
offset_momentum bs@([a,b,c,_,_,_,d]:_) = [a, b, c, f vx, f vy, f vz, d] : tail bs
  where
    f g = -((sum $ map (\b -> g b * mass b) bs) / solar_mass)
    
unbodies :: [Body] -> [Exp]
unbodies = concat

mkbodies :: [Exp] -> [Body]
mkbodies = unfoldr (\bs -> if null bs then Nothing else Just $ splitAt 7 bs)

nbody n = while (0 : unbodies (offset_momentum bodies)) (\(x:xs) -> (x `lt` n, x + 1 : unbodies (advance 0.01 $ mkbodies xs), energy $ mkbodies xs))

dbl x = x + x

solar_mass = 4 * pi^2

-- data Body = Body{ x :: Double, y :: Double, z :: Double, vx :: Double, vy :: Double, vz :: Double, mass :: Double }

-- proj :: Int -> M [Expr] -> Expr
-- proj x m = m >>= (((!! x) <$>) . sequence)

-- var :: Integer -> Expr
-- -- var x = return (0, Reg $ Register $ negate $ succ x)
-- var x = return $ Reg $ Register $ negate $ succ x

-- instance PP St where
--   pp x = vcat (pp (nextUnique x) : map pp (whiles x))

-- type While = ([Register], [Exp], Exp, [Exp])

-- data Tree a = Tree a [Tree a] deriving Show

-- instance PP a => PP (Tree a) where
--   pp (Tree a []) = pp a
--   pp (Tree a bs) = parens (text "Tree" <+> pp a <+> hsep (map pp bs))

data MapR a b = MapR
  { hmapR :: M.HashMap b a
  , next :: a
  } deriving Show
  
data St = St
  { cexps :: MapR Free CExp
  } deriving Show

data St2 = St2
  { blocks :: [Block]
  , nextLabel :: Label
  , fvars :: Array Free CExp
  } deriving Show

data Terminator
  = Jump Label
  | Switch AExp [Label] Label
  | Exit
  deriving Show

instance PP Terminator where
  pp x = case x of
    Jump a -> text "jump" <+> pp a
    Switch a bs c -> ppSwitch a bs c
    Exit -> text "exit"

data Block = Block
  { label :: Label
  , phis :: [(Either Free Bound, [(AExp, Label)])]
  , insns :: [(Free, (Op, [AExp]))]
  , term :: Terminator
  } deriving Show

type N a = State St2 a

-- -- foo = M.toList . cexps
-- foo = undefined -- graphFromEdges [] -- . map f . M.toList . hmapR . cexps

-- -- toCFGNodes :: (Free, CExp) -> [(Free, Free)]
-- toCFGNodes (x,y) = case y of
--   CAExp{} -> unused
--   COp _ bs -> ins bs
--   CSwitch a bs c -> ins [a] ++ outs (c : bs)
--   CWhile a bs c -> ins (a : map (fst . snd) bs) ++ outs (c : map (snd . snd) bs)
--   where
--     ins = map (flip pair x) . catMaybes . map toFree
--     outs = map (pair x) . catMaybes . map toFree

-- toFree :: AExp -> Maybe Free
-- toFree x = case x of
--   FVar a -> Just a
--   _ -> Nothing
  
type M a = State St a

emptyR = MapR M.empty 0

runSt x = runState (aexp x) $ St emptyR

compile x = do
  let (a, st) = runSt x
  print $ pp (a, st)
  let (b, st2) = runSt2 $ cexps st
  print $ pp st2

runSt2 x = (a, st{ blocks = sortBy (\a b -> compare (label a) (label b)) $ blocks st })
  where
    (a, st) = runState m $ St2 [Block 0 [] [] Exit] 1 arr
    arr = array (0, n) $ map swap $ M.toList $ hmapR x
    m = do
      compute $ FVar n
      pushTerm Exit
    n = pred $ next x
    
instance PP St where pp x = pp $ cexps x
  
-- unique :: M Integer
-- unique = do
--   u <- gets nextUnique
--   modify $ \st -> st{ nextUnique = succ u }
--   return u

-- joinExprs :: ([Ex] -> Ex) -> [Expr] -> Expr
-- joinExprs f xs = f <$> sequence xs
--   -- (bs, cs) <- unzip <$> sequence xs
--   -- return (maximum bs, f cs)

-- while :: [Expr] -> ([Expr] -> (Expr, [Expr])) -> M [Expr]
-- while xs f = do
--   ws <- gets whiles
--   modify $ \st -> st{ whiles = [] }
--   bs <- sequence xs
--   vs <- mapM (\_ -> Register <$> unique) xs
--   let es = map (return . Reg) vs
--   let (m, ns) = f es
--   c <- m
--   ds <- sequence ns
--   ws' <- gets whiles
--   modify $ \st -> st{ whiles = Tree (vs, bs, c, ds) ws' : ws }
--   return es

-- fastpow b e =
--   proj 2 $ while [b, e, 1] $ \[b, e, r] -> (e `gt` 0, [ b * b, e `div` 2, ift ((e `mod` 2) `ne` 0) (r * b) r ])

-- loop :: M Exp -> M Exp -> M Exp -> M Exp
-- loop b e r = ift (e > 0) (loop (b*b) (e `div` 2) (ift ((e `mod` 2) /= 0) (r*b) r)) r
    
-- data AExp
--   = AInt Integer
--   | AReg Register
--   | AIft AExp [AExp] [AExp]
--   | AWhile [AExp] AExp [AExp]
--     deriving (Show, Eq, Ord, Generic)
             
-- instance Hashable AExp
-- instance PP AExp where
--   pp x = case x of
--     AInt a -> integer a
--     AReg a -> pp a
--     AIft a bs cs -> hsep [text "if", pp bs, pp cs]
--     AWhile bs c ds -> hsep [text "while", pp bs, pp c, pp ds]
      
{-
data Map a b = Map{ hmap :: M.HashMap a b, hmapR :: M.HashMap b a, next :: a }
  deriving Show



empty :: (Hashable b, Eq b, Num a) => Map a b
empty = Map M.empty M.empty 0


lookup :: (Hashable a, Eq a) => a -> Map a b -> Maybe b
lookup a = M.lookup a . hmap

instruction :: Insn -> M AExp
instruction x = do
  m <- gets insns
  let (a, m') = insert x m
  modify $ \st -> st{ insns = m' }
  return $ AReg a

terminator :: Terminator -> M Label
terminator b = do
  m <- gets blocks
  let (a, m') = insert b m
  modify $ \st -> st{ blocks = m' }
  return a
  
insert x m = case lookupR x m of
  Just a -> (a, m)
  Nothing -> let a = next m in (a, m{ hmap = M.insert a x $ hmap m, hmapR = M.insert x a $ hmapR m, next = succ a })

data Program = Program Label St

prog (a,b) = Program a b

compile :: Exp -> Program
compile e = prog $ runState (exit e) $ St empty empty M.empty

exit e = case e of
  AExp a -> terminator $ Exit a
  Call f -> f (terminator . Exit . \[a] -> a)

instance PP Program where pp (Program x y) = vcat [ text "main:", pp x, pp y ]
  
data Insn
  = Op Op [AExp]
  | Phi [(AExp, Label)]
  deriving (Show, Eq, Ord, Generic)
          
data Op = Add | Sub | Mul | Div | Mod | Eq | Ne | Gt | Lt | Gte | Lte
  deriving (Show, Eq, Ord, Generic, Enum)
instance Hashable Op

add = binop Add
sub = binop Sub
mul = binop Mul
div = binop Div
mod = binop Mod
eq = binop Eq
ne = binop Ne
gt = binop Gt
lt = binop Lt
gte = binop Gte
lte = binop Lte


instance Hashable Insn
instance PP Insn where
  pp x = case x of
    Op a bs -> hsep $ pp a : map pp bs
    Phi bs -> text "phi" <+> pp bs
      
data Exp
  = AExp AExp
  | Call (Cont -> M Label)

data Terminator
  = Exit AExp
  | Branch Label
  | Switch AExp Label [Label]
  | Until AExp Label
  deriving (Show, Eq, Ord, Generic)

instance PP Terminator where
  pp x = case x of
    Exit a -> text "exit" <+> pp a
    Branch a -> text "branch" <+> pp a
    Switch a b bs -> hsep ([text "switch", pp a, pp b] ++ map pp bs)
    
instance Hashable Terminator

type M a = State St a
data St = St
  { insns :: Map Register Insn
  , blocks :: Map Label Terminator -- Can this be a simple HashMap?
  , phis :: M.HashMap Label [AExp]
  }
  deriving Show

instance PP St where
  pp st = vcat [ text "registers:", pp $ insns st, text "blocks:", pp $ blocks st ]

int = AExp . AInt

op o xs = Call $ flip (opf o) xs
  
binop o x y = op o [x,y]

opf :: Op -> Cont -> [Exp] -> M Label
opf o cont = loop []
  where
    loop xs ys = case ys of
      [] -> do
        r <- instruction $ Op o $ reverse xs
        cont [r]
      y:ys' -> case y of
        AExp x -> loop (x:xs) ys'
        Call f -> f $ \[a] -> loop (a : xs) ys'

type Cont = [AExp] -> M Label

insertPhis :: Label -> [AExp] -> M ()
insertPhis x ys = modify $ \st -> st{ phis = M.insert x ys $ phis st }
  
switch x y ys = Call $ flip switchf (x:y:ys)
  
loop b e r = ift (e `gt` 0) (loop (b * b) (e `div` 2) (ift ((e `mod` 2) `ne` 0) (r*b) r)) r

expsf :: Cont -> [Exp] -> M Label
expsf cont = loop []
  where
    loop xs [] = cont $ reverse xs
    loop xs (y:ys) = case y of
      AExp x -> loop (x:xs) ys
      Call f -> f $ \[x] -> loop (x:xs) ys

adjust :: (Hashable a, Eq a, Hashable b, Eq b) => (b -> b) -> a -> Map a b -> Map a b
adjust f a m = case lookup a m of
  Nothing -> m
  Just b -> let b' = f b in m{ hmap = M.insert a b' $ hmap m, hmapR = M.insert b' a $ hmapR m }

phiLabel :: M Label
phiLabel = do
  m <- gets blocks
  let lbl = next m
  modify $ \st -> st{ blocks = m{ next = succ lbl } }
  return lbl

phi :: Cont -> [[Exp]] -> M Label
phi cont xs = do
  lbl0 <- phiLabel
  lbls <- mapM (expsf (\bs -> terminator (Branch lbl0) >>= \l -> insertPhis lbl0 bs >> return l)) xs
  m <- gets phis
  rs <- mapM (instruction . Phi) $ transpose [ let Just es = M.lookup l m in [ (e,l) | e <- es ] | l <- lbls ]
  lbl <- cont rs
  modify $ \st ->
    st{ phis = M.delete lbl0 $ phis st
      , blocks = foldr (adjust (\(Branch _) -> Branch lbl)) (blocks st) lbls
      }
  return lbl

switchf :: Cont -> [Exp] -> M Label
switchf cont (x:ys) = case x of
  Call f -> f $ \[a] -> switchf cont (AExp a : ys)
  AExp a -> do
    undefined
    -- lbl:lbls <- mapM (\[a] -> expsf cont a) ys -- BAL: when to join these with a phi node?  always?
    -- terminator $ Switch a lbl lbls

dbl x = x + x

until :: ([Exp] -> Exp) -> ([Exp] -> [Exp]) -> [Exp] -> [Exp]
until f g xs = Call $ \cont -> untilf cont undefined undefined
  
untilf :: Cont -> [Exp] -> [Exp] -> M Label
untilf cont xs ys = phi cont' [xs,ys]
  where
    cont' = \(e:es) -> do
      lbl <- cont es
      terminator $ Until e lbl
      
-- untilf :: Cont -> [Exp] -> M Label
-- untilf cont = loop []
--   where
--     loop xs ys = case ys of
--       [] -> do
--         let x':xs' = reverse xs
--         lbl <- cont xs'
--         terminator $ Until x' lbl
--       y:ys' -> case y of
--         AExp x -> loop (x:xs) ys'
--         Call f -> f $ \[a] -> loop (a : xs) ys'

ift x y z = switch x y [z]

instance Num Exp where
  fromInteger = AExp . AInt
  (*) = mul
  (+) = add
  (-) = sub
  abs = undefined
  signum = undefined
-}
  
-- loop :: (Num a) => a -> a -> a -> a
-- loop :: M Exp -> M Exp -> M Exp -> M Exp
-- loop b e r = ift (e > 0) (loop (b*b) (e `div` 2) (ift ((e `mod` 2) /= 0) (r*b) r)) r

-- t = and [ fastpow a b == a^b | a <- [0..9], b <- [0..9] ]

-- ifnz x y z = if (x /= 0) then y else z

-- lte x y = fromEnum (x <= y)

-- data Exp
--   = AExp AExp
--   | IfT Exp Exp Exp
--   | Call [AExp] [Exp]
--   deriving (Show, Eq)

-- nameIfT :: Exp -> Exp -> Exp -> M [Exp]
-- nameIfT = undefined

-- foo :: Exp -> M Exp
-- foo x = case x of
--   Call bs (IfT a t f : cs) -> do
--     cs' <- nameIfT a t f
--     foo $ Call bs (Call [] cs' : cs)
--   Call bs (c@Call{} : cs) -> undefined
--   -- make a new function f : \a -> Call bs (AExp a : cs)
--   -- c where the continuation passed into c is f
--   Call bs (AExp i : cs) -> foo $ Call (i:bs) cs
--   Call bs [] -> return $ Call (reverse bs) []

--   AExp{} -> return x

--   IfT AExp{} Call{} Call{} -> return x
--   IfT (IfT a t f) b c -> do
--     cs' <- nameIfT a t f
--     foo $ IfT (Call [] cs') b c
--   IfT Call{} b c -> undefined
  
-- instance Num (M Exp) where
--   (*) = undefined

-- ift :: M Exp -> M Exp -> M Exp -> M Exp
-- ift x y z = do
--   a <- x
--   case a of
--     AExp (Int 0) -> z
--     AExp (Int _) -> y
--     AExp (Reg r) -> do
--       lbl <- label
--       undefined -- return $ Call $ \ret -> IfT r (jump ret y) (jump ret z)
--     Call f -> do -- the scrutinee is a call
--       lbl <- label
--       reg <- register
--       fun lbl [reg] $ ift (liftReg reg) y z
--       f lbl

-- liftReg = return . AExp . Reg

-- fun :: Label -> [Register] -> M Exp -> M ()
-- fun = undefined

-- data Exp
--   = AExp AExp
--   | Call (Label -> M Exp)
           
-- data AExp
--   = Int Integer
--   | Reg Register
--   deriving (Show, Eq)

-- -- fastpow b e = loop b e 1

-- (>) = undefined
-- div = undefined
-- mod = undefined
-- (/=) = undefined

-- data Op = GT | MUL | DIV | MOD | NE deriving (Show, Eq)

-- -- loop' :: M Exp -> M Exp -> M Exp -> M Exp
-- loop' ret b e r =
--   let v0 = e > 0
--       loop'' =
--         let
--           v1 = b*b
--           v2 = e `div` 2
--           v3 = e `mod` 2
--           v4 = v3 /= 0
--           loop''' =
--             let v5 = r*b
--             in loop' ret v1 v2 v5
--         in ift v4 loop''' (loop' ret v1 v2 r)
--   in ift v0 loop'' (ret r)

-- newtype Register = Register Int deriving (Show, Eq, Ord, Enum)
-- newtype Label = Label Int deriving (Show, Eq, Ord, Enum)

-- -- data Block = Block Lbl [Reg] Term

-- type M a = State St a

-- type Target = (Lbl, [AExp])
-- data Term
--   = Jump Target
--   | Switch AExp [Target]
--   deriving (Show, Eq)

-- ift' :: M AExp -> M Term -> M Term -> M Term
-- ift' x y z = switch x [z,y]

-- switch :: M AExp -> [M Term] -> M Term
-- switch x ys = do
--   a <- x
--   lbls <- sequence $ replicate (length ys) newLbl
--   return $ Switch a [ (lbl, []) | lbl <- lbls ]
--               -- Switch <$> x <*> mapM target ys

-- target :: M Term -> M Target
-- target = undefined

-- foo :: Lbl -> M Term -> M ()
-- foo = undefined

-- block3 :: (Lbl -> M AExp -> M AExp -> M AExp -> M Term) -> M Block
-- block3 f = do
--   lbl <- newLbl
--   r0 <- newReg
--   r1 <- newReg
--   r2 <- newReg
--   let e = return . R
--   t <- f lbl (e r0) (e r1) (e r2)
--   return $ Block lbl [r0, r1, r2] t
    
-- -- block :: Lbl -> [M AExp] -> M Term -> M Block
-- -- block x ys z = sequence ys >>= \ys' -> return $ Block x (map unreg ys') 

-- loop' :: Lbl -> M Block
-- loop' = \ret -> block3 $ \lbl b e r ->
--   let f r = jump lbl [b*b, e `div` 2, r]
--   in ift' (e >. 0) (ift' ((e `mod` 2) /=. 0) (f (r*b)) (f r)) (jump ret [r])

-- register :: M Register
-- register = do
--   r <- gets nextReg
--   modify $ \st -> st{ nextReg = succ r }
--   return r

-- label :: M Label
-- label = do
--   r <- gets nextLbl
--   modify $ \st -> st{ nextLbl = succ r }
--   return r

-- data St = St
--   { regs :: Map Register (Op, [AExp])
--   , nextReg :: Register
--   , nextLbl :: Label
--   }
              
-- binop :: Op -> M AExp -> M AExp -> M AExp
-- binop o x y = do
--   a <- x
--   b <- y
--   r <- newReg
--   modify $ \st -> st{ regs = insert r (o, [a,b]) $ regs st }
--   return $ R r

-- jump :: Lbl -> [M AExp] -> M Term
-- jump x ys = sequence ys >>= \ys' -> return $ Jump (x, ys')

-- (>.) :: M AExp -> M AExp -> M AExp
-- (>.) = binop Gt

-- (/=.) :: M AExp -> M AExp -> M AExp
-- (/=.) = binop Ne

-- unused = error "unused"

-- instance Integral (M AExp) where
--   quotRem x y = (binop Quot x y, binop Rem x y)
--   toInteger = unused
  
-- instance Real (M AExp) where toRational = unused
-- instance Enum (M AExp) where
--   toEnum = unused
--   fromEnum = unused
  
-- instance Eq (M AExp) where (==) = unused
-- instance Ord (M AExp) where (<=) = unused

-- instance Num (M AExp) where
--   fromInteger = return . I
--   (+) = binop Add
--   (*) = binop Mul
--   (-) = binop Sub
--   abs = unused
--   signum = unused

-- -- unreg (Atom (R x)) = x

-- -- type Dag a b = (a, Map a b)

-- -- loop b1 e1 r1 = v3
-- --       where
-- --         v4 = lte e1 0
-- --         v3 = ifnz v4 r1 v1
-- --         v6 = next b1 e1 r1
-- --         v1 = ifnz t0 v2 v6
-- --         v2 = next b1 e1 r2
-- --         r2 = r1 * b1
-- --         t0 = e1 `mod` 2
        
-- -- next b1 e1 r3 = loop b3 e3 r3
-- --   where
-- --         b3 = b1 * b1
-- --         e3 = e1 `div` 2


-- -- -- lte' x y = Call (Fun 0) [x, y]
-- -- -- mod' x y = Call (Fun 1) [x, y]
-- -- -- div' x y = Call (Fun 2) [x, y]
-- -- -- (*.) x y = Call (Fun 3) [x, y]

-- -- data Var = Var Int
-- --   deriving Show

-- -- data Fun = Fun Int
-- --   deriving Show
           
-- -- data AExp
-- --   = V Var
-- --   | C Int
-- --   deriving Show

-- -- type FunD = Map Fun Lam
-- -- type VarD = Map Var (Either If Call)

-- -- data Lam = Lam [Var] Var deriving Show
-- -- data Call = Call Fun [AExp] deriving Show
-- -- data If = IfNZ AExp AExp AExp deriving Show

-- -- lte' :: AExp -> AExp -> AExp
-- -- lte' = undefined

-- -- mod' :: AExp -> AExp -> AExp
-- -- mod' = undefined

-- -- div' :: AExp -> AExp -> AExp
-- -- div' = undefined

-- -- (*.) :: AExp -> AExp -> AExp
-- -- (*.) = undefined

-- -- lam :: [AExp] -> AExp -> AExp
-- -- lam = undefined

-- -- ifnz' :: AExp -> AExp -> AExp -> AExp
-- -- ifnz' = undefined

-- -- fastpow' b0 e0 = loop b0 e0 (C 1)
-- --   where
-- --     loop :: AExp -> AExp -> AExp -> AExp
-- --     loop b1 e1 r1 = lam [b1, e1, r1] $ ifnz' v4 r1 v1
-- --       where
-- --         v4 = lte' e1 (C 0)
-- --         v3 = ifnz' v4 r1 v1
-- --         v6 = next r1
-- --         v1 = ifnz' v7 v2 v6
-- --         v2 = next r2
-- --         r2 = r1 *. b1
-- --         v7 = e1 `mod'` (C 2)
-- --         e3 = e1 `div'` (C 2)
-- --         b3 = b1 *. b1
-- --         next :: AExp -> AExp
-- --         next r3 = v5
-- --           where
-- --             v5 = loop b3 e3 r3
