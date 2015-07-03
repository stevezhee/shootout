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
import qualified Data.HashMap.Strict as M
import           Data.Hashable
import           Data.List hiding (insert, lookup)
import           GHC.Generics (Generic)
import           Prelude hiding (div, mod, lookup)
import qualified Text.PrettyPrint as PP
import           Text.PrettyPrint hiding (int, empty)
class PP a where pp :: a -> Doc

instance PP a => PP [a] where pp = parens . hsep . map pp
  
instance (PP a, PP b) => PP (a,b) where pp (a,b) = parens (pp a <+> pp b)

newtype Free = Free Integer deriving (Show, Eq, Num, Ord, Generic, Enum)
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

data Op = Add | Sub | Mul | Div | Mod | Eq | Ne | Gt | Lt | Gte | Lte
  deriving (Show, Eq, Ord, Generic, Enum)
instance Hashable Op
instance PP Op where pp = text . show

instance PP Integer where pp = integer

type NumBV = Integer

data AExp
  = Int Integer
  | FVar Free
  | BVar Bound
  | UVar User
  deriving (Show, Eq, Generic, Ord)
instance Hashable AExp

data Exp
  = EAExp AExp
  | EOp NumBV Op [Exp]
  | ESwitch NumBV Exp [Exp] Exp
  | EWhile NumBV [Phi Exp] Exp Exp
  deriving (Show)

type Phi a = (Bound, (a, a))

data CExp
  = CAExp AExp
  | COp Op [AExp]
  | CSwitch AExp [AExp] AExp
  | CWhile [Phi AExp] AExp AExp
  deriving (Show, Eq, Generic, Ord)
instance Hashable CExp

fromCExp :: CExp -> Exp
fromCExp x = case x of
  CAExp a -> EAExp a
  COp a bs -> EOp 0 a $ map EAExp bs
  CSwitch a bs c -> ESwitch 0 (EAExp a) (map EAExp bs) (EAExp c)
  CWhile bs c d -> EWhile 0 [ (v, (EAExp p, EAExp q)) | (v, (p, q)) <- bs ] (EAExp c) (EAExp d)

instance PP CExp where pp = pp . fromCExp
  
aexp :: Exp -> M AExp
aexp x = cexp x >>= name

name :: CExp -> M AExp
name x = case x of
  CAExp a -> return a
  _ -> do
    tbl <- gets cexps
    let (a, tbl') = insertR x tbl
    modify $ \st -> st{ cexps = tbl' }
    return $ FVar a

swap (x,y) = (y,x)
pair x y = (x,y)

instance (Hashable b, Eq b, Num a, PP a, PP b, Ord b, Ord a) => PP (MapR a b) where
  pp = vcat . map (\(a,b) -> hcat [ pp a, text ": ", pp b]) . sort . map swap . M.toList . hmapR

lookupR :: (Hashable b, Eq b) => b -> MapR a b -> Maybe a
lookupR b = M.lookup b . hmapR

insertR :: (Hashable b, Eq b, Enum a) => b -> MapR a b -> (a, MapR a b)
insertR b tbl = case lookupR b tbl of
  Just a -> (a, tbl)
  Nothing -> let a = next tbl in (a, tbl{ next = succ a, hmapR = M.insert b a $ hmapR tbl })
    
cexp :: Exp -> M CExp
cexp x = case x of
  EAExp a -> return $ CAExp a
  EOp _ b cs -> COp b <$> mapM aexp cs
  ESwitch _ b cs d -> CSwitch <$> aexp b <*> mapM aexp cs <*> aexp d
  EWhile _ bs c d -> CWhile <$> mapM f bs <*> aexp c <*> aexp d
    where f (v, (p, q)) = pair v <$> (pair <$> aexp p <*> aexp q)

instance PP Exp where
  pp x = case x of
    ESwitch _ a bs c ->
      parens $ vcat [text "Switch", nest 2 $ vcat $ map pp $ a : bs ++ [c] ]
    EOp _ a bs -> parens (pp a <+> hsep (map pp bs))
    EAExp a -> pp a
    EWhile _ bs c d -> parens $ vcat [text "While", nest 2 $ vcat [pp bs, pp c, pp d]]

instance PP AExp where
  pp x = case x of
    FVar a -> pp a
    BVar a -> pp a
    UVar a -> pp a
    Int a -> pp a

instance (PP a, PP b, PP c, PP d) => PP (a,b,c,d) where pp (a,b,c,d) = parens (vcat [pp a, pp b, pp c, pp d])

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

switch :: Exp -> [Exp] -> Exp -> Exp
switch x ys z = ESwitch (maximumBV $ x : z : ys) x ys z

var = EAExp . UVar . User

ife x y z = switch x [z] y
  
instance Num Exp where
  fromInteger = EAExp . Int
  (*) = mul
  (+) = add
  (-) = sub
  abs = undefined
  signum = undefined

while :: [Exp] -> ([Exp] -> (Exp, [Exp], Exp)) -> Exp
while xs f = assert (length xs == length ys) $ EWhile (n + m) (zip vs $ zip xs ys) e r
  where
    m = genericLength xs
    (e, ys, r) = f $ map (EAExp . BVar) vs
    vs = map (Bound . (+) n) [0 .. m - 1]
    n = maximumBV (e : xs ++ ys)

fastpow b e = while [b, e, 1] $ \[b, e, r] -> (e `gt` 0, [ b * b, e `div` 2, ife ((e `mod` 2) `ne` 0) (r * b) r ], r)

dbl x = x + x
  
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

type M a = State St a

emptyR = MapR M.empty 0

run = flip runState $ St emptyR

instance PP St where
  pp x = pp $ cexps x
  
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
