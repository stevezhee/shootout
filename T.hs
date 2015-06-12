{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module T where

import Control.Applicative
import Control.Monad.State
import Data.Map hiding (map)

t = and [ fastpow a b == a^b | a <- [0..9], b <- [0..9] ]

-- ifnz x y z = if (x /= 0) then y else z

-- lte x y = fromEnum (x <= y)

ift x y z = if x then y else z

fastpow b e = loop b e 1

loop b e r = ift (e > 0) (loop (b*b) (e `div` 2) (ift ((e `mod` 2) /= 0) (r*b) r)) r

data Op = Gt | Mul | Div | Rem | Quot | Lte | Ne | Add | Sub deriving (Show, Eq)

newtype Reg = Reg Int deriving (Show, Eq, Ord, Enum)
newtype Lbl = Lbl Int deriving (Show, Eq, Ord, Enum)

data AExp
  = I Integer
  | R{ unreg :: Reg }
  deriving (Show, Eq)

data Block = Block Lbl [Reg] Term

type M a = State St a

type Target = (Lbl, [AExp])
data Term
  = Jump Target
  | Switch AExp [Target]
  deriving (Show, Eq)

ift' :: M AExp -> M Term -> M Term -> M Term
ift' x y z = switch x [z,y]

switch :: M AExp -> [M Term] -> M Term
switch x ys = do
  a <- x
  lbls <- sequence $ replicate (length ys) newLbl
  return $ Switch a [ (lbl, []) | lbl <- lbls ]
              -- Switch <$> x <*> mapM target ys

target :: M Term -> M Target
target = undefined

foo :: Lbl -> M Term -> M ()
foo = undefined

block3 :: (Lbl -> M AExp -> M AExp -> M AExp -> M Term) -> M Block
block3 f = do
  lbl <- newLbl
  r0 <- newReg
  r1 <- newReg
  r2 <- newReg
  let e = return . R
  t <- f lbl (e r0) (e r1) (e r2)
  return $ Block lbl [r0, r1, r2] t
    
-- block :: Lbl -> [M AExp] -> M Term -> M Block
-- block x ys z = sequence ys >>= \ys' -> return $ Block x (map unreg ys') 

loop' :: Lbl -> M Block
loop' = \ret -> block3 $ \lbl b e r ->
  let f r = jump lbl [b*b, e `div` 2, r]
  in ift' (e >. 0) (ift' ((e `mod` 2) /=. 0) (f (r*b)) (f r)) (jump ret [r])

newReg :: M Reg
newReg = do
  r <- gets nextReg
  modify $ \st -> st{ nextReg = succ r }
  return r

newLbl :: M Lbl
newLbl = do
  r <- gets nextLbl
  modify $ \st -> st{ nextLbl = succ r }
  return r

data St = St
  { regs :: Map Reg (Op, [AExp])
  , nextReg :: Reg
  , nextLbl :: Lbl
  }
              
binop :: Op -> M AExp -> M AExp -> M AExp
binop o x y = do
  a <- x
  b <- y
  r <- newReg
  modify $ \st -> st{ regs = insert r (o, [a,b]) $ regs st }
  return $ R r

jump :: Lbl -> [M AExp] -> M Term
jump x ys = sequence ys >>= \ys' -> return $ Jump (x, ys')

(>.) :: M AExp -> M AExp -> M AExp
(>.) = binop Gt

(/=.) :: M AExp -> M AExp -> M AExp
(/=.) = binop Ne

unused = error "unused"

instance Integral (M AExp) where
  quotRem x y = (binop Quot x y, binop Rem x y)
  toInteger = unused
  
instance Real (M AExp) where toRational = unused
instance Enum (M AExp) where
  toEnum = unused
  fromEnum = unused
  
instance Eq (M AExp) where (==) = unused
instance Ord (M AExp) where (<=) = unused

instance Num (M AExp) where
  fromInteger = return . I
  (+) = binop Add
  (*) = binop Mul
  (-) = binop Sub
  abs = unused
  signum = unused

-- unreg (Atom (R x)) = x

-- type Dag a b = (a, Map a b)

-- loop b1 e1 r1 = v3
--       where
--         v4 = lte e1 0
--         v3 = ifnz v4 r1 v1
--         v6 = next b1 e1 r1
--         v1 = ifnz t0 v2 v6
--         v2 = next b1 e1 r2
--         r2 = r1 * b1
--         t0 = e1 `mod` 2
        
-- next b1 e1 r3 = loop b3 e3 r3
--   where
--         b3 = b1 * b1
--         e3 = e1 `div` 2


-- -- lte' x y = Call (Fun 0) [x, y]
-- -- mod' x y = Call (Fun 1) [x, y]
-- -- div' x y = Call (Fun 2) [x, y]
-- -- (*.) x y = Call (Fun 3) [x, y]

-- data Var = Var Int
--   deriving Show

-- data Fun = Fun Int
--   deriving Show
           
-- data AExp
--   = V Var
--   | C Int
--   deriving Show

-- type FunD = Map Fun Lam
-- type VarD = Map Var (Either If Call)

-- data Lam = Lam [Var] Var deriving Show
-- data Call = Call Fun [AExp] deriving Show
-- data If = IfNZ AExp AExp AExp deriving Show

-- lte' :: AExp -> AExp -> AExp
-- lte' = undefined

-- mod' :: AExp -> AExp -> AExp
-- mod' = undefined

-- div' :: AExp -> AExp -> AExp
-- div' = undefined

-- (*.) :: AExp -> AExp -> AExp
-- (*.) = undefined

-- lam :: [AExp] -> AExp -> AExp
-- lam = undefined

-- ifnz' :: AExp -> AExp -> AExp -> AExp
-- ifnz' = undefined

-- fastpow' b0 e0 = loop b0 e0 (C 1)
--   where
--     loop :: AExp -> AExp -> AExp -> AExp
--     loop b1 e1 r1 = lam [b1, e1, r1] $ ifnz' v4 r1 v1
--       where
--         v4 = lte' e1 (C 0)
--         v3 = ifnz' v4 r1 v1
--         v6 = next r1
--         v1 = ifnz' v7 v2 v6
--         v2 = next r2
--         r2 = r1 *. b1
--         v7 = e1 `mod'` (C 2)
--         e3 = e1 `div'` (C 2)
--         b3 = b1 *. b1
--         next :: AExp -> AExp
--         next r3 = v5
--           where
--             v5 = loop b3 e3 r3
