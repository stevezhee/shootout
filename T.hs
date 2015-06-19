{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module T where

import Prelude
import Control.Applicative hiding (empty)
import Control.Monad.State
import qualified Data.HashMap.Strict as M
import Data.Hashable
import GHC.Generics (Generic)
import Text.PrettyPrint hiding (int, empty)
import qualified Text.PrettyPrint as PP
import Data.List hiding (insert)

data Map a b = Map{ hmap :: M.HashMap b a, next :: a }
  deriving Show

class PP a where pp :: a -> Doc

instance (Hashable b, Eq b, Num a, PP a, PP b, Ord b, Ord a) => PP (Map a b) where
  pp = vcat . map (\(a,b) -> hcat [ pp a, text ": ", pp b]) . sort . map swap . M.toList . hmap

swap (a,b) = (b,a)

empty :: (Hashable b, Eq b, Num a) => Map a b
empty = Map M.empty 0

lookupR :: (Hashable b, Eq b) => b -> Map a b -> Maybe a
lookupR b = M.lookup b . hmap

instruction :: Op -> [AExp] -> M AExp
instruction x ys = do
  m <- gets insns
  let (a, m') = insert (Insn x ys) m
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
  Nothing -> let a = next m in (a, m{ hmap = M.insert x a $ hmap m, next = succ a })

compile :: Exp -> (Label, St)
compile e = runState (exit e) $ St empty empty

exit e = case e of
  AExp a -> terminator $ Exit a
  Call f -> f (terminator . Exit)

data AExp = AInt Integer | AReg Register deriving (Show, Eq, Ord, Generic)
instance Hashable AExp
instance PP AExp where
  pp x = case x of
    AInt a -> integer a
    AReg a -> pp a
    
newtype Register = Register Integer deriving (Show, Eq, Num, Ord, Generic, Enum)
instance Hashable Register
instance PP Register where
  pp (Register a) = text "R" <> integer a
  
newtype Label = Label Integer deriving (Show, Eq, Num, Ord, Generic, Enum)
instance Hashable Label
instance PP Label where
  pp (Label a) = text "L" <> integer a
  
data Insn = Insn Op [AExp] deriving (Show, Eq, Ord, Generic)

data Op = Add | Sub | Mul | Div | Mod | Eq | Neq | Gt | Lt | Gte | Lte
  deriving (Show, Eq, Ord, Generic, Enum)
instance Hashable Op

instance PP Op where pp = text . show

instance Hashable Insn
instance PP Insn where
  pp (Insn a bs) = hsep $ pp a : map pp bs
  
data Exp
  = AExp AExp
  | Call ((AExp -> M Label) -> M Label)

data Terminator
  = Exit AExp
  | Branch Label [AExp]
  | Switch AExp Label [Label]
  deriving (Show, Eq, Ord, Generic)

instance PP Terminator where
  pp x = case x of
    Exit a -> text "exit" <+> pp a
    Branch a bs -> hsep ([text "branch", pp a] ++ map pp bs)
    Switch a b bs -> hsep ([text "switch", pp a, pp b] ++ map pp bs)
    
instance Hashable Terminator

type M a = State St a
data St = St{ insns :: Map Register Insn, blocks :: Map Label Terminator }
  deriving Show

instance PP St where
  pp st = vcat [ text "registers:", pp $ insns st, text "blocks:", pp $ blocks st ]

int = AExp . AInt

add :: Exp -> Exp -> Exp
add x y = Call $ flip addf [x,y]

expf cont e = case e of
  AExp a -> cont a
  Call f -> f $ \a -> expf cont $ AExp a
  
addf :: (AExp -> M Label) -> [Exp] -> M Label
addf cont xs = case xs of
  [AExp a, AExp b] -> do
    r <- instruction Add [a, b]
    cont r
  [Call f, b] -> f $ \a -> addf cont [AExp a, b]
  [a, Call f] -> f $ \b -> addf cont [a, AExp b]

switch x y ys = Call $ flip switchf (x:y:ys)

switchf cont (x:ys) = case x of
  AExp a -> do
    lbl:lbls <- mapM (expf cont) ys -- BAL: when to join these?  always?
    terminator $ Switch a lbl lbls

  Call f -> f $ \a -> switchf cont (AExp a : ys)
  
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

-- loop :: (Num a) => a -> a -> a -> a
-- loop :: M Exp -> M Exp -> M Exp -> M Exp
-- loop b e r = ift (e > 0) (loop (b*b) (e `div` 2) (ift ((e `mod` 2) /= 0) (r*b) r)) r

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
