{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module EvalSBV where

import Untyped
import Data.SBV hiding (compileToSMTLib)
import Data.SBV.Dynamic
import Data.SBV.Internals (imposeConstraint)
import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Maybe

data St = St
  { freeTbl :: Vector SVal
  , defnTbl :: HashMap String SVal
  , userTbl :: Vector SVal
  }

type S a = StateT St Symbolic a

sbv :: ([(Free, CExp)], [Defn AExp]) -> IO ()
sbv (xs, ys) = do
  s <- compileToSMTLib SMTLib2 True $ do
    bs <- V.fromList <$> mapM (\(a,_) -> sbvDeclare (show $ pp a) (ftype a)) xs
    cs <- M.fromList <$> mapM (\a -> (did a,) <$> sbvDeclare (did a) (dtype a)) ys
    ds <- V.fromList <$> mapM (\a -> sbvDeclare (show $ pp a) (utype a)) (dbvars $ head ys) -- BAL:
    flip evalStateT St{ freeTbl = bs, defnTbl = cs, userTbl = ds } $ do
      mapM_ sbvDefineFree xs
      mapM_ sbvDefineDefn ys
    return $ snd $ head $ M.toList cs -- BAL:
  putStrLn s

lookupFree :: Free -> S SVal
lookupFree x = flip fidIdx x <$> gets freeTbl

lookupUser :: User -> S SVal
lookupUser x = flip uidIdx x <$> gets userTbl

lookupDefn :: Defn AExp -> S SVal
lookupDefn x = (fromJust . M.lookup (did x)) <$> gets defnTbl
  
sbvAExp :: AExp -> S SVal
sbvAExp x = case x of
  LAExp a -> return $ case a of
    Rat t r
      | t == tbool -> if (r == 0) then svFalse else svTrue
      | otherwise -> case t of
        TFloating i -> case i of
          32 -> svFloat $ fromRational r
          64 -> svDouble $ fromRational r
          _ -> error "sbv aritrary sized floating types not implemented"
        TVector{} -> error "sbv vector types not implemented"
        _ -> svInteger (toKind t) $ numerator r
  VAExp a -> case a of
    FVar v -> lookupFree v
    UVar v -> lookupUser v
    
sbvUOp :: UOp -> [AExp] -> S SVal
sbvUOp x = case x of
  Add -> f2 svPlus
  Mul -> f2 svTimes
  Sub -> f2 svMinus
  Div -> f2 svDivide
  Rem -> f2 svRem
  And -> f2 svAnd
  Or -> f2 svOr
  Xor -> f2 svXOr
  -- Shl -> svShl
  -- Lshr -> svShr
  -- Ashr -> svShr
  -- Rol -> svRol
  -- Ror -> svRor
  
  Eq -> f2 svEqual
  Ne -> f2 svNotEqual
  Gt -> f2 svGreaterThan
  Lt -> f2 svLessThan
  Gte -> f2 svGreaterEq
  Lte -> f2 svLessEq
  Neg -> f1 svUNeg
  Abs -> f1 svAbs
  Not -> f1 svNot
  -- Signum -> sv
  
  -- Sqrt -> sv
  -- ExpF -> sv
  -- Log -> sv
  -- Sin -> sv
  -- Cos -> sv
  -- Asin -> sv
  -- Atan -> sv
  -- Acos -> sv
  -- Sinh -> sv
  -- Cosh -> sv
  -- Asinh -> sv
  -- Atanh -> sv
  -- Acosh -> sv
  -- InsertElement -> sv
  -- ExtractElement -> sv
  -- ShuffleVector -> sv
  -- Cast -> svSign or svUnsign
  
  where
    f1 f [a] = f <$> sbvAExp a
    f2 f [a,b] = f <$> sbvAExp a <*> sbvAExp b

sbvCExp :: CExp -> S SVal
sbvCExp x = case x of
  AExp a -> sbvAExp a
  App (Left a) bs -> sbvUOp (uop a) bs
  -- App (Either Op (Defn a)) [a]
  Switch a [b] c | typeof a == tbool -> svIte <$> sbvAExp a <*> sbvAExp c <*> sbvAExp b
  Switch a bs c -> svSelect <$> mapM sbvAExp bs <*> sbvAExp c <*> sbvAExp a
  -- While Integer a [(Bound, (a, a))] Bound
  
sbvDefine :: CExp -> SVal -> S ()
sbvDefine x v = (svEqual v <$> sbvCExp x) >>= lift . imposeConstraint

sbvDefineFree :: (Free, CExp) -> S ()
sbvDefineFree (x,y) = lookupFree x >>= sbvDefine y

sbvDefineDefn :: Defn AExp -> S ()
sbvDefineDefn x = case body x of
  Nothing -> error "sbv for externs not implemented"
  Just a -> lookupDefn x >>= sbvDefine (AExp a)
  
toKind :: Type -> Kind
toKind x = case x of
  TSInt a -> KBounded True $ fromIntegral a
  TUInt 1 -> KBool
  TUInt a -> KBounded False $ fromIntegral a
  TFloating a | a == 32 -> KFloat
  TFloating a | a == 64 -> KDouble
  _ -> error "sbv type not implemented"

sbvDeclare :: String -> Type -> Symbolic SVal
sbvDeclare s t = svMkSymVar Nothing (toKind t) (Just s)

-- sbvFree :: (Free, CExp) -> S ()
-- sbvFree (x, y) = sbvDefine (strFid x) (ftype x) y

-- class PPSMT a where ppsmt :: a -> Doc

-- instance PPSMT AExp where ppsmt = pp

-- instance (PPSMT a, Typed a, PP a) => PPSMT (Expr a) where
--   ppsmt x = case x of
--     AExp a -> ppsmt a
--     App eab bs -> smtFun (either (map toLower . show . uop) did eab) $ map ppsmt bs
--     -- Switch a bs c -> ppSwitchC (ppc a) (map ppc bs) (ppc c)
--     -- While _ a bs c -> ppWhileC (ppc a) [ (ppc (typeof p), (pp p, (ppc q, ppc r)))
--     --                                    | (p, (q, r)) <- bs ] (pp c)

-- hlist = parens . hsep

-- smtFun s xs = hlist (text s : xs)

-- smtFun0 = flip smtFun []
-- smtFun1 s x = smtFun s [x]
-- smtFun2 s x y = smtFun s [x,y]

-- smtDefineFun = smtFun2 "define-fun"
-- smtAssert = smtFun1 "assert"

-- smtPush = smtFun0 "push"
-- smtPop = smtFun0 "pop"
-- smtCheckSat = smtFun0 "check-sat"
-- smtGetModel = smtFun0 "get-model"

  -- u0 :: SInt32 <- exists "u0"
  -- u1 :: SInt32 <- exists "u1"
  -- f0 <- "f0" `sDef` (u0 .< u1)
  -- f1 <- "f1" `sDef` (u1 .< 14)
  -- f2 <- "f2" `sDef` (ite f1 u1 u0)
  -- f3 <- "f3" `sDef` (u0 .== 12)
  -- f4 <- "f4" `sDef` (u0 + u1)
  -- f5 <- "f5" `sDef` (ite f3 f4 u1)
  -- f6 <- "f6" `sDef` (select [f2] f5 (oneIf f0 :: SWord32))
  -- f6 <- "f6" `sDef` (ite f0 f5 f2)
  -- return [f0 &&& f3, f0 &&& bnot f3]

-- data Type = TInt32 | TBool

-- mkInt s = svMkSymVar Nothing (KBounded True 32) (Just s)
-- mkBool s = svMkSymVar Nothing KBool (Just s)

-- foo = do
--   u0 <- mkInt "u0"
--   u1 <- mkInt "u1"
--   f0 <- mkBool "f0"
--   constrain (f0 `svEqual` (u0 `svLessThan` u1))
--   return f0
  
--   sDef "f0" svLessThan "u0" "u1"
--   sDef "f1" svLessThan "u1" 14
--   f2 <- "f2" `sDef` (ite f1 u1 u0)
--   f3 <- "f3" `sDef` (u0 .== 12)
--   f4 <- "f4" `sDef` (u0 + u1)
--   f5 <- "f5" `sDef` (ite f3 f4 u1)
--   f6 <- "f6" `sDef` (select [f2] f5 (oneIf f0 :: SWord32))
--   f6 <- "f6" `sDef` (ite f0 f5 f2)
  
-- svEqual
-- svLessThan
-- svPlus
-- svIte

-- outputSVal :: SVal -> Symbolic ()
-- svMkSymVar :: Maybe Quantifier -> Kind -> Maybe String -> Symbolic SVal
-- svInteger :: Kind -> Integer -> SVal


--     (concat (_ BitVec i) (_ BitVec j) (_ BitVec m))
--       - concatenation of bitvectors of size i and j to get a new bitvector of
--         size m, where m = i + j
--     ((_ extract i j) (_ BitVec m) (_ BitVec n))
--       - extraction of bits i down to j from a bitvector of size m to yield a
--         new bitvector of size n, where n = i - j + 1

-- bvnot
-- bvand
-- bvor
-- bvneg
-- bvadd
-- bvmul
-- bvudiv
-- bvurem
-- bvshl
-- bvlshr
-- bvult
-- bvxor
-- bvsub
-- bvsdiv
-- bvsrem
-- bvsmod
-- bvashr
-- zero_extend
-- sign_extend
-- rotate_left
-- rotate_right
-- bvule
-- bvugt
-- bvuge
-- bvslt
-- bvsle
-- bvsgt
-- bvsge

--     (bvnand s t) abbreviates (bvnot (bvand s t))
--     (bvnor s t) abbreviates (bvnot (bvor s t))
--     (bvxor s t) abbreviates (bvor (bvand s (bvnot t)) (bvand (bvnot s) t))
--     (bvcomp s t) abbreviates (bvxnor s t) if m = 1, and
