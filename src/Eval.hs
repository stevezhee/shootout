{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Eval where

import Control.Monad.State hiding (mapM, sequence)
import Control.Monad
import Untyped
import Data.Maybe
import Data.Ratio
import Text.PrettyPrint hiding (int, empty)
import Typed (E(..))

data ESt = ESt{ env :: [(Var, Rational)] } deriving Show

instance PP ESt where pp = vcat . map pp . env

type Eval a = State ESt a

evalBound b e =
  evalExp e >>= \v -> modify $ \st -> st{ env = [(BVar b, v)] ++ env st }

evalExp = eval . unExp

eval :: Expr Exp -> Eval Rational
eval = \case
  AExp a -> case a of
    Left b -> case b of
      Rat _ r -> return r
      -- Undef _ -> error "eval:undef"
    Right b -> gets env >>= return . fromMaybe (unused "eval:VAExp") . lookup b
  App (Left a) bs -> let f = fromMaybe (unused "eval:App") (lookup (uop a) $ optbl $ otype a) in mapM evalExp bs >>= return . f
  Switch a bs c -> do
    i <- evalExp a >>= return . fromInteger . numerator
    evalExp $ if i < length bs then (bs !! i) else c
  While _ a t c -> do
    mapM_ (\(b, (e,_)) -> evalBound b e) t
    let go = do
          r <- evalExp a >>= return . toEnum . fromInteger . numerator
          if r
             then do
               mapM_ (\(b, (_,e)) -> evalBound b e) t
               go
             else eval $ bvar c
    go

runEval :: E a -> (Rational, ESt)
runEval x = flip runState (ESt []) $ evalExp $ unE x

binop :: (Rational -> Rational -> Rational) -> [Rational] -> Rational
binop f = \case
  [a,b] -> f a b
  _ -> error "binop"

binopi :: (Integer -> Integer -> Integer) -> [Rational] -> Rational
binopi f = \case
  [a,b] -> toRational $ f (numerator a) (numerator b)
  _ -> error "binop"

binop2 :: Type -> (Rational -> Rational -> Rational) -> (Integer -> Integer -> Integer) -> [Rational] -> Rational
binop2 t f g = case t of
  TFloating{} -> binop f
  _ -> binopi g

cmpop :: (Rational -> Rational -> Bool) -> [Rational] -> Rational
cmpop f = \case
  [a,b] -> toRational $ fromEnum $ f a b
  _ -> error "cmpop"

frem = unused "frem"

optbl :: Type -> [(UOp, [Rational] -> Rational)]
optbl t =
  (Add, binop (+)) :
  (Sub, binop (-)) :
  (Mul, binop (*)) :
  (Div, binop2 t (/) div) :
  (Rem, binop2 t frem rem) :
  (Eq, cmpop (==)) :
  (Ne, cmpop (/=)) :
  (Lt, cmpop (<)) :
  (Gt, cmpop (>)) :
  (Lte, cmpop (<=)) :
  (Gte, cmpop (>=)) :
  []
