module SMTEval where

import Untyped
import qualified Text.PrettyPrint as PP
import Text.PrettyPrint
import Data.Char
import Control.Exception
import Data.Ratio

smtEval :: ([(Free, CExp)], [Defn AExp]) -> Doc
smtEval (xs, ys) = vcat $
  map defnFree xs ++
  map covFree xs ++
  map satFree xs

satFree (x, _) = vcat
  [ sexp' "push" []
  , sexp' "assert" [cov x]
  , sexp' "check-sat" []
  , sexp' "get-value" [text "(u0)"] -- BAL:
  , sexp' "pop" []
  ]

class PPSMT a where ppsmt :: a -> Doc

ppsmtUOp :: Type -> UOp -> Doc
ppsmtUOp t x = pp x <> pp t

cmpUOp :: (PPSMT a, Typed a) => UOp -> a -> Int -> Doc
cmpUOp o a i =
  sexp' "isOne" [sexp [ppsmtUOp t o, ppsmt a, ppsmt $ Rat t $ fromIntegral i ] ]
  where t = typeof a
         
(.==) :: (PPSMT a, Typed a) => a -> Int -> Doc
(.==) = cmpUOp Eq
(.>=) = cmpUOp Gte

instance (PPSMT a, Typed a) => PPSMT (CExpr a) where
  ppsmt x = case x of
    App a bs -> sexp (either (ppsmtUOp (typeof $ head bs) . uop) pp a : map ppsmt bs)
    Switch a bs c ->
      foldr
        (\(i,b) e -> sexp' "ite" [a .== i, ppsmt b, e])
        (ppsmt c)
        (zip [0 ..] bs)
    -- While Integer a [(Bound, (a, a))] Bound

instance (PPSMT a, PPSMT b) => PPSMT (Either a b) where ppsmt = either ppsmt ppsmt
instance PPSMT Var where ppsmt = pp

instance PPSMT Lit where
  ppsmt (Rat t r) = case t of
    TInt a b -> assert (denominator r == 1) $
      sexp' "_" [text "bv" <> integer (numerator r), integer b]
      
defnFree :: (Free, CExp) -> Doc
defnFree (x,y) = defineConst (pp x) (pp $ typeof x) (ppsmt y)

(.&) :: Doc -> Doc -> Doc
(.&) x y = sexp' "and" [x,y]

implies :: Doc -> Doc -> Doc
implies x y = sexp' "assert" [sexp' "=>" [x, y]]

impliesAExp :: Doc -> AExp -> Doc
impliesAExp x y = case y of
  Right (FVar a) -> x `implies` cov a
  _ -> empty

impliesBranch :: Doc -> (Doc, AExp) -> Doc
impliesBranch x (y,z) = vcat
  [ declareBool y
  , y `implies` x
  , y `impliesAExp` z
  ]
      
cov :: PP a => a -> Doc
cov x = text "c" <> pp x

covBranch :: PP a => a -> Int -> Doc
covBranch x i = cov x <> text "!" <> int i

declareBool x = declareConst x (text "Bool")
  
covFree :: (Free, CExp) -> Doc
covFree (x,y) = vcat $
  declareBool (cov x) :
  case y of
    App _ bs -> map (impliesAExp (cov x)) bs
    Switch a bs c ->
      cov x `impliesAExp` a :
      (cov x .& (a .>= length bs)) `impliesBranch` (covBranch x $ length bs, c) :
      [ (cov x .& (a .== i)) `impliesBranch` (covBranch x i, b) | (i,b) <- zip [0 ..] bs ]

sexp = parens . hsep
sexp' x ys = sexp (text x : ys)
defineConst x y z = sexp' "define-fun" [ x, parens empty, y, z ]
declareConst x y = sexp' "declare-fun" [ x, parens empty, y ]
