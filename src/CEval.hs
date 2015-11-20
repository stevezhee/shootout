module CEval where

import Untyped
import Text.PrettyPrint

instance PPC Type where ppc = pp
class PPC a where ppc :: a -> Doc

instance PPC Lit where ppc = pp

instance PPC Free where ppc x = pp x <> parens (ppCommas $ map ppc $ fbvars x)
  
instance PPC Var where
  ppc x = case x of
    FVar a -> ppc a
    x -> pp x

instance (PPC a, PPC b) => PPC (Either a b) where ppc = either ppc ppc

instance (PPC a, Typed a, PP a) => PPC (CExpr a) where
  ppc x = case x of
    App a bs -> ppAppC a bs
    Switch a bs c -> ppSwitchC (ppc a) (map ppc bs) (ppc c)
    While _ a bs c -> ppWhileC (ppc a) [ (ppc (typeof p), (pp p, (ppc q, ppc r)))
                                       | (p, (q, r)) <- bs ] (pp c)
      

ppReturnC x = text "return" <+> x <> semi

ppBlockC x ys = x $$ (nest 2 $ vcat [ text "{" $+$ (nest 2 $ vcat ys), text "}" ])

ppAltC :: Doc -> Doc -> Doc
ppAltC x y = hsep [ x <> colon, ppReturnC y ]
ppCaseC :: (Int, Doc) -> Doc
ppCaseC (x,y) = ppAltC (text "case" <+> int x) y
ppDefaultC :: Doc -> Doc
ppDefaultC = ppAltC (text "default")
ppSwitchC :: Doc -> [Doc] -> Doc -> Doc
ppSwitchC x ys z = ppBlockC (text "switch" <> parens x) $
  map ppCaseC (zip [0..] ys) ++ [ ppDefaultC z ]

ppAssignC x y = x <+> text "=" <+> y <> semi

ppVarDeclC x = ppc (typeof x) <+> ppc x

ppWhileC a bs c =
  vcat
    [ vcat [ t <+> ppAssignC p q | (t, (p, (q, _))) <- bs ]
    , ppBlockC (text "while" <> parens a) [ ppAssignC p r | (_, (p, (_, r))) <- bs ]
    , ppReturnC c
    ]
                                            
ppAppC :: (PPC a, Typed a, PP a) => Either Op (Defn a) -> [a] -> Doc
ppAppC x ys = ppReturnC $ case () of
  () | isBinop x -> b0 <+> pp x <+> b1
     | isCast x -> parens (ppc (typeof x)) <> b0
     | otherwise -> pp x <> parens (ppCommas bs)
  where
    bs = map ppc ys
    b0:_ = bs
    _:b1:_ = bs


ppCExpC :: (Free, CExp) -> Doc
ppCExpC (x, y) = ppBlockC (ppCExpSigC x) [ppc y]

ppCExpSigC x = text "static" <+> ppSigC x (pp x) (fbvars x)

ppSigC x y zs = ppc (typeof x) <+> y <> parens (ppCommas $ map ppVarDeclC zs)
ppDefnSigC x = ppSigC x (text $ did x) (map UVar $ dbvars x)

ppDefnDeclC x = ppDefnSigC x <> semi

ppDefnC :: Defn AExp -> Doc
ppDefnC x = case body x of
  Nothing -> empty
  Just a -> ppBlockC (ppDefnSigC x) [ppReturnC $ ppc a]

printC :: ([(Free, CExp)], [Defn AExp]) -> IO ()
printC (cexps, defns) = do
  print $ vcat $ map ppDefnDeclC defns ++ map ppCExpC cexps ++ map ppDefnC defns
