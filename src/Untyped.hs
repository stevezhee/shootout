{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

module Untyped where

import Control.Applicative hiding (empty)
import Control.Monad
import Control.Monad.State
import Data.Bits
import Data.Char
import Data.Foldable hiding (mapM_)
import Data.Graph hiding (Tree, Node)
import Data.Hashable
import Data.List (sortBy, sort, intersperse, (\\), nub, union, groupBy)
import Data.Maybe
import Data.Ratio
import Data.Traversable hiding (mapM)
import Data.Vector (Vector)
import Data.Word
import Debug.Trace
import GHC.Generics (Generic)
import Prelude hiding (foldr, foldr1, mapM, mapM_, sequence, elem, maximum, concat, empty)
import System.Process hiding (env)
import Text.PrettyPrint hiding (int, empty)
import qualified Data.HashMap.Strict as M
import qualified Data.Text.Lazy.IO as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Text.PrettyPrint as PP
import Data.Int
import Data.GraphViz

data Tree a = Node [Tree a] | Leaf a deriving (Show, Eq)

fromLeaf x = case x of
  Leaf a -> a
  Node{} -> unused "fromLeaf"
    
instance Foldable Tree where
  foldr f b x = case x of
    Leaf a -> f a b
    Node ns -> foldr (flip (foldr f)) b ns

instance Traversable Tree where
  traverse f x = case x of
    Leaf a -> Leaf <$> f a
    Node ns -> Node <$> traverse (traverse f) ns

instance Functor Tree where
  fmap f x = case x of
    Leaf a -> Leaf $ f a
    Node ns -> Node $ fmap (fmap f) ns
    
instance PP Int where pp = PP.int
instance PP a => PP [a] where pp = vcat . map pp
instance PP a => PP (Vector a) where pp xs = pp $ zip [0 :: Int ..] $ V.toList xs

instance (PP a, PP b) => PP (a,b) where pp (a,b) = parens (pp a <+> pp b)
instance (PP a, PP b, PP c) => PP (a,b,c) where
  pp (a,b,c) = parens (pp a <+> pp b <+> pp c)

data Type
  = TInt Bool Integer | TFloating Integer | TArray Integer Type | TWorld | TAgg
  deriving (Show, Eq, Ord, Generic)
instance Hashable Type
                         
data Lit
  = Rat Type Rational
  | Undef Type
  deriving (Show, Eq, Generic, Ord)
instance Hashable Lit

data UId = UId{ uid :: Integer, utype :: Type } deriving (Show, Eq, Ord, Generic)
instance Hashable UId

data FId = FId{ fid :: Integer, ftype :: Type }
          deriving (Show, Eq, Ord, Generic)
instance Hashable FId

data BId = BId{ bid :: Integer, btype :: Type } deriving (Show, Eq, Ord, Generic)
instance Hashable BId

data Var -- don't reorder
  = UVar UId
  | BVar BId
  | FVar FId
  deriving (Show, Eq, Generic, Ord)
instance Hashable Var

type AExp = Either Lit Var

data Defn a
  = Defn{ did :: String, dbvars :: [Var], dtype :: Type, body :: Maybe a }
  | Loop{ lid :: Maybe FId, dtype :: Type, lbody :: a }
  deriving (Show, Eq, Ord, Generic)
instance Hashable a => Hashable (Defn a)

data CExpr a
  = App (Defn a) [a]
  | If a a a
  | While a a -- 2nd argument is a Lam
  | Lam Integer [BId] [a]
  | From a BId
  -- | Phi a a
  deriving (Show, Eq, Ord, Generic)
instance Hashable a => Hashable (CExpr a)

-- BAL: beware when generating code for a while - need to ensure that
-- only the minimal computation is done pre-loop, on first iteration,
-- and on subsequent iterations.

type CExp = CExpr AExp
type Expr a = Either AExp (CExpr a)
newtype Exp = Exp{ unExp :: Expr Exp } deriving (Show, Eq, Ord)

maximumBV :: (Foldable t, Functor t) => t Exp -> Integer
maximumBV = maximum . fmap maxBV

maxBV :: Exp -> Integer
maxBV x = case unExp x of
  Left _ -> 0
  -- ^ not a typo
  -- see http://pchiusano.github.io/2014-06-20/simple-debruijn-alternative.html
  -- or http://www.cse.chalmers.se/~emax/documents/axelsson2013using.pdf
  Right e -> case e of
    Lam n _ _ -> n
    App (Loop _ _ b) _ -> maxBV b
    App _ bs -> maximumBV bs
    If a b c -> maximumBV [a,b,c]
    While _ b -> maxBV b
    From a _ -> maxBV a
    
zipWithTree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
  -- ^ trees must have the same shape
zipWithTree f x y =
  case (x,y) of
   (Leaf a, Leaf b) -> Leaf $ f a b
   (Node bs, Node cs) -> Node $ map (uncurry $ zipWithTree f) $ zip bs cs
   _ -> unused "zipWithTree"

-- listToTree :: [Tree a] -> Tree [a]
-- listToTree xs@(b:_) = foldr (zipWithTree (:)) (fmap (\_ -> []) b) xs

-- switch :: Exp -> [Tree Exp] -> Tree Exp -> Tree Exp
-- switch x ys z = fmap (\(a:bs) -> mkExp $ Switch x bs a) $ listToTree (z : ys)

zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree = zipWithTree (,)

if' x y z = fmap (\(a,b) -> mkExp $ If x a b) $ zipTree y z
-- if' x y z = fmap (\(a,b) -> let u = undef (etype a) in mkExp $ Phi (mkExp $ If x a u) (mkExp $ If x u b)) $ zipTree y z

while :: Tree Exp -> (Tree Exp -> (Exp, Tree Exp)) -> Tree Exp
while x f =
  fmap (mkExp . From (mkExp . App defn $ toList x)) xb
  where
    defn :: Defn Exp = Loop Nothing TAgg $ mkExp $ While e $ mkExp $ Lam (n+m) (toList xb) (toList x1)
    m = fromIntegral $ length x
    n = maximum [maximumBV x, maxBV e, maximumBV x1]
    (e, x1 :: Tree Exp) = f x0
    x0 :: Tree Exp
    x0 = fmap (Exp . bvar) xb
    xb :: Tree BId
    xb = snd $ mapAccumR (\(b:bs) j -> (bs, BId b (etype j))) [n..] x

mkExp = Exp . Right

var :: Var -> Exp
var = Exp . Left . Right

bvar :: BId -> Expr Exp
bvar = Left . Right . BVar
    
rat :: Type -> Rational -> Exp
rat t = Exp . Left . Left . Rat t

undef :: Type -> Exp
undef = Exp . Left . Left . Undef

defn :: String -> Type -> Maybe Exp -> Tree Exp -> Exp
defn s t x e = mkExp $ App (Defn s bvs t x) $ toList e
  where
    bvs = toList $ instantiate $ fmap etype e

instantiate :: Tree Type -> Tree Var
instantiate = snd . mapAccumL f 0
  where
    f x y = (succ x, UVar $ UId x y)

type F a = State (MapR Integer CExp) a

toAExpDefn :: Defn Exp -> F (Defn AExp)
toAExpDefn x = case x of
  Defn a bs c d -> Defn a bs c <$> mapM toAExp d
  Loop a t b -> do
    b'@(Right (FVar v)) <- toAExp b
    return $ Loop (Just v) t b'

toAExp :: Exp -> F AExp
toAExp x0 = do
  x <- toCExp x0
  case x of
    Left a -> return a
    Right e -> do
      tbl <- get
      let (a, tbl') = insertR e tbl
      modify $ \_ -> tbl'
      return $ Right $ FVar $ FId a (either atype ctype x)

toCExp :: Exp -> F (Either AExp CExp)
toCExp x = case unExp x of
  Left a -> return $ Left a
  Right e -> Right <$> case e of
    App a bs -> App <$> toAExpDefn a <*> mapM toAExp bs
    If a b c -> If <$> toAExp a <*> toAExp b <*> toAExp c
    From a b -> flip From b <$> toAExp a
    While a b -> While <$> toAExp a <*> toAExp b
    Lam a bs cs -> Lam a <$> return bs <*> mapM toAExp cs
    -- Phi a b -> Phi <$> toAExp a <*> toAExp b
    
-- pretty printing

class PP a where pp :: a -> Doc

instance PP Type where
  pp x = case x of
    TInt a b -> (text $ if a then "I" else "W") <> integer b
    TFloating a -> text "F" <> integer a
    TArray a b -> text "A" <+> integer a <+> pp b
    TWorld -> text "world"
    TAgg -> text "agg"
    
instance PP Double where pp = double
instance PP Lit where
  pp x = {- pp (ltype x) <+> -} case x of
    Rat _ b -> pp b
    Undef _ -> text "undef"

instance (PP a, PP b) => PP (Either a b) where pp = either pp pp

-- printPP :: ([(FId, CExp)], [Defn AExp]) -> IO ()
printPP (cexps, defns) = do
  print $ vcat $ map pp cexps ++ map pp defns
  
ppParens x = if (any isSpace $ show d) then parens d else d
  where d = pp x

ppStore x y = pp x <+> text ":=" <+> pp y
    
instance PP Exp where pp = pp . unExp

ppCommas = hcat . intersperse (text ", ")

instance PP Integer where pp = integer

instance PP (Ratio Integer) where
  pp x
    | denominator x == 1 = pp (numerator x)
    | otherwise = pp (fromRational x :: Double)

instance PP FId where pp x = {- pp (ftype x) <+> -} text (showFId $ fid x)
instance PP UId where pp x = {- pp (utype x) <+> -} text "u" <> pp (uid x)

showFId x = "f" ++ show x

instance PP BId where pp x = {- pp (btype x) <+> -} text "b" <> pp (bid x)

instance PP Var where
  pp x = case x of
    UVar a -> pp a
    BVar a -> pp a
    FVar a -> pp a

instance PP a => PP (Defn a) where
  pp x = case x of
    Defn{} -> hsep [ pp $ dtype x, text $ did x, ppParens $ dbvars x, text "=", ppDefnBody (body x) ]
    Loop{} -> pp $ lbody x

ppDefnBody :: PP a => Maybe a -> Doc
ppDefnBody = maybe (text "extern") pp

ppCall x ys = text x <> parens (commaSep ys)

commaSep = hcat . intersperse (text ",")
  
instance PP a => PP (CExpr a) where
  pp x = case x of
    If a b c -> ppCall "select" $ map pp [a,b,c]
--                                  vcat [text "if" <+> ppParens a, nest 2 $ pp [b,c]]
    App a@Defn{} bs -> ppCall (did a) $ map pp bs
      -- text (did a) <+> hsep (map ppParens bs)
    App (Loop (Just v) _ _) bs -> pp v <+> hsep (map ppParens bs)
    App a bs -> pp a <+> hsep (map ppParens bs)
    From a b -> text "from" <+> pp a <+> pp b
    While a b -> hsep [ text "while", pp a, pp b ]
    Lam _ bs cs -> hsep [ text "\\", ppParens bs, text "->", ppParens cs ]
    -- Phi a b -> ppCall "phi" $ map pp [a,b]
      
-- type information

ltype x = case x of
  Rat a _ -> a
  Undef a -> a

wtype = TInt False
itype = TInt True

booltype = wtype 1

vtype x = case x of
  UVar a -> utype a
  BVar a -> btype a
  FVar a -> ftype a

ctype :: CExp -> Type
ctype = cexprtype atype

cexprtype :: (a -> Type) -> CExpr a -> Type
cexprtype f x = case x of
  App a _ -> dtype a
  If _ _ c -> f c
  From _ b -> btype b
  While _ b -> f b
  Lam{} -> TAgg
  -- Phi a _ -> f a
  
etype :: Exp -> Type
etype = either atype (cexprtype etype) . unExp

atype :: AExp -> Type
atype = either ltype vtype

-- misc
swap (x,y) = (y,x)

singleton a = [a]

unused = error . (++) "unused:"
-- debug x = trace ("debug:" ++ show x) x

idIdx f x = V.unsafeIndex x . fromIntegral . f

fidIdx :: Vector a -> FId -> a
fidIdx = idIdx fid

data MapR a b = MapR
  { hmapR :: M.HashMap b a
  , next :: a
  } deriving Show

lookupR :: (Hashable b, Eq b) => b -> MapR a b -> Maybe a
lookupR b = M.lookup b . hmapR

insertR :: (Hashable b, Eq b, Enum a) => b -> MapR a b -> (a, MapR a b)
insertR b tbl = case lookupR b tbl of
  Just a -> (a, tbl)
  Nothing -> (a, tbl{ next = succ a, hmapR = M.insert b a $ hmapR tbl })
    where a = next tbl

runCExpMap :: [Defn Exp] -> ([Defn AExp], MapR Integer CExp)
runCExpMap = flip runState (MapR M.empty 0) . mapM toAExpDefn

-- compile :: [Def] -> ([(Defn CExp)], [Defn AExp])
compile xs = do
  let
    (defns0, m) = runCExpMap xs
    n = next m
    cexps0 :: [(Integer, CExp)] = sort $ map swap $ M.toList $ hmapR m
    -- bvs :: Vector [Var] = constructB argsCExp n cexps0
--    defns :: [Defn AExp] = map (updFBVarsDefn bvs) defns0
--    cexps :: [(FId, CExp)] = map (\(p, q) -> (toFId bvs p (ctype q), updFBVarsCExp bvs q)) cexps0
   -- defns1 = map (\(i, e) -> Defn (showFId i) (V.unsafeIndex bvs (fromIntegral i)) (ctype e) (Just e)) cexps0
    defns1 = map (\(i, e) -> Defn (showFId i) [] (ctype e) (Just e)) cexps0
    -- (gr, _) = graphFromEdges' $ ((), n, map fid $ catMaybes $ map asdf defns0) : map foo cexps0
--    (gr, g) = graphFromEdges' [ (1, 1, [2]), (4,4,[]), (3,3,[4]), (2,2,[3])]
    -- evalof x = x{ fid = fid x + n }
    cexps1 = map (\(a,b) -> (FId a $ ctype b, b)) cexps0
    -- evals = bar cexps1
    -- tbl = groupByFst $ concatMap depsCExp cexps1 ++ concatMap depsEvalWhen evals
    -- lookupF a = case lookup a tbl of
    --   Nothing -> []
    --   Just bs -> bs
    -- (gr, g) = graphFromEdges' $
    --   [ (pp booltype <+> ppAssign (ppEId a) (ppEvalWhen bs), eid a, lookupF $ eid a) | (a, bs) <- evals ] ++
    --   [ (pp (ftype a) <+> pp a <> semi <+> text "if" <+> parens (ppEId a) <+> braces (ppAssign (pp a) (pp b)), fid a, lookupF $ fid a) | (a, b) <- cexps1 ]
      -- [ (, eid a, depsEvalWhen bs) | (a, bs) <- bar cexps1 ] ++
--      [ (, fid a, eid a : depsCExp b) | (a,b) <- cexps1 ]
    ---  [ (text "if" <+> parens (pp $ FId (a+n) booltype) <+> braces (pp (FId a (ctype b)) <+> text "=" <+> pp b <> semi), a + n, [a]) | (a, b) <- cexps0 ] ++
   -- trace (show $ indegree gr)
      -- trace (show $ vcat $ map (\(a,bs) -> ppAssign (ppEId a) (ppEvalWhen bs)) evals) $
      -- trace (show $ vcat $ map (pp . (\(a,b) -> (ppEFId a, hcat $ map ppEFId b))) tbl) $
  -- print $ vcat $ fmap (fstOfTriple . g) $ topSort gr
  -- viz cexps0
  viz $ bar n cexps1 defns0
  return (defns1, defns0)

instance PP Doc where pp = id

viz :: ([(Integer, String)], [(Integer, Integer)]) -> IO ()
viz (bs, cs) = do
  let params = nonClusteredParams{ fmtNode = singleton . toLabel . snd }
  let s = printDotGraph $ graphElemsToDot params bs [ (a,b,()) | (a,b) <- cs ]
  T.writeFile "t.dot" s
  _ <- system "dot -Gordering=out -Tsvg t.dot > t.dot.svg"
  return ()

bar n xs ys = (ns ++ concatMap (nodes toBegin toEnd) xs, es ++ nub (concatMap (foo toBegin toEnd) xs))
  where
    toBegin = (+) n . fid
    toEnd = (+) (n*2) . fid
    toFun = (+) (n*3) . fid
    (ns,es) = unzip [ ((toFun v, did d), (toFun v, toBegin v)) | d@(Defn _ _ _ (Just (Right (FVar v)))) <- ys ]

nodes toBegin toEnd (x, y) =
  [ (toBegin x, "begin:" ++ showFId (fid x))
  , (fid x, show $ pp x <+> text ":" <+> pp y)
  , (toEnd x, "end:" ++ showFId (fid x))
  ]

foo toBegin toEnd (x, y) = case y of
  App _ bs -> case catMaybes $ map mFId bs of
    [] ->
      [ (toBegin x, fid x)
      , (fid x, toEnd x)
      ]
    bs' ->
      (fid x, toEnd x) :
      map ((toBegin x,) . toBegin) bs' ++
      map ((, fid x) . toEnd) bs'
  If a b c -> catMaybes
    [ fmap ((toBegin x,) . toBegin) $ mFId a
    , fmap ((, fid x) . toEnd) $ mFId a
    , fmap ((fid x, ) . toBegin) $ mFId b
    , fmap ((fid x, ) . toBegin) $ mFId c
    , fmap ((, toEnd x) . toEnd) $ mFId b
    , fmap ((, toEnd x) . toEnd) $ mFId c
    ]
    
mFId :: AExp -> Maybe FId
mFId x = case x of
  Right (FVar v) -> Just v
  _ -> Nothing

-- foo :: [(Integer, CExp)] -> [(Integer, Integer)]
-- foo = concatMap deps
--  where
--  deps (x, y) = catMaybes $ case y of
--   App _ bs -> map (f (, x)) bs
--   If a b c -> [ f (, x) a, f (x,) b, f (x,) c ]
--   where
--     f :: (Integer -> a) -> AExp -> Maybe a
--     f g a = case a of
--       Right (FVar v) -> Just $ g $ fid v
--       _ -> Nothing
        
-- ppAssign x y = x <+> text "=" <+> y <> semi

-- fstOfTriple (a, _, _) = a

-- ppEFId x = text $ if x >= 0 then ("f" ++ show x) else ("e" ++ show (abs (succ x)))
  
-- depsCExp :: (FId, CExp) -> [(Integer, Integer)]
-- depsCExp (x, y) = (eid x, fid x) : map (, fid x) $ nub $ catMaybes $ map f $ case y of
--   App _ bs -> bs
--   If a b c -> [a, b, c]
--   where
--     f a = case a of
--       Right (FVar v) -> Just $ fid v
--       _ -> Nothing
      
-- depsEvalWhen :: (FId, [EvalWhen]) -> [(Integer, Integer)]
-- depsEvalWhen (x, ys) = map (, eid x) (nub $ concatMap f ys)
--   where
--     f y = case y of
--       Always a -> [eid a]
--       IsTrueAnd a b -> [eid a, fid b]
--       IsFalseAnd a b -> [eid a, fid b]

-- eid :: FId -> Integer
-- eid x = negate $ succ $ fid x

-- ppEvalWhen :: [EvalWhen] -> Doc
-- ppEvalWhen xs = foldr (\a b -> f a <+> text "||" <+> b) (f $ head xs) $ tail xs
--   where
--     f x = case x of
--       Always a -> ppEId a
--       IsTrueAnd a b -> parens (ppEId a <+> text "&&" <+> pp b)
--       IsFalseAnd a b -> parens (ppEId a <+> text "&& !" <> pp b)
    
-- ppEId :: FId -> Doc
-- ppEId x = text ("e" ++ show (fid x))
  
-- bar :: [(FId, CExp)] -> [(FId, [EvalWhen])]
-- bar = groupByFst . concatMap foo
--   where
--      foo (x, y) = catMaybes $ case y of
--       App _ bs -> map (((, Always x) <$>) . f) bs
--       If a b c ->
--         [ (, Always x) <$> f a
--         , g IsTrueAnd b a
--         , g IsFalseAnd b a
--         ]
--       where
--         f a = case a of
--           Right (FVar v) -> Just v
--           _ -> Nothing
--         g h b a = case (f b, f a) of
--           (Just p, Just q) | fid q < fid p -> Just (p, h x q)
--           (Just p, Just q) -> Just (p, Always x)
--           _ -> Nothing
          
--         -- g a = case a of
--         --   Right (FVar v) | v < x -> Just v
--         --   _ -> Nothing

--         -- f :: (FId -> EvalWhen) -> AExp -> Maybe (FId, EvalWhen)
--         -- f g e = case e of
--         --   Right (FVar v) -> Just (v, g x)
--         --   _ -> Nothing

-- predDeps :: Pred -> [Integer]
-- predDeps = map fid . nub . concatMap (\(a,b) -> (a : maybe [] (either singleton singleton) b))
      
-- ppFoo :: Pred -> Doc
-- ppFoo (y:ys) = foldr (\a b -> a <+> text "||" <+> b) (f y) (map f ys)
--   where
--     f (a, mb) = maybe (pp a) (\b -> pp a <+> text "&&" <+> either pp ((text "!" <>) . pp) b) mb

-- instance PP a => PP (Maybe a) where pp = maybe PP.empty pp

-- type Pred = [(FId, Maybe (Either FId FId))]

groupByFst :: (Eq a, Ord a, Eq b) => [(a,b)] -> [(a, [b])]
groupByFst = map (\cs -> (fst $ head cs, map snd cs)) . groupBy (\a b -> fst a == fst b) . sortBy (\a b -> compare (fst a) (fst b)) . nub

-- data EvalWhen = Always FId | IsTrueAnd FId FId | IsFalseAnd FId FId deriving (Show, Eq)

-- bar :: Integer -> [(Integer, CExpr (Either Lit Var))] -> [(FId, Pred)]
-- bar n = groupByFst . concatMap (foo n)

-- foo n (x, y) = catMaybes $ map f $ case y of
--   If a b c ->
--     [ a `evaluated` always
--     , b `evaluated` whenTrue a
--     , c `evaluated` whenFalse a
--     ]
--   While a b ->
--     [ a `evaluated` always
--     , b `evaluated` whenTrue a
--     ]
--   From a _ -> [ a `evaluated` always ]
--   App _ bs -> map (`evaluated` always) bs
--   Lam _ _ cs -> map (`evaluated` always) cs
--   where
--     f (a, b) = case a of
--       Right (FVar v) -> Just (evalof v, b)
--       _ -> Nothing
--     evalof x = x{ fid = fid x + n }
--     i = FId x $ ctype y
--     always = (i, Nothing)
--     evaluated a (b,c) = (a, (evalof b, c))
--     whenTrue (Right (FVar a)) = (i, Just $ Left a)
--     whenFalse (Right (FVar a)) = (i, Just $ Right a)
    
-- asdf :: Defn AExp -> Maybe FId
-- asdf x = case x of
--   Defn _ _ _ (Just a) -> bar a
--   _ -> Nothing
  
-- foo :: (Integer, CExp) -> ((), Integer, [Integer])
-- foo (i,e) = ((), i, map fid $ blah e)

-- bar :: AExp -> Maybe FId
-- bar x = case x of
--   Right (FVar a) -> Just a
--   _ -> Nothing

-- blah :: CExp -> [FId]
-- blah x = nub $ case x of
--   App a bs -> case a of
--     Loop{} -> catMaybes [lid a] ++ f bs
--     Defn{} -> f $ catMaybes [body a] ++ bs
--   Switch a bs c -> f (a : c : bs)
--   From a _ -> f [a]
--   While a b -> f [a, b]
--   Lam _ _ cs -> f cs
--   where
--     f = catMaybes . map bar
    
-- toFId :: Vector [Var] -> Integer -> Type -> FId
-- toFId bvs i t = let v = FId i t $ fidIdx bvs v in v
    
-- updFBVarsAExp :: Vector [Var] -> AExp -> AExp
-- updFBVarsAExp bvs x = case x of
--   Right (FVar a) -> Right $ FVar a{ fbvars = fidIdx bvs a }
--   x -> x

-- updFBVarsCExp :: Vector [Var] -> CExp -> CExp
-- updFBVarsCExp bvs x = case x of
--   App a bs -> App (updFBVarsDefn bvs a) $ map f bs
--   Switch a bs c -> Switch (f a) (map f bs) (f c)
--   While a b cs d -> While a (f b) [ (p, (f q, f r)) | (p, (q, r)) <- cs ] d
--   where
--     f = updFBVarsAExp bvs
  
-- updFBVarsDefn :: Vector [Var] -> Defn AExp -> Defn AExp
-- updFBVarsDefn bvs x = x{ body = fmap (updFBVarsAExp bvs) $ body x }

-- argsAExp :: Vector [Var] -> AExp -> [Var]
-- argsAExp arr x = case x of
--   Right v -> case v of
--     FVar a -> fidIdx arr a
--     _ -> singleton v
--   Left{} -> []

-- argsCExp :: Vector [Var] -> CExp -> [Var]
-- argsCExp arr x = sort $ nub $ case x of
--   App _ bs -> go bs
--   Switch a bs c -> go (a : c : bs)
--   From _ b -> go [b]
--   While _ bs c ds -> go (c : ds) \\ map BVar bs
--   where
--     go = foldr1 union . map (argsAExp arr)

-- constructB :: (Vector a -> b -> a) -> Integer -> [(Integer, b)] -> Vector a
-- constructB f n xs = V.create $ do
--   arr <- VM.new $ fromIntegral n
--   iarr <- V.unsafeFreeze arr
--   mapM_ (\(i,b) -> VM.unsafeWrite arr (fromIntegral i) (f iarr b)) xs
--   return arr
