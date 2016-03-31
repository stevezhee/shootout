{-# OPTIONS_GHC -Wall -fno-warn-name-shadowing -fno-warn-missing-signatures #-}
-- {-# LANGUAGE IncoherentInstances #-}
-- {-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric          #-}
-- {-# LANGUAGE FlexibleInstances      #-}
-- {-# LANGUAGE FunctionalDependencies #-}
-- {-# LANGUAGE MultiParamTypeClasses  #-}
-- {-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE ScopedTypeVariables    #-}
-- {-# LANGUAGE FlexibleContexts #-}


module Main where

-- import           Data.Bits              hiding (xor)
-- import qualified Data.Bits              as B
import           Prelude                hiding (concat, foldr, maximum)
-- import           Data.Bits hiding (xor)
-- import           Data.Hashable          (Hashable)
-- import qualified Data.Hashable          as H
import           Data.List (intersperse)
import           Data.Map.Strict               (Map)
import qualified Data.Map.Strict               as M
-- import           Data.Maybe
import qualified Data.Set as S
import           Data.Set               (Set)
-- import           Control.Monad.Identity
import           Control.Monad.State
import           Data.Word
-- import qualified Prelude                as P
-- import Data.Graph hiding (Tree, Node)
-- import           T
-- -- import           Prelude (fromIntegral, undefined, Either(..), ($), unwords, fmap, (.), (++), String, error, Bool(..), show, concat, unzip3, fst, Float, Int, fromEnum, print, Integer, Show, Num)
-- import           Prelude hiding ((==), (/=), (>), (<), (>=), (<=), (+), (-), (*), (/), (%), (^), abs, fromInteger, fromRational, pi, sqrt, negate, sum, succ, pred)
-- import           Debug.Trace
-- instance TyCmp Int Bool where cmpRec = cmpRecA
-- class TyCmp a b | a -> b where cmpRec :: CmpRec a b
-- instance Rep r => TyCmp (r Int) (r Bool) where cmpRec = cmpRecR
-- import           Control.Applicative
import           Data.Foldable
-- import           Data.Functor
-- import           Data.Traversable
import           GHC.Generics           (Generic)
import Numeric
import Data.Char

type M a = StateT St IO a

data Ptr a = Ptr{ unPtr :: Int }
  deriving (Show, Generic, Eq, Ord)

data Val a = Lit a | BVar String Int | FVar String Int deriving Show

tyVal :: Ty a => Val a -> String
tyVal (_ :: Val a) = ty (unused "ty (Val a)" :: a)

ppBVar i = "%v" ++ show i
ppFVar i = "@g" ++ show i

commaSep = concat . intersperse ", "

prim_ x ys = hcat x $ commaSep ys

output :: String -> M ()
output x = output0 $ indent 2 x

output0 = lift . putStrLn

prim :: String -> [String] -> M ()
prim x = output . prim_ x

class PP a where ppu :: a -> String
instance PP RectT where ppu _ = "somerect"
instance PP SurfaceT where ppu _ = "somesurface"
instance PP TextureT where ppu _ = "sometexture"
instance PP WindowT where ppu _ = "somewindow"
instance PP RendererT where ppu _ = "somerenderer"
instance PP Bool where
  ppu x = case x of
    True -> "true"
    False -> "false"
    
instance PP Char where ppu = ppu . fromEnum
instance PP Int where ppu = show
instance PP Word8 where ppu = show

instance Ty a => PP (Ptr a) where ppu = ppu . unPtr -- show in hex

instance Ty a => PP (Val a) where
  ppu x = case x of
            BVar _ i -> ppBVar i
            FVar _ i -> ppFVar i
            Lit a -> ppu a

pp x = hcat (tyVal x) $ ppu x

store :: Ty a => Val a -> Val (Ptr a) -> M ()
store x y = prim "store" [pp x, pp y]

fresh :: M Int
fresh = do
  i <- gets next
  modify $ \st -> st{ next = succ i }
  return i

parens xs = "(" ++ commaSep xs ++ ")"

alloca :: Ty a => M (Val (Ptr a))
alloca = assign $ \v -> prim_ "alloca" [tyVal v]

load :: Ty a => Val (Ptr a) -> M (Val a)
load x = assign $ \_ -> prim_ "load" [pp x]

assign :: Ty a => (Val a -> String) -> M (Val a)
assign (f :: Val a -> String) = do
  i <- fresh
  let v :: Val a = BVar (tyVal v) i
  output $ unwords [ppu v, "=", f v]
  return v

add :: Ty a => Val a -> Val a -> M (Val a)
add x y = assign $ \_ -> prim_ "add" [pp x, ppu y]
  
data Label = Label{ unLabel :: Int } deriving Show
  
-- foo = proc "foo" $ \(a,b) -> do
--   store (int 4) a
--   v <- load a
--   r <- add b v
--   store b a
--   store r a

-- bar = proc "bar" $ \(a, b) -> do
--   c <- alloca
--   call foo (b, a)
--   call foo (c, a)
--   inline foo (c, a)

data St = St
  { next :: Int
  , strings :: Map String (Val (Ptr (Ptr Char)))
  , decls :: Set String
  } deriving Show
  
exec = flip execStateT $ St 0 M.empty S.empty

declString :: (String, Val (Ptr (Ptr Char))) -> IO ()
declString (x, y) = do
  putStrLn $ unwords [ n, "= private unnamed_addr constant", t, escString x ++ ", align 1" ]
  putStrLn $ unwords [ ppu y, "= global i8* getelementptr inbounds (" ++ t ++ "* " ++ n ++ ", i32 0, i32 0), align 4" ]
  where
    n = ppu y ++ ".str"
    t = tyString x
    
main :: IO ()
main = do
  st <- exec $ do
    define main'
  mapM_ declString $ M.toList $ strings st
  mapM_ putStrLn $ decls st
  -- define bar
--   define "foo" $ \(a,b) -> do
--     label $ Label 0
--     x <- alloca
--     store (int 4) x
--     store a x
--     store a b
--     v <- load x
--     switch v (Label 0) [(0, Label 1), (1, Label 2), (14, Label 3)]
--     switch v (Label 0) [(0, Label 1), (1, Label 2), (14, Label 3)]
--     br $ Label 0
--     call "foo" (x, v)
--     ret

inline :: Agg a => Proc a b -> a -> M b
inline (Proc n mf) x = maybe (error $ "unable to inline ffi call:" ++ n) (\f -> f x) mf
  
data Proc a b = Proc String (Maybe (a -> M b))

proc :: (Agg a, Ret b) => String -> (a -> M b) -> Proc a b
proc x = Proc x . Just

foo :: Proc () IntT
foo = proc_ "main" $ do
  s <- str "hello, world."
  puts s
  ret $ int 0

proc_ :: Ret b => String -> M b -> Proc () b
proc_ x m = proc x (\() -> m)
  
define :: (Agg a, Ret b) => Proc a b -> M ()
define (Proc n Nothing) = error $ "unable to define ffi call:" ++ n
define ((Proc n (Just f)) :: Proc a b) = do
  a <- instantiate
  output0 $ "define " ++ tyRet (unused "define" :: b) ++ " @" ++ n ++ args a  
  output0 "{"
  _ <- f a
  output0 "}"

hcat x y = unwords [x, y]

type P a = Val (Ptr a)

withResource :: (Agg a, Ty b) =>
  (a -> M (P b)) -> (P b -> M ()) -> String -> a -> (P b -> M ()) -> M ()
withResource create destroy s x use =
  withPtr (create x) (err $ "creating " ++ s) $ \p -> do
    use p
    destroy p

type CString = P Char

type Rect = ((IntT, IntT), (IntT, IntT))

data WindowT = WindowT{ unWindowT :: Int }

instance Ty WindowT where tyRec = tyRecInt WindowT unWindowT

data RendererT = RendererT{ unRendererT :: Int }
instance Ty RendererT where tyRec = tyRecInt RendererT unRendererT

data TextureT = TextureT{ unTextureT :: Int }
instance Ty TextureT where tyRec = tyRecInt TextureT unTextureT

data SurfaceT = SurfaceT{ unSurfaceT :: Int }
instance Ty SurfaceT where tyRec = tyRecInt SurfaceT unSurfaceT

data RectT
instance Ty RectT

type IntT = Val Int
type Window = P WindowT
type Renderer = P RendererT
type Texture = P TextureT
type Surface = P SurfaceT

type Word8T = Val Word8

withWindow :: (CString, Rect, IntT) -> (Window -> M ()) -> M ()
withWindow = withResource sdlCreateWindow sdlDestroyWindow "window"
withWindowSurface :: Window -> (Surface -> M ()) -> M ()
withWindowSurface = withResource sdlGetWindowSurface sdlFreeSurface "window surface"
withRenderer :: (Window, IntT, IntT) -> (Renderer -> M ()) -> M ()
withRenderer = withResource sdlCreateRenderer sdlDestroyRenderer "renderer"

sdlCreateWindow :: (CString, Rect, IntT) -> M Window
sdlCreateWindow = ffi "SDL_CreateWindow"

sdlGetWindowSurface :: Window -> M Surface
sdlGetWindowSurface = ffi "SDL_GetWindowSurface"

sdlDestroyWindow :: Window -> M ()
sdlDestroyWindow = ffi "SDL_DestroyWindow"

sdlFreeSurface :: Surface -> M ()
sdlFreeSurface = ffi "SDL_FreeSurface"

sdlCreateRenderer :: (Window, IntT, IntT) -> M Renderer
sdlCreateRenderer = ffi "SDL_CreateRenderer"

sdlDestroyRenderer :: Renderer -> M ()
sdlDestroyRenderer = ffi "SDL_DestroyRenderer"

sdlCreateTextureFromSurface :: (Renderer, Surface) -> M Texture
sdlCreateTextureFromSurface = ffi "SDL_CreateTextureFromSurface"

sdlDestroyTexture :: Texture -> M ()
sdlDestroyTexture = ffi "SDL_DestroyTexture"

sdlLoadBMP :: CString -> M Surface
sdlLoadBMP = ffi "SDL_LoadBMP"

class Ret b where
  call :: Agg a => Proc a b -> a -> M b
  ret :: b -> M b
  tyRet :: b -> String

declare :: (Agg a, Ret b) => String -> a -> b -> M ()
declare n a v = modify $ \st -> st{ decls = S.insert s $ decls st }
  where
    s = "declare " ++ tyRet v ++ " @" ++ n ++ tyargs a

instance Ty a => Ret (Val a) where
  call (Proc x _) y = assign $ \v -> prim_ "call" [ tyVal v ++ " @" ++ x ++ args y ]
  ret x = do
    output $ "ret " ++ pp x
    return x
  tyRet = tyVal
  
instance Ret () where
  call (Proc x _) y = prim "call" [ "void @" ++ x ++ args y ]
  ret () = output "ret void"
  tyRet _ = "void"
  
ffi :: (Agg a, Ret b) => String -> a -> M b
ffi x y = do
  b <- call (Proc x Nothing) y
  declare x y b
  return b

puts :: CString -> M ()
puts = ffi "puts"

err :: String -> M ()
err x = str x >>= puts

sdl_renderer_accelerated :: IntT
sdl_renderer_accelerated = int 0x00000002

tyString s = ("[" ++ show (length s + 1) ++ " x i8]")

escString :: String -> String
escString s = "c\"" ++ concatMap escChar (s ++ "\0") ++ "\""

escChar :: Char -> [Char]
escChar c
  | c == '"' || c < ' ' || c > '~' = "\\" ++ map toUpper a'
  | otherwise = [c]
  where
    a = showHex (fromEnum c) ""
    a' = case a of
      [_] -> "0" ++ a
      _ -> a
    
str :: String -> M CString
str s = do
  tbl <- gets strings
  p <- case M.lookup s tbl of
    Just v -> return v
    Nothing -> do
      i <- fresh
      let v = FVar "i8**" i
      modify $ \st -> st{ strings = M.insert s v tbl }
      return v
  load p
  
if' :: Val Bool -> M () -> M () -> M ()
if' x y z = do
  istrue <- freshLabel
  isfalse <- freshLabel
  done <- freshLabel
  switch x istrue [(False, isfalse)]
  label istrue
  y
  br done
  label isfalse
  z
  br done
  label done

phi :: Ret b => [(b, Label)] -> M b
phi = undefined

sdl_init_timer :: IntT
sdl_init_timer = int 0x00000001
sdl_init_audio :: IntT
sdl_init_audio = int 0x00000010
sdl_init_video :: IntT
sdl_init_video = int 0x00000020
sdl_init_events :: IntT
sdl_init_events = int 0x00004000

eq :: Ty a => Val a -> Val a -> M (Val Bool)
eq x y = assign $ \_ -> prim_ "icmp eq" [pp x, ppu y]

instance Agg () where
 unAgg (Node []) = ()
 unAgg _ = unused "unAgg"
 agg () = Node []
 instantiate = return ()

sdlInit :: IntT -> M ()
sdlInit = ffi "SDL_Init"

sdlQuit :: M ()
sdlQuit = ffi "SDL_Quit" ()

sdlRenderPresent :: Renderer -> M ()
sdlRenderPresent = ffi "SDL_RenderPresent"
sdlRenderCopy :: (Renderer, Texture, P RectT, P RectT) -> M ()
sdlRenderCopy = ffi "SDL_RenderCopy"
sdlRenderClear :: Renderer -> M ()
sdlRenderClear = ffi "SDL_RenderClear"
sdlSetRendererDrawColor :: (Renderer, (Word8T, Word8T, Word8T, Word8T)) -> M ()
sdlSetRendererDrawColor = ffi "SDL_SetRendererDrawColor"

withBMPTexture :: (Renderer, CString) -> (Texture -> M ()) -> M ()
withBMPTexture (rndr, n) f = do
  bmp <- sdlLoadBMP n
  r <- bmp `eq` nullptr
  if' r (err "BMP surface") $ do
    tex <- sdlCreateTextureFromSurface(rndr, bmp)
    sdlFreeSurface bmp
    r <- tex `eq` nullptr
    if' r (err "BMP texture") $ do
      f tex
      sdlDestroyTexture tex

withSDL :: IntT -> M a -> M a
withSDL x m = do
  sdlInit x
  a <- m
  sdlQuit
  return a

main' :: Proc () IntT
main' = proc_ "main" $ withSDL sdl_init_video $ do
  s <- str "Hello World"
  withWindow (s, ((int 100, int 100), (int 640, int 480)), int 0) $ \win ->
    withWindowSurface win $ \dest ->
    withRenderer (win, int (-1), sdl_renderer_accelerated) $ \rndr -> do
      sdlSetRendererDrawColor (rndr, (w8 0xff, w8 0xff, w8 0xff, w8 0xff))
      s <- str "ship.bmp"
      withBMPTexture (rndr, s) $ \ship -> do
        sdlRenderClear rndr
        sdlRenderCopy (rndr, ship, nullptr, nullptr)
        sdlRenderPresent rndr
  ret $ int 0
  
nullptr :: Ty a => Val (Ptr a)
nullptr = Lit $ Ptr 0

withPtr :: Ty a => M (Val (Ptr a)) -> M () -> (Val (Ptr a) -> M ()) -> M ()
withPtr m n f = do
  p <- m
  r <- p `eq` nullptr
  if' r n (f p)
  
ppVal :: UVal -> String
ppVal x = case x of
  BVar t i -> hcat t $ ppBVar i
  FVar t i -> hcat t $ ppFVar i
  Lit a -> case a of
    LInt a -> ppt a
    LBool a -> ppt a
    LPtr t a -> hcat t $ show a -- show in hex
    LWord8 a -> ppt a
    LChar a -> ppt a

ppTyVal :: UVal -> String
ppTyVal x = case x of
  BVar t _ -> t
  FVar t _ -> t
  Lit a -> case a of
    LInt a -> ty a
    LBool a -> ty a
    LPtr t _ -> t
    LChar a -> ty a
    LWord8 a -> ty a
    
args :: Agg a => a -> String
args x = parens (map ppVal $ toList $ agg x)

tyargs :: Agg a => a -> String
tyargs x = parens (map ppTyVal $ toList $ agg x)

int :: Int -> IntT
int = Lit

w8 :: Word8 -> Word8T
w8 = Lit

indent n = (++) (replicate n ' ')
brackets xs = "[\n" ++ unlines (map (indent 4) xs) ++ indent 4 "]"

br :: Label -> M ()
br x = prim "br" [ppLabel x]
  
ppULabel = (++) "L" . show . unLabel
ppLabel x = hcat "label" $ "%" ++ ppULabel x

freshLabel :: M Label
freshLabel = Label <$> fresh
  
label :: Label -> M ()
label x = output0 (ppLabel x ++ ":")
  
switch :: Ty a => Val a -> Label -> [(a, Label)] -> M ()
switch x y zs =
  prim "switch" [ pp x, hcat (ppLabel y) $ brackets $ map f zs ]
  where
    f (a,b) = commaSep [ ppt a, ppLabel b ]

ppt a = hcat (ty a) $ ppu a

data TyRec a = TyRec
  { _ty :: a -> String
  , _toULit :: a -> ULit
  , _unULit :: ULit -> a
  }

class PP a => Ty a where
  ty :: a -> String
  toULit :: a -> ULit
  unULit :: ULit -> a
  tyRec :: TyRec a
  ty = _ty tyRec
  toULit = _toULit tyRec
  unULit = _unULit tyRec
  tyRec = TyRec
    { _ty = ty
    , _toULit = toULit
    , _unULit = unULit
    }
    
tyRecInt :: (Int -> a) -> (a -> Int) -> TyRec a
tyRecInt f g = TyRec
  { _ty = const "i32"
  , _toULit = LInt . g
  , _unULit = f . unLInt
  }

instance Ty Int where tyRec = tyRecInt id id

instance Ty Word8 where
  ty _ = "i8"
  toULit = LWord8
  unULit = unLWord8
  
instance Ty Bool where
  ty _ = "i1"
  toULit = LBool
  unULit = unLBool
  
instance Ty Char where
  ty _ = "i8"
  toULit = LChar
  unULit = unLChar
  
instance Ty a => Ty (Ptr a) where
  ty (_ :: Ptr a) = ty (unused "ppPtr" :: a) ++ "*"
  toULit x@(Ptr a) = LPtr (ty x) a
  unULit (LPtr _ a) = Ptr a
  unULit _ = unused "unULit"

unused s = error $ "unused:" ++ s

data Tree a = Node [Tree a] | Leaf a deriving (Show, Eq, Generic)

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

type UVal = Val ULit

class Agg a where
  unAgg :: Tree UVal -> a
  unAgg _ = unused "unAgg"
  agg :: a -> Tree UVal
  instantiate :: M a
  
instance Ty a => Agg (Val a) where
  unAgg (Leaf a) = unUVal a
  unAgg _ = unused "unAgg"
  agg = Leaf . toUVal
  instantiate = f $ unused "instantiate"
    where
      f :: a -> M (Val a)
      f a = BVar (ty a) <$> fresh
    
instance (Agg a, Agg b) => Agg (a, b) where
  unAgg (Node [a,b]) = (unAgg a, unAgg b)
  unAgg _ = unused "unAgg"
  agg (a, b) = Node [agg a, agg b]
  instantiate = (,) <$> instantiate <*> instantiate
 
instance (Agg a, Agg b, Agg c) => Agg (a, b, c) where
  unAgg (Node [a,b,c]) = (unAgg a, unAgg b, unAgg c)
  unAgg _ = unused "unAgg"
  agg (a, b, c) = Node [agg a, agg b, agg c]
  instantiate = (,,) <$> instantiate <*> instantiate <*> instantiate
 
instance (Agg a, Agg b, Agg c, Agg d) => Agg (a, b, c, d) where
  unAgg (Node [a,b,c,d]) = (unAgg a, unAgg b, unAgg c, unAgg d)
  unAgg _ = unused "unAgg"
  agg (a, b, c, d) = Node [agg a, agg b, agg c, agg d]
  instantiate = (,,,) <$> instantiate <*> instantiate <*> instantiate <*> instantiate
 
unUVal :: Ty a => UVal -> Val a
unUVal x = case x of
  BVar t i -> BVar t i
  FVar t i -> FVar t i
  Lit a -> Lit $ unULit a

toUVal :: Ty a => Val a -> Val ULit
toUVal x = case x of
  BVar t i -> BVar t i
  FVar t i -> FVar t i
  Lit a -> Lit $ toULit a

data ULit
  = LInt{ unLInt :: Int }
  | LWord8{ unLWord8 :: Word8 }
  | LBool{ unLBool :: Bool }
  | LChar{ unLChar :: Char }
  | LPtr String Int
  deriving (Show, Generic, Eq, Ord)

{-
instance Ty Lit where
  ty x = "wtf"
  -- case x of
  --   LInt a -> ty a
  --   LBool a -> ty a
    
instance PP Lit where
  ppu x = case x of
    LInt a -> ppu a
    LBool a -> ppu a
-}

{-

label :: Label -> M ()
label = undefined

ret :: M ()
ret = undefined

goto :: Label -> M ()
goto = undefined

switch :: TyInt a => Val a -> Label -> [Label] -> M ()
switch = undefined

proc :: String -> [Val U] -> M ()
proc = undefined
     
call :: String -> [Val U] -> M ()
call = undefined

-- TyArith Int, Float, or Vector
add :: TyArith a => Val a -> Val a -> M (Val a)
add = undefined
sub :: TyArith a => Val a -> Val a -> M (Val a)
sub = undefined
mul :: TyArith a => Val a -> Val a -> M (Val a)
mul = undefined
div :: TyArith a => Val a -> Val a -> M (Val a)
div = undefined
rem :: TyArith a => Val a -> Val a -> M (Val a)
rem = undefined

-- TyBits Int or Vector
shl :: TyBits a => Val a -> Val a -> M (Val a)
shl = undefined
lshr :: TyBits a => Val a -> Val a -> M (Val a)
lshr = undefined
ashr :: TyBits a => Val a -> Val a -> M (Val a)
ashr = undefined
and :: TyBits a => Val a -> Val a -> M (Val a)
and = undefined
or :: TyBits a => Val a -> Val a -> M (Val a)
or = undefined
xor :: TyBits a => Val a -> Val a -> M (Val a)
xor = undefined

extractelement :: (Ty a, TyInt a) => Val (Vector n a) -> Val n b -> Val a
extractelement = undefined

insertelement :: Val (Vector n a) -> Val a -> Val n b -> Val (Vector n a)
insertelement = undefined

shufflevector :: Val (Vector n a) -> Val (Vector n a) -> Val (Vector m Int) -> Val (Vector m a)
shufflevector = undefined

gt :: Val a -> Val a -> M (Val Bool)
gt = undefined

class TyInt a
-}

{-
fact :: E Int -> E Int
fact n = snd $ while (n, lit 1) $ \(a,b) -> (a `gt` lit 0, (a `sub` lit 1, a `mul` b))
-}

{-
instance TyNum Int
instance TyCmp Int

add x y = do
  a <- eval x
  b <- eval y
  return $ call "add" [a,b]

if' x f g = do
  v <- eval x
  lD <- label
  lT <- label
  lF <- label
  call "if" [v, lT, lF]
  call "label" [lT]
  f
  call "goto" [lD]
  call "label" [lF]
  g
  call "goto" [lD]
  call "label" [lD]

while es f g = do
  lP <- label
  lB <- label
  lD <- label
  vs <- mapM eval es
  call "goto" [lP]
  call "label" [lP]
  p <- eval $ f vs
  call "if" [p, lB, lD]
  call "label" [lB]
  vs1 <- mapM eval $ g vs
  sequence_ [ assign v v1 | (v,v1) <- zip vs vs1 ]
  call "goto" [lP]
  call "label" [lD]
  
mkEdge :: (Hash, UCExp) -> ((Hash, UCExp), Hash, [Hash])
mkEdge x@(a, Op _ bs) = (x, a, [ i | BVar i <- bs ] )

foo :: (Hash, UCExp) -> Map Hash Pred -> Map Hash Pred
foo (x, y) = \tbl ->
  let p = ulookup x tbl in
  case y of
    Op "if" [a,b,c] ->
      evalWhen p a $
      evalWhen (p `pAnd` IsTrue a) b $
      evalWhen (p `pAnd` pNot (IsTrue a)) c tbl
    Op _ bs -> foldr ($) tbl (map (evalWhen p) bs)

bar :: AExp a -> [(Hash, UCExp)] -> Map Hash Pred
bar (BVar i) xs = foldr foo tbl xs where tbl = M.fromList $ [ (a, PFalse) | (a,_) <- xs ] ++ [(i, PTrue)]
bar _ _ = M.empty

topSortCExps x = [ d | (d, _, _) <- map g $ topSort gr ]
  where
    (gr, g) = graphFromEdges' $ fmap mkEdge $ M.toList $ cexps x

ppFoo x y p = case y of
  Op "if" [a,b,c] -> case p' of
    PTrue -> ppAssign (x, b)
    PFalse -> ppAssign (x, c)
    _ -> "if(" ++ pp p' ++ ") { " ++ ppAssign (x, b) ++ " } else { " ++ ppAssign (x, c) ++ " }"
    where p' = p `pAnd` IsTrue a
  Op{} -> case p of
    PTrue -> ppAssign (x, y)
    PFalse -> "// always false: " ++ ppAssign (x, y)
    _ -> "if(" ++ pp p ++ ") { " ++ ppAssign (x, y) ++ " }"

main = do
  -- mapM_ print ds
--  mapM_ print $ sort $ concatMap foo $ M.toList $ cexps x
  let ds = sort $ M.toList $ cexps x
  let tbl = bar (aexp x) ds
  mapM_ putStrLn [ ppFoo a b $ ulookup a tbl | (a,b) <- ds ]
  putStrLn $ "return " ++ pp (aexp x)
  -- putStrLn $ pp x
  where
    x = renameE $ if' c ((a `sub` lit 1) `mod'` b) (if' c (a `div'` b) (a `mul` a))
    a = add (lit 42) (lit 7) :: E Int
    b = mul (lit 24) (lit 5) :: E Int
    c = a `gt` lit 0

pNot (PNot a) = a
pNot PFalse = PTrue
pNot PTrue = PFalse
pNot (PAnd a b) = pOr (pNot a) (pNot b)
pNot (POr a b) = pAnd (pNot a) (pNot b)
pNot a = PNot a

data Pred =
  PFalse | PTrue | PNot Pred | IsTrue UAExp | PAnd Pred Pred | POr Pred Pred
  deriving (Show, Eq)

instance PP Pred where
  pp x = case x of
    PFalse -> "false"
    PTrue -> "true"
    IsTrue a -> pp a
    PNot a -> "not " ++ pp a
    PAnd a b -> "(" ++ pp a ++ " && " ++ pp b ++ ")"
    POr a b -> "(" ++ pp a ++ " || " ++ pp b ++ ")"
    
ulookup k tbl = fromMaybe (error ("unknown key:" ++ show tbl)) $ M.lookup k tbl

pAnd _ PFalse = PFalse
pAnd a PTrue = a
pAnd PFalse _ = PFalse
pAnd PTrue b = b
pAnd a b | a == b = a
pAnd (PNot a) b | a == b = PFalse
pAnd a (PNot b) | a == b = PFalse
pAnd a b = PAnd a b

pOr a PFalse = a
pOr PFalse b = b
pOr _ PTrue = PTrue
pOr PTrue _ = PTrue
pOr a b = POr a b

evalWhen p (BVar i) tbl = M.adjust (pOr p) i tbl
evalWhen _ _ tbl = tbl
    
instance PP Bool where pp = show . fromEnum
instance Ty Bool where
  ty _ = "i1"
  toULit = LBool
  fromULit (LBool a) = a

toUAExp :: Ty a => AExp a -> UAExp
toUAExp x = case x of
  BVar a -> BVar a
  Lit a -> Lit $ toULit a

fromUAExp :: Ty a => UAExp -> AExp a
fromUAExp x = case x of
  BVar a -> BVar a
  Lit a -> Lit $ fromULit a

toUE :: Ty a => E a -> UE
toUE x = x{ aexp = toUAExp $ aexp x }

fromUE :: Ty a => UE -> E a
fromUE x = x{ aexp = fromUAExp $ aexp x }

debug :: Show a => a -> b -> b
debug x = trace (show x)

ppAssign (k, v) = unwords [ ppBVar k, "=", pp v ]

instance Ty a => PP (E a) where
  pp x = unlines
    [ pp $ aexp x
    , unlines $ map ppAssign $ sort $ M.toList $ cexps x
    ]

ppBVar = (++) "%v" . show

instance PP ULit where
  pp x = case x of
    LInt a -> pp a
    LWord a -> pp a
    LBool a -> pp a

indent x ys = unlines (x : map ("  " ++) ys)

instance PP UCExp where
  pp x = case x of
    Op a bs -> unwords (a : map pp bs)

instance PP a => PP (AExp a) where
  pp x = case x of
    Lit a -> pp a
    BVar a -> ppBVar a

uaexp :: Ty a => E a -> UAExp
uaexp = toUAExp . aexp

maxBVAgg :: Agg a => a -> Word
maxBVAgg = maximum . fmap maxBV . unAgg

uaexpAgg :: Agg a => a -> Tree UAExp
uaexpAgg = fmap aexp . unAgg

cexpsAgg :: Agg a => a -> Map Hash UCExp
cexpsAgg = foldr M.union M.empty . fmap cexps . unAgg

zipTree :: Tree a -> Tree b -> Tree (a,b)
zipTree x y = case (x, y) of
  (Leaf a, Leaf b) -> Leaf (a,b)
  (Node bs, Node cs) -> Node $ fmap (uncurry zipTree) $ zip bs cs

-- mkSubstTbl :: Tree BVar -> Tree BVar -> Map BVar BVar
-- mkSubstTbl x y = foldr (\(a,b) -> M.insert a b) M.empty $ zipTree x y

-- subst :: Map BVar BVar -> BVar -> BVar
-- subst tbl k = fromMaybe k $ M.lookup k tbl

assert :: String -> Bool -> a -> a
assert s a b = case a of
  True -> b
  False -> error ("assert failed:" ++ s)

type BVar = Word

renameE :: Ty a => E a -> E a
renameE x = fromUE $ rename f a
  where
    a = toUE x
    tbl = M.fromList $ zip (fmap fst $ reverse $ topSortCExps a) [0 .. ]
    f = subst tbl
  
rename :: (Word -> Word) -> UE -> UE
rename f e = e{ cexps = M.mapKeys f $ fmap h $ cexps e, aexp = g $ aexp e }
  where
    h :: UCExp -> UCExp
    h (Op a bs) = Op a $ fmap g bs
    g a = case a of
      BVar v -> BVar $ f v
      _ -> a
    
subst tbl k = fromMaybe k $ M.lookup k tbl

{-
while :: Agg a => a -> (a -> (E Bool, a)) -> a
while x f =
  -- agg $ fmap (mapUE (subst $ mkSubstTbl (fmap aexp ubvs) $ fmap aexp t)) t
  agg $ fmap (\u -> u{ cexps = M.union (cexpsAgg x) cexps' }) a
  where
    (t,e) = mkWhile x f
    a = fmap mkPhi t
    tbl = M.fromList [ (k, aexp v) | ((k,_), v) <- toList $ zipTree t a ]
    cexps' = foldr M.union (cexps $ rename (subst tbl) e) $ fmap cexps a
    mkPhi (_,(b,c)) =
      e{ cexps = M.singleton k v
       , aexp = HVar k
       }
      where
        v = Op "phi" [aexp e, b, c]
        k = hash v

mkWhile :: (Agg a) => a -> (a -> (E Bool, a)) -> (Tree (UAExp, (UAExp, UAExp)), UE)
mkWhile x f =
  ( t
  , E{ maxBV = max (maxBVAgg x) i
     , cexps = M.insert k v $ M.unions [ cexps p, cexpsAgg x' ]
     , aexp = HVar k
     }
  )
  where
    t = zipTree (uaexpAgg bvs) $ zipTree (uaexpAgg x) (uaexpAgg x')
    k = hash v
    v = Op "while" $ uaexp p : concat [ [a,b,c] | (a,(b,c)) <- toList t ]
    n = max (maxBV p) (maxBVAgg x')
    (i, bvs) = inst n x
    (p, x') = f bvs
-}

if' :: Agg a => E Bool -> a -> a -> a
if' x y z = agg $ fmap (mkIf x) $ zipTree (unAgg y) (unAgg z)

mkIf :: E Bool -> (UE, UE) -> UE
mkIf x (y,z) = E
  { maxBV = maximum [maxBV x, maxBV y, maxBV z]
  , cexps = M.insert k v $ M.unions [cexps x, cexps y, cexps z]
  , aexp = BVar k
  }
  where
    v = Op "if" [uaexp x, aexp y, aexp z]
    k = hash v

sub :: TyNum a => E a -> E a -> E a
sub = binop (-) $ wrapFlags "sub"

add :: TyNum a => E a -> E a -> E a
add = binop (-) $ wrapFlags "add"

gt :: TyCmp a => E a -> E a -> E Bool
gt = binop (>) ["gt"] -- ["icmp", "sgt"]

mul :: TyNum a => E a -> E a -> E a
mul = binop (*) $ wrapFlags "mul"

div' :: TyNum a => E a -> E a -> E a
div' = binop (*) $ wrapFlags "div"

mod' :: TyNum a => E a -> E a -> E a
mod' = binop (*) $ wrapFlags "mod"

binop :: (Ty a, Ty b, Ty c) => (a -> b -> c) -> [String] -> E a -> E b -> E c
binop f ss x y = case (aexp x, aexp y) of
  -- (Lit a, Lit b) -> lit $ f a b -- BAL: put back in
  _ -> E
    { maxBV = maximum [maxBV x, maxBV y]
    , cexps = M.insert k v $ M.unions [cexps x, cexps y]
    , aexp = BVar k
    }
  where
    v = Op (unwords ss) [uaexp x, uaexp y]
    k = hash v

bvar :: Ty a => Word -> E a
bvar = E 0 M.empty . BVar

lit :: Ty a => a -> E a
lit = E 0 M.empty . Lit

class Agg a where
  agg :: Tree UE -> a
  unAgg :: a -> Tree UE
  inst :: Word -> a -> (Word, a)

instance Ty a => Agg (E a) where
  agg (Leaf a) = fromUE a
  unAgg = Leaf . toUE
  inst i _ = (succ i, bvar i)

instance (Agg a, Agg b) => Agg (a,b) where
  agg (Node [a,b]) = (agg a, agg b)
  unAgg (a,b) = Node [unAgg a, unAgg b]
  inst i (a, b) = let (j, a') = inst i a in let (k, b') = inst j b in (k, (a', b'))

-- --  inst :: Word -> a -> Tree b
--   unAgg :: a -> Tree b
--   agg :: Tree b -> a
--   -- uninst :: Tree Word -> a
--   -- mkR :: Tree Word -> a
--   -- maxBVAgg :: a -> Word

--   inst :: u -> Word -> a -> (Word, a)

-- instance Rep r u => Agg (r a) u where
--   agg (Leaf x) = toA x
--   unAgg = Leaf . fromA
--   inst (_ :: u) i _ = (succ i, (bvar :: Rep r u => Word -> r a) i)

-- instance (Agg a u, Agg b u) => Agg (a, b) u where
--   agg (Node [a, b]) = (agg a, agg b)
--   unAgg (a, b) = Node [unAgg a, unAgg b]
-- --

-- class Rep r u | u -> r where
--   toA :: u -> r a
--   fromA :: r a -> u
--   bvar :: Word -> r a

instance PP Int where pp = show
instance PP Word where pp = show
instance Ty Int where
  ty _ = "i32"
  toULit = LInt
  fromULit (LInt a) = a

data ULit
  = LInt Int
  | LWord Word
  | LBool Bool
  deriving (Show, Generic, Eq, Ord)
instance Hashable ULit

data AExp a = Lit a | BVar Word deriving (Show, Generic, Eq, Ord)
instance Hashable a => Hashable (AExp a)

type UAExp = AExp ULit

data UCExp = Op String [UAExp] deriving (Show, Generic, Eq, Ord)
instance Hashable UCExp

type Hash = Word

type UE = E ULit
data E a = E
  { maxBV :: Word
  , cexps :: Map Hash UCExp
  , aexp  :: AExp a
  } deriving Show

-- data Eval = Eval Hash | EvalAnd Hash Hash | EvalAndNot Hash Hash deriving (Show, Eq, Ord)

-- foo :: (Hash, UCExp) -> [(Hash, Eval)]
-- foo (x,y) = catMaybes $ case y of
--   Op "if" [a@(HVar i),b,c] -> [ f a $ Eval x, f b $ EvalAnd x i, f c $ EvalAndNot x i ]
--   Op _ bs -> map (flip f $ Eval x) bs
--   where
--     f (HVar i) e = Just (i, e)
--     f _ _ = Nothing
  
hash :: UCExp -> Hash
hash = fromIntegral . H.hash

class (TyCmp a, Num a) => TyNum a
class (Ty a, Ord a) => TyCmp a

data Tree a = Node [Tree a] | Leaf a deriving (Show, Eq, Generic)
instance Hashable a => Hashable (Tree a)

class PP a where pp :: a -> String
class PP a => Ty a where
  ty :: a -> String
  toULit :: a -> ULit
  fromULit :: ULit -> a
-}

-- fooBinop :: (Ty a, Ty b, Ty c) => (a -> b -> c) -> E a -> E b -> E c
-- fooBinop f x y = case (unLit x, unLit y) of
--   (Just a, Just b) -> lit $ f a b

--                       -- undefined -- Exp $ max (maxBV x) (maxBV y)

-- lengthAgg :: Agg a => a -> Word
-- lengthAgg = snd . inst 0

-- instance (Ty a) => Agg (E a) where
--   mkR (Leaf i) = undefined -- Exp i
--   uninst (Leaf i) = bvar i
--   inst i _ = (Leaf i, i + 1)
-- --  maxBVAgg = maxBV

-- instance (Agg a, Agg b) => Agg (a, b) where
--   mkR (Node [a, b]) = (mkR a, mkR b)
--   uninst (Node [a,b]) = (uninst a, uninst b)
--   inst i (x, y) = let (a, j) = inst i x in let (b, k) = inst j y in (Node [a, b], k)
--   maxBVAgg (x, y) = max (maxBVAgg x) (maxBVAgg y)

-- class Rep r where
--   lit :: Ty a => a -> r a
--   bvar :: Ty a => Word -> r a
--   if' :: Ty a => r Bool -> r a -> r a -> r a
--   while :: Agg a => a -> (a -> (r Bool, a)) -> a
--   add :: TyNum a => r a -> r a -> r a
--   sub :: TyNum a => r a -> r a -> r a
--   mul :: TyNum a => r a -> r a -> r a
--   gt :: TyCmp a => r a -> r a -> r Bool

-- debug :: Show a => a -> b -> b
-- debug = trace . show

-- instance Rep E where
  -- lit = Exp 0 M.empty . Lit . toLit
  -- bvar = Exp 0 M.empty . BVar -- not a typo
  -- if' x y z = Exp (maximum [maxBV x, maxBV y, maxBV z]) (insert k v $ unions [cexps x, cexps y, cexps z]) $ HVar k
  --   where
  --     k = hash v
  --     v = If (aexp x) (aexp y) (aexp z)
  -- while (x :: a) f = mkR $ fmap (\_ -> max (maxBVAgg x) i) t
  --   where
  --     n :: Word = max (maxBV p) (maxBVAgg a')
  --     (t :: Tree Word, i :: Word) = inst n x
  --     a :: a = uninst t
  --     (p :: Exp Bool, a' :: a) = f a
  -- while x f = While x a p a'
  --   where
  --     n = max (maxBV p) (maxBVAgg a')
  --     (a, i) = inst n x
  --     (p, a') = f a

  -- add = fooBinop (+)
  -- sub = fooBinop (-)
  -- mul = fooBinop (*)
  -- gt = fooBinop (>)

-- fact2 :: E Int -> E Int
-- fact2 n = snd $ while (n, lit 1) $ \(a,b) -> (a `gt` lit 0, (a `sub` lit 1, if' (a `gt` lit 10) (fact (a `mul` b)) (a `sub` b)))

  ------
-- while :: r a -> (r a -> (r Bool, r a)) -> r a
-- while2 :: (r a, r b) -> ((r a, r b) -> (r Bool, (r a, r b))) -> (r a, r b)

-- class Rep r where
--   lit :: a -> r a

-- class Agg a where
--   while :: Rep r => a -> (a -> (r Bool, a)) -> a
--   fun :: Rep r => String -> (a -> r b) -> a -> r b


--   while :: Rep r => a -> (a -> (r Bool, a)) -> a

-- instance Rep r u => Agg (r a)
-- instance Rep r => Agg (r a, r b)

-- fromInteger :: (Rep r, Num a) => Integer -> r a
-- fromInteger = lit . P.fromInteger

-- foo :: (Rep r, Num a, Ord a) => r a -> r [a]
-- foo x = if' (1 + x > 3) (stream 2) (stream 3)

-- stream :: r a -> r [a]
-- stream = undefined

-- bar :: (Rep r, Num a, Ord a) => r a
-- bar = foo 1

-- class Rep r where
--   lit :: a -> r a
--   if' :: r Bool -> r a -> r a -> r a
--   while :: r a -> r b -> (r a -> r b -> r Bool) -> (r a -> r b -> r a) -> (r a -> r b -> r b) -> r b
--   while2 :: r a -> r b -> r c -> (r a -> r b -> r c -> r Bool) -> (r a -> r b -> r c -> r a) -> (r a -> r b -> r c -> r b) -> (r a -> r b -> r c -> r c) -> r c
--   (+) :: Num a => r a -> r a -> r a
--   (-) :: Num a => r a -> r a -> r a
--   (*) :: Num a => r a -> r a -> r a
--   (>) :: Ord a => r a -> r a -> r Bool
--   (<) :: Ord a => r a -> r a -> r Bool

-- binopI :: (a -> b -> c) -> Identity a -> Identity b -> Identity c
-- binopI f x y = f <$> x <*> y

-- fact n = while n 1 (\a _ -> a > 1) (\a _ -> a - 1) (*)

-- fib n = while2 n 0 1 (\a _ _ -> a > 0) (\a _ _ -> a - 1) (\_ _ c -> c) (\_ b c -> b + c)

-- fact3 :: Rep r => r Int
-- fact3 = fact 5

-- fib3 :: [Identity Int]
-- fib3 = map fib [1,2,3,4,5,6,7]

-- instance Rep Identity where
--   lit = Identity
--   if' x y z = case runIdentity x of
--     True -> y
--     False -> z
--   while x y f g h = if' (f x y) (while (g x y) (h x y) f g h) y
--   while2 x y z f g h i = if' (f x y z) (while2 (g x y z) (h x y z) (i x y z) f g h i) z
--   (+) = binopI (P.+)
--   (-) = binopI (P.-)
--   (*) = binopI (P.*)
--   (>) = binopI (P.>)
--   (<) = binopI (P.<)

-- data CmpRec a b = CmpRec
--   { _if :: b -> a -> a -> a
--   , _eq :: a -> a -> b
--   , _ne :: a -> a -> b
--   , _gt :: a -> a -> b
--   , _ge :: a -> a -> b
--   , _lt :: a -> a -> b
--   , _le :: a -> a -> b
--   }

-- class TyCmp a where


-- ifThenElse :: TyCmp a b => b -> a -> a -> a
-- ifThenElse = _if cmpRec

-- (==) :: TyCmp a b => a -> a -> b
-- (==) = _eq cmpRec
-- (/=) :: TyCmp a b => a -> a -> b
-- (/=) = _ne cmpRec
-- (>) :: TyCmp a b => a -> a -> b
-- (>) = _gt cmpRec
-- (<) :: TyCmp a b => a -> a -> b
-- (<) = _lt cmpRec
-- (>=) :: TyCmp a b => a -> a -> b
-- (>=) = _ge cmpRec
-- (<=) :: TyCmp a b => a -> a -> b
-- (<=) = _le cmpRec

-- cmpRecR :: (Rep r) => CmpRec (r a) (r Bool)
-- cmpRecR = CmpRec
--   { _if = if'
--   , _eq = eq
--   , _ne = ne
--   , _gt = gt
--   , _ge = ge
--   , _lt = lt
--   , _le = le
--   }

-- cmpRecA :: (Eq a, Ord a) => CmpRec a Bool
-- cmpRecA = CmpRec
--   { _if = \a b c -> case a of
--                      True -> b
--                      False -> c
--   , _eq = (P.==)
--   , _ne = (P./=)
--   , _gt = (P.>)
--   , _ge = (P.>=)
--   , _lt = (P.<)
--   , _le = (P.<=)
--   }

{-

bitsRecA :: (Bits a, Integral a) => BitsRec a
bitsRecA = BitsRec
  { _shl = \a b -> shiftL a (fromIntegral b)
  , _shr = \a b -> shiftL a (fromIntegral b)
  , _and = (.&.)
  , _or = (.|.)
  , _xor = B.xor
  }

bitsRecR :: (Rep r) => BitsRec (r a)
bitsRecR = BitsRec
  { _shl = shl
  , _shr = shr
  , _and = and
  , _or = or
  , _xor = xor
  }

data BitsRec a = BitsRec
  { _shl :: a -> a -> a
  , _shr :: a -> a -> a
  , _and :: a -> a -> a
  , _or :: a -> a -> a
  , _xor :: a -> a -> a
  }
fromInteger :: TyNum a => Integer -> a
fromInteger = _fromInteger numRec

class TyNum a where numRec :: NumRec a

numRecI :: (Num a, Integral a) => NumRec a
numRecI = NumRec
  { _fromInteger = P.fromInteger
  , _add = (P.+)
  , _sub = (P.-)
  , _mul = (P.*)
  , _div = P.div
  , _rem = P.rem
  }

numRecF :: (Num a, Fractional a) => NumRec a
numRecF = NumRec
  { _fromInteger = P.fromInteger
  , _add = (P.+)
  , _sub = (P.-)
  , _mul = (P.*)
  , _div = (P./)
  , _rem = undefined
  }

instance TyNum Int where numRec = numRecI
instance TyBits Int where bitsRec = bitsRecA

class Rep r where
  lit :: a -> r a
  add :: r a -> r a -> r a
  sub :: r a -> r a -> r a
  mul :: r a -> r a -> r a
  div :: r a -> r a -> r a
  rem :: r a -> r a -> r a
  if' :: r Bool -> r a -> r a -> r a
  eq :: r a -> r a -> r Bool
  ne :: r a -> r a -> r Bool
  gt :: r a -> r a -> r Bool
  ge :: r a -> r a -> r Bool
  lt :: r a -> r a -> r Bool
  le :: r a -> r a -> r Bool
  shl :: r a -> r a -> r a
  shr :: r a -> r a -> r a
  and :: r a -> r a -> r a
  or :: r a -> r a -> r a
  xor :: r a -> r a -> r a

class TyNum a => TyBits a where bitsRec :: BitsRec a

numRecR :: (Rep r, TyNum a) => NumRec (r a)
numRecR = NumRec
  { _fromInteger = lit . fromInteger
  , _add = add
  , _sub = sub
  , _mul = mul
  , _div = div
  , _rem = rem
  }

(+) :: TyNum a => a -> a -> a
(+) = _add numRec

(-) :: TyNum a => a -> a -> a
(-) = _sub numRec

(*) :: TyNum a => a -> a -> a
(*) = _mul numRec

(/) :: TyNum a => a -> a -> a
(/) = _div numRec

(%) :: TyNum a => a -> a -> a
(%) = _rem numRec

instance Rep r => TyNum (r Int) where numRec = numRecR
instance Rep r => TyBits (r Int) where bitsRec = bitsRecR

newtype MaxBV a = MaxBV{ unMaxBV :: Int } deriving Show

maximumBV = MaxBV . maximum

binopMaxBV x y = maximumBV [unMaxBV x, unMaxBV y]

instance Rep MaxBV where
  lit _ = MaxBV 0
  add = binopMaxBV
  sub = binopMaxBV
  mul = binopMaxBV
  div = binopMaxBV
  rem = binopMaxBV
  if' x y z = maximumBV [unMaxBV x, unMaxBV y, unMaxBV z]
  eq = binopMaxBV
  ne = binopMaxBV
  gt = binopMaxBV
  ge = binopMaxBV
  lt = binopMaxBV
  le = binopMaxBV
  shl = binopMaxBV
  shr = binopMaxBV
  and = binopMaxBV
  or = binopMaxBV
  xor = binopMaxBV
-}
-- stream :: TyCmp a b => a -> (a -> b) -> (a -> a) -> [a]
-- stream a f g = if f a then [a] else [] --  : stream (g a) f g) else []

-- fold :: b -> (a -> b -> b) -> [a] -> b
-- fold b f xs = case xs of
--   [] -> b
--   (x:xs') -> fold (f x b) f xs'

-- fact :: Int -> Int
-- fact n = fold 1 (*) $ stream n ((<) 1) ((+) 1)

{-
(.<<) :: TyBits a => a -> a -> a
(.<<) = _shl bitsRec
(.>>) :: TyBits a => a -> a -> a
(.>>) = _shr bitsRec
(.&) :: TyBits a => a -> a -> a
(.&) = _and bitsRec
(.|) :: TyBits a => a -> a -> a
(.|) = _or bitsRec
(.^) :: TyBits a => a -> a -> a
(.^) = _xor bitsRec

t :: (TyCmp a b, TyBits a) => a -> a
t x = if ((1 + x) / 3) > 2 then x else x .<< 3

tt :: (TyCmp a b, TyBits a) => a
tt = t 3

-}

-- instance PP Word where pp = show
-- instance Ty Word where ty _ = "i32"
-- instance PP Float where pp = show
-- instance Ty Float where ty _ = "float"
-- instance TyCmp Int where cmpRec = cmpRecSInt
-- instance TyNum Int where arithRec = arithRecSInt
-- instance TyCmp Word where cmpRec = cmpRecUInt
-- instance TyNum Word where arithRec = arithRecUInt

-- true :: Rep r => r Bool
-- true = lit True

-- false :: Rep r => r Bool
-- false = lit False

-- fromInteger :: (Rep r, TyNum a) => Integer -> r a
-- fromInteger = lit . P.fromInteger

-- foo :: (Rep r, TyNum a) => r a -> r a
-- foo x = if' (x < 0) (negate x) x

-- bar :: (Rep r, TyBits a) => r a -> r a
-- bar x = if' (x < 0) (negate x) (x .<< 2)

-- negate :: (Rep r, TyNum a) => r a -> r a
-- negate = (-) 0

--   -- fold :: r (Stream a) -> r b -> (r a -> r b -> r b) -> r b
--   -- stream :: r a -> (r a -> r Bool) -> (r a -> r a) -> r (Stream a)

-- newtype MaxBV a = MaxBV{ unMaxBV :: Word } deriving Show

-- -- maximumBV :: [MaxBV a] -> MaxBV a
-- -- maximumBV = P.maximum . fmap unMaxBV

-- instance Rep MaxBV where
--   lit _ = MaxBV undefined
--   bvar _ = MaxBV undefined
--   if' x y z = MaxBV $ P.maximum [ unMaxBV x, unMaxBV y, unMaxBV z]
--   while _ _ f g h = MaxBV $ undefined P.+ n
--     where
--       p = f a
--       a1 = g a
--       b1 = h a b
--       a = bvar n
--       b = bvar (P.succ n)
--       n = P.maximum [ unMaxBV p, unMaxBV a1, unMaxBV b1 ]
--   (+) = maxBVBinop
--   (-) = maxBVBinop
--   (*) = maxBVBinop
--   (/) = maxBVBinop
--   (%) = maxBVBinop
--   (.<<) = maxBVBinop
--   (.>>) = maxBVBinop
--   (.|) = maxBVBinop
--   (.&) = maxBVBinop
--   (.^) = maxBVBinop
--   (==) = maxBVBinop
--   (/=) = maxBVBinop
--   (>) = maxBVBinop
--   (<) = maxBVBinop
--   (>=) = maxBVBinop
--   (<=) = maxBVBinop

-- maxBVBinop :: MaxBV a -> MaxBV b -> MaxBV c
-- maxBVBinop x y = MaxBV $ P.maximum [unMaxBV x, unMaxBV y]

-- fact n = while n 1 (\a -> a > 1) (\a -> a - 1) (*)

-- type Stmt = String
-- newtype Name = Name{ unName :: Word }
--   deriving (P.Ord, P.Eq)

-- hash :: H.Hashable a => a -> Name
-- hash = Name . fromIntegral . H.hash

-- data Node = Node
--   { ppNode :: String
--   , deps :: S.Set Name
--   }

-- newtype LLVM a = LLVM{ unLLVM :: Either (Name, Map Name Node) a }

-- instance P.Eq a => P.Eq (LLVM a) where
--   (==) x y = case (unLLVM x, unLLVM y) of
--     (Left a, Left b) -> fst a P.== fst b
--     (Right a, Right b) -> a P.== b
--     _ -> False

-- instance PP Name where pp = (++) "%v" . show . unName

-- ssa :: PP a => LLVM a -> (String, Maybe Name, Map Name Node)
-- ssa x = case unLLVM x of
--   Left (a, b) -> (pp a, Just a, b)
--   Right a -> (pp a, Nothing, empty)

-- unused :: a
-- unused = error "unused"

-- binop :: (Ty a, Ty b, Ty c) => (a -> b -> c) -> [String] -> LLVM a -> LLVM b -> LLVM c
-- binop (f :: a -> b -> c) ss x y = case (unLLVM x, unLLVM y) of
--   (Right a, Right b) -> lit $ f a b
--   _ -> stmt [ssa x, ssa y] $
--        \[ppa, ppb] -> commaSep [ss ++ [ty (unused :: c), ppa ], [ppb] ]

wrapFlags x = [x] -- "nuw nsw"
fastMathFlags x = [x] -- ""

-- commaSep = concat . intersperse ", " . fmap unwords

-- stmt xs f = LLVM $ Left (v, insert v (Node d (S.fromList $ catMaybes ns)) $ unions ms)
--   where
--     d = f pps
--     v = hash d
--     (pps, ns, ms) = unzip3 xs

-- succ :: (Rep r, TyNum a) => r a -> r a
-- succ = (+) 1

-- bitsRecSInt :: (TyBits a) => BitsRec a
-- bitsRecSInt = BitsRec
--   { _shl = binop (\a b -> shiftL a (fromIntegral b)) ["shl", wrapFlags]
--   , _shr = binop (\a b -> shiftR a (fromIntegral b)) ["ashr", wrapFlags]
--   , _and = binop (.&.) ["and"]
--   , _or = binop (.|.) ["or"]
--   , _xor = binop Data.Bits.xor ["xor"]
--   }

-- bitsRecUInt :: (TyBits a) => BitsRec a
-- bitsRecUInt = bitsRecSInt
--   { _shr = binop (\a b -> shiftR a (fromIntegral b)) ["lshr", wrapFlags]
--   }

-- arithRecSInt :: (TyNum a, P.Integral a) => ArithRec a
-- arithRecSInt = ArithRec
--   { _add = binop (P.+) ["add", wrapFlags]
--   , _sub = binop (P.-) ["sub", wrapFlags]
--   , _mul = binop (P.*) ["mul", wrapFlags]
--   , _div = binop P.div ["sdiv"]
--   , _rem = binop P.rem ["srem"]
--   }

-- cmpRecSInt :: TyCmp a => CmpRec a
-- cmpRecSInt = CmpRec
--   { _eq = binop (P.==) ["icmp", "eq"]
--   , _ne = binop (P./=) ["icmp", "ne"]
--   , _gt = binop (P.>) ["icmp", "sgt"]
--   , _ge = binop (P.>=) ["icmp", "sge"]
--   , _lt = binop (P.<) ["icmp", "slt"]
--   , _le = binop (P.<=) ["icmp", "sle"]
--   }

-- cmpRecUInt :: TyCmp a => CmpRec a
-- cmpRecUInt = cmpRecSInt
--   { _gt = binop (P.>) ["icmp", "ugt"]
--   , _ge = binop (P.>=) ["icmp", "uge"]
--   , _lt = binop (P.<) ["icmp", "ult"]
--   , _le = binop (P.<=) ["icmp", "ule"]
--   }

-- cmpRecFloat :: TyCmp a => CmpRec a
-- cmpRecFloat = CmpRec
--   { _eq = binop (P.==) ["fcmp", "ueq"]
--   , _ne = binop (P./=) ["fcmp", "une"]
--   , _gt = binop (P.>) ["fcmp", "ugt"]
--   , _ge = binop (P.>=) ["fcmp", "uge"]
--   , _lt = binop (P.<) ["fcmp", "ult"]
--   , _le = binop (P.<=) ["fcmp", "ule"]
--   }

-- arithRecUInt :: (TyNum a, P.Integral a) => ArithRec a
-- arithRecUInt = arithRecSInt
--   { _div = binop P.div ["udiv"]
--   , _rem = binop P.rem ["urem"]
--   }

-- arithRecFloat :: (TyNum a, P.Fractional a) => ArithRec a
-- arithRecFloat = ArithRec
--   { _add = binop (P.+) ["fadd", fastMathFlags]
--   , _sub = binop (P.-) ["fsub", fastMathFlags]
--   , _mul = binop (P.*) ["fmul", fastMathFlags]
--   , _div = binop (P./) ["fdiv", fastMathFlags]
--   , _rem = binop undefined ["frem", fastMathFlags]
--   }

-- instance Rep LLVM where
--   lit = LLVM . Right
--   if' x (y :: LLVM a) z = case unLLVM x of
--     Right True -> y
--     Right False -> z
--     _ | y P.== z -> y
--     _ -> stmt [ssa x, ssa y, ssa z] $ \[ppa, ppb, ppc] ->
--            commaSep [ ["select i1", ppa], [t, ppb], [t, ppc] ]
--     where
--       t = ty (unused :: a)

--   (+) = _add arithRec
--   (-) = _sub arithRec
--   (*) = _sub arithRec
--   (/) = _sub arithRec
--   (%) = _sub arithRec

--   (.<<) = _shl bitsRec
--   (.>>) = _shr bitsRec
--   (.|) = _or bitsRec
--   (.&) = _and bitsRec
--   (.^) = _xor bitsRec

--   (==) = _eq cmpRec
--   (/=) = _ne cmpRec
--   (>) = _gt cmpRec
--   (<) = _lt cmpRec
--   (>=) = _ge cmpRec
--   (<=) = _le cmpRec

{-
data Cov a = Cov (SBV a) [Symbolic ()]

mkCov x = Cov x []

if' :: SymWord a => Cov Bool -> Cov a -> Cov a -> Cov a
if' (Cov a bs) (Cov c ds) (Cov e fs) =4
  Cov (ite a c e) $
  bs ++ g a ds ++ g (bnot a) fs
  where
    g x ys = if null ys then [ constrain x ] else [ constrain x >> y | y <- ys ]

instance (SymWord a, Num a) => Num (Cov a) where
  fromInteger = mkCov . fromInteger
  (*) = binop (*)
  (+) = binop (+)
  abs = unop abs
  signum = unop signum
  negate = unop negate
  (-) = binop (-)

unop :: (SBV a -> SBV b) -> Cov a -> Cov b
unop f (Cov a bs) = Cov (f a) bs

binop :: (SBV a -> SBV b -> SBV c) -> Cov a -> Cov b -> Cov c
binop f (Cov a bs) (Cov c ds) = Cov (f a c) $ bs ++ ds

(>.) :: SymWord a => Cov a -> Cov a -> Cov Bool
(>.) = binop (.>)
(==.) :: SymWord a => Cov a -> Cov a -> Cov Bool
(==.) = binop (.==)

foo = func $ \(x :: Cov Word32) (y :: Cov Word32) ->
  if' (x >. 5) (if' (x >. 10) 42 5) ((if' (x >. 6) x 13) * (if' (y ==. 0) y 12))

mIO :: IO a -> IO (Maybe a)
mIO m = catch (Just <$> m) $ \(_ :: SomeException) -> return Nothing

var :: SymWord a => String -> Symbolic (Cov a)
var s = mkCov <$> free s

func :: (SymWord a, SymWord b) => (Cov a -> Cov b -> Cov c) -> IO [SatResult]
func f = loop [] 0
  where
    loop rs i = do
      mr <- mIO $ sat $ do
        x <- var "x"
        y <- var "y"
        let Cov _ cs = f x y
        case drop i cs of
          [] -> error "func"
          (a:_) -> a >> return (true :: SBool)
      case mr of
       Just r -> loop (r:rs) $ succ i
       Nothing -> return rs
-}

{-
module Main where

import Shoot
import Fannkuch
import NBody
import Spectral

factorialF = func "factorialF" $ \(i :: Int') -> factorial i
foo = func "foo" $ \(x :: Word', y :: Int') -> switch x [y] 42

main = do
  a <- compile
    -- [ defIO $ proc "foo" $ \() -> do
    --      puti 42
    --      puti 27
    --      puti 39
    -- [ def $ func "foo" $ \() -> 42 :: Int' -- BAL: incorrect
    -- [ def $ func "foo" $ \x -> x + 42 :: Int'
    -- [ def $ func "foo" $ \(x,y) -> x + y :: Int'
    -- [ def $ func "foo" $ \x -> switch x [] x :: Word'
    -- [ def $ func "foo" $ \x -> switch x [42] x :: Word'
    -- [ def $ func "foo" $ \(x,y) -> switch x [42,y] x :: Word'
    [
      -- def $ func "foo" $ \(x :: Word') -> while 0 $ \i -> (i < x, succ i)
      -- def $ func "foo" $ \(x :: Word', y) -> fastpow x y
      -- def $ func "foo" $ \(x :: Word') -> spctMain C1
      -- def $ func "foo" $ \(x :: Word') -> negate x -- the wrong definition of negate will make this loop
      -- def $ func "foo" $ \(x :: Word') -> reps 3 x (+ 1)
      --defIO $ proc "add1" $ \(x,y) -> puti (x + y)
      -- def $ func "asdf" $ \(x :: Int', y :: Int') -> x + y
--      defIO $ proc "asdf" puti
--      defIO $ \x -> puti (1 + x)
--      def $ func "foo" $ \(x :: Int') -> dbl (x + 1)
--      def $ func "foo" $ \(x :: Int') -> if' (x > 3) x (x + 1)
--      def $ func "foo" $ \(x :: Int') -> if' (x > 3) (1 + (x + 1)) (x + 1)
--      def factorialF
      -- defIO $ proc "factoriala" $ \x -> puti $ factorialF x,
      -- defIO $ proc "factorial" (puti . factorialF)
      -- defIO fannkuchredux
      -- def $ func "foo" $ \(x :: Word') -> switch (x + 1) [x - 1, (x / 3) + dbl (x - 1), x - 1, 2 * x] (dbl ( x * (3 + (x / 3) + (x + 1))))
      -- def $ func "foo" $ \(x :: Word') -> if' (x > 0) (if' (x < 10) (x + 1) (x + 3)) (x + 42)
      -- def $ func "foo" $ \(x :: Word') -> if' (x > 0) (if' (x < 10) (x + 1) (x + 3)) (if' (x + 1 > 42) ((x + 1) + (x + 42)) (x + 42))
      -- def $ func "foo" $ \(x :: Word') -> if' (x < 10) (if' (x < 5) (x + 1) x) (if' ((x+1) < 42) (x + 1) x)
      -- def $ func "foo" $ \(x :: Word') -> if' (x < 10) (x + 1) (x + 2)
      -- def $ func "foo" $ \(x :: Word') -> if' (x + 1 < 10) (x + 1) (x + 2)
      -- def $ func "foo" $ \(x :: Word') -> let y = x + 1 in y + y + y
      def $ func "foo" $ \(x :: Word') -> if' (3 + (x + 1) > 10) (2 + (x + 1)) (x + 2)

      -- def $ func "foo" $ \(x :: Word') -> if' (x > 0) (if' (x < 10) (x + 1) x) (if' (x + 1 > 42) (x + 1) x)
      -- def $ func "foo" $ \(x :: Bool', y :: Bool') -> if' x (if' x (not' x) x) (if' (not' x) (not' x) x)
      -- defIO nbody
      -- def $ func "foo" $ \(x :: Word') -> dbl $ fst $ snd $ while (x, (3,2 :: Int')) $ \(a, (b,c)) -> (a > 0, (pred a, (b - c, c + b)))
      -- def $ func "foo" $ \(x :: Word') -> let (a,b) = snd $ while (x, (3,2 :: Int')) $ \(a, (b,c)) -> (a > 0, (pred a, (b - 42, c + 36))) in a + b
      -- def $ func "foo" $ \(x :: Word') -> snd (while (x, (3 :: Int')) $ \(a, b) -> (a > 0, (pred a, b - 42))) + snd (while (x, 2 :: Int') $ \(a, c) -> (a > 0, (pred a, c + 36)))
      -- def $ func "foo" $ \(x :: Word') -> reps x x succ + reps x 3 pred
      -- def $ func "foo" $ \(x :: Word') -> let (a,b) = reps x (x,3) (\(a,b) -> (succ a, pred b)) in a + b
      -- def $ func "spectral" $ \(x :: Word') -> spctMain C1
      -- def $ func "foo" $ \(x :: Word') -> snd $ while (x,3 :: Word') $ \(a,b) -> (a > 0, (pred a, succ b))
      -- def $ func "foo" $ \(x :: Word') -> snd $ while (x,3 :: Word') $ \(a,b) -> (a > 0, (pred a, b))
--      def $ func "foo" $ \(x :: Word') -> snd $ while ((x, 2 :: Word'),3 :: Word') $ \((a,c),b) -> (a > 0, ((pred a, c), x + c))
      -- def foo,
      -- def $ func "bar" $ \(x :: Int', y :: Word') -> foo (y, x)
--      def $ func "foo" $ \(x :: Word') -> snd $ while (x,3 :: Word') $ \(a,b) -> (a > 0, (pred a, b - x))
      -- def $ func "foo" $ \(x :: Int') -> while x $ \a -> (a > 0, dtoi $ while (itod a) $ \b -> (b > 10, itod a + pred b))
    ]
  -- print a
  printPP a
  return ()

data C1 = C1
instance Count C1 where countof _ = 1

dbl x = x + x

fastpow :: (Agg a, Arith a, Cmp a Bool') => a -> a -> a
fastpow b e =
  while_ ((b, e), 1) $ \((b, e), r) ->
    (e > 0, ((b * b, e / 2), if' ((e % 2) /= 0) (r * b) r))

-- import Prelude ((>>), ($), print, return, IO, Float, Int, Double, (.))
-- import Typed
-- import Eval
-- import SMTEval
-- import CEval
-- -- import Untyped
-- import Data.Word
-- import CBOR

-- -- data C16 = C16
-- -- instance Count C16 where ecountof _ = 16
-- -- data C4 = C4
-- -- instance Count C4 where ecountof _ = 4
-- -- data C1 = C1
-- -- instance Count C1 where ecountof _ = 1
-- -- data C2 = C2
-- -- instance Count C2 where ecountof _ = 2
-- -- data C5500 = C5500
-- -- instance Count C5500 where ecountof _ = 5500

-- foo x = do
--   print $ pp x
--   print $ pp $ runEval x

-- f1 = func "myfunc" $ \(a :: E Int) -> a + a
-- f2 = func "myfunc2" $ \(a :: E Int, b) -> a + cast b
-- f3 = func "myfunc3" $ \(a, b :: E Float) -> f1 a - f2 (a, b)
-- f4 = extern "extfunc1" :: E Float -> E Double
-- f5 = extern "extfunc2" :: (E Int, E Float) -> E Double

-- fpint = func "fastpowint" $ \(a, b :: E Int) -> fastpow a b

-- main :: IO ()
-- main = do
--   -- print tt
--   -- foo $ fastpow 2 (3 :: E Int)
--   -- foo $ fastpow (3 :: E Int) 2
--   -- foo $ dbl (dbl (2 :: E Double))
--   -- compile $ dbl $ fastpow (var 0 :: E Int) (var 1)
--   -- compile $ dbl (dbl (2 :: E Double))
--   let
--     t = compile "mymodule"
--       [
--         -- def fpint,
--         -- def $ func "fastpowint2" $ dbl . fpint
-- --        def $ func "foo" $ \(u0 :: E Word) -> switch u0 [7,8] (9 :: E Int)
--         def $ func "foo" $ \(u0 :: E Word) ->
--           if' (u0 > 42) 4 (if' (u0 < 10) (13 :: E Int) 5),
--         def $ func "bar" $ \(u0 :: E Int) ->
--           if' (u0 > 42) 4 (if' (u0 < 10) (13 :: E Int) 5)
--         -- def $ func "foo" $ \(u0 :: E Int) ->
--         -- if' (u0 > 42) 4 (if' (u0 < 10) (13 :: E Int) 5)
--         -- def $ func "foo" $ \(u1 :: E Int, u2 :: E Int) ->
--         -- if' (u1 > u2) (if' (u1 < 3) (13 :: E Int) 42) 5
--          -- if' ((u1 + u2) > 3)
--          --   (if' (u1 < 12)
--          -- (u1 + 4) u2)
--          --   (if' (u2 > 42) 42 (if' (u1 < 12) (u1 + u2) (u1 + 4)))
--                                                         -- , def f1
--                                                         -- , def f2
--                                                         -- , def f3
--                                                         -- , def f4
--                                                         -- , def f5
--       ]
--   -- printC t
--   printPP t
--   print $ smtEval t

-- -- main = compile $
-- --   spctMain C1
--   -- let arr :: E (V C4 Int) = vec [5 .. ] in
--   -- (vfold (+) (var 0) arr)
--   -- (ex (vmap (+ 1) arr) (var 0))
--   -- (ex arr (var 0))
--   -- doesn't work (ex (ex (vec (repeat $ vec [5 .. ]) :: E (V C4 (V C4 Int))) (var 0)) (var 0))
--   -- (ex (var 0) (vec [0 .. ] :: E (V C16 Int)))
--   -- evalA 1 (var 0)
--   -- ((snd $ fkMain (var 0)) :: E Int)
--   -- factorial (var 0 :: E Word64)
--   -- fkFlip $ fst $ nextPerm $ nextPerm perm0
--   -- fst $ nextPerm $ nextPerm perm0
--   -- fst perm0
--   --updix list0 (var 0) (+ 3)
--   -- setix list0 (var 0) 2
--   -- getix list0 (var 0)
--   -- rotate list0 (var 0)
-- --  (fannkuchredux ([4,2,1,5,3] :: [E Word]) :: E Word)
-- --  nbody (var 0)
-- --  sqrt (var 0 :: E Double)
-- --  (+) (var 0 :: E Double) (var 0 :: E Double)
-- --  max (var 0 :: E Int) (var 1)
-- --  fastpow (var 0 :: E Int) (var 1 :: E Word)
-}
