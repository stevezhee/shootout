{-# OPTIONS_GHC -Wall #-}

module Main(main) where

import Data.Char
import Data.List(sort, intersperse)
import Data.Maybe
import Data.Map
import Control.Monad.State
import System.IO
import System.Process

data FFI = FFI
  { ffiHeader :: [String]
  , ffiProcs :: [String]
  , ffiFields :: [(String, String, [String])]
  , ffiInts :: [String]
  }

main :: IO ()
main = mapM_ ffiGen ffiTbl

ffiGen :: (String, FFI) -> IO ()
ffiGen (a,b) = do
  let cFn = a ++ ".c++"
  let llFn = a ++ ".ll"
  let hsFn =  a ++ ".hs"
  writeFile cFn $ ffiGenC b
  callCommand $ unwords ["clang -std=c++11 -S -emit-llvm -o", llFn, cFn]
  s <- readFile llFn
  withFile hsFn WriteMode $ \h -> flip evalStateT (St empty h) $ do
    output $ unwords ["module", a, "where"]
    output "import Shoot"
    mapM_ unDecl $ pDecls $ lines s  

ffiTbl :: [(String, FFI)]
ffiTbl =
  [("SDL", FFI
    { ffiHeader =
      [ "#include <stdio.h>"
      , "#include <SDL2/SDL.h>"
      ]
    , ffiProcs =
      [ "SDL_Init"
      , "SDL_CreateWindow"
      , "SDL_DestroyTexture"
      , "SDL_DestroyRenderer"
      , "SDL_DestroyWindow"
      , "SDL_Quit"
      , "SDL_GetWindowSurface"
      , "SDL_GetError"
      , "SDL_CreateRenderer"
      , "SDL_SetRenderDrawColor"
      , "SDL_LoadBMP_RW"
      , "SDL_RWFromFile"
      , "SDL_ConvertSurface"
      , "SDL_CreateTextureFromSurface"
      , "SDL_FreeSurface"
      , "SDL_PollEvent"
      , "SDL_RenderClear"
      , "SDL_RenderCopy"
      , "SDL_RenderPresent"
      ]
    , ffiFields =
      [ ("SDL_Rect", "r", ["x", "y", "w", "h"])
      , ("SDL_Surface", "s", ["w", "h"])
      , ("SDL_Event", "e", ["type", "key"])
      , ("SDL_KeyboardEvent", "ke", ["keysym"])
      , ("SDL_Keysym", "ks", ["sym"])
      ]
    , ffiInts =
      [ "SDL_QUIT"
      , "SDL_KEYDOWN"
      , "SDL_INIT_VIDEO"
      , "SDLK_UP"
      , "SDLK_DOWN"
      , "SDLK_LEFT"
      , "SDLK_RIGHT"
      , "SDL_RENDERER_ACCELERATED"
      ]
    })
  ]
  
ffiGenC :: FFI -> String
ffiGenC x = unlines $
  ffiHeader x ++
  fmap ffiFieldGenC (ffiFields x) ++
  [ "auto _fun_" ++ i ++ " = " ++ i ++ ";" | i <- ffiProcs x ] ++
  [ "int _val_" ++ i ++ " = " ++ i ++ ";" | i <- ffiInts x ]

ffiFieldGenC :: (String, String, [String]) -> String
ffiFieldGenC (a, b, cs) = unlines $
    [ a ++ " " ++ n ++ ";" ] ++
    [ "auto _fld_" ++ b ++ "_" ++ c ++ " = &" ++ n ++ "." ++ c ++ ";" | c <- cs ]
  where
    n = "_rec_" ++ a ++ "_rec_"

type M a = StateT St IO a

data St = St
  { tys :: Map String String
  , hndl :: Handle
  } deriving Show

data Decl
  = Ty String String
  | Fld String String String String String
  | Val String String
  | Fun String [String] String
  deriving (Show, Eq, Ord)

pSep :: String -> String -> Maybe [String]
pSep _ "" = return []
pSep pat s = case pString pat s of
  Nothing -> return [s]
  Just (b,c) -> do
    bs <- pSep pat c
    return (b:bs)
  
pString :: String -> String -> Maybe (String, String)
pString = loop "" ""
  where
    loop _     left ""     right           = Just (reverse left, right)
    loop _     _    _      ""              = Nothing
    loop saved left (p:ps) (r:rs) | r == p = loop (r:saved) left ps rs
    loop ""    left pat    (r:rs)          = loop "" (r:left) pat rs
    loop saved left pat    right           = loop "" (r:left) (ps ++ pat) (rs ++ right)
      where
        ps@(r:rs) = reverse saved

pStrings :: [String] -> String -> Maybe [String]
pStrings [] s = return [s]
pStrings (p:ps) s = do
  (b,c) <- pString p s
  bs <- pStrings ps c
  return (b:bs)

pOr :: [String -> Maybe a] -> String -> Maybe a
pOr [] _ = Nothing
pOr (p:ps) s = case p s of
  Nothing -> pOr ps s
  Just a -> return a

output :: String -> M ()
output s = gets hndl >>= \h -> lift (hPutStrLn h s)

commaSep :: [String] -> String
commaSep = concat . intersperse ", "
parens :: String -> String
parens xs = case xs of
  '(':_ -> xs
  _ -> "(" ++ xs ++ ")"

lookupType :: String -> M String
lookupType s = do
  t <- loop s
  case t of
    "()" -> return "()"
    a -> return $ val a
  where
  loop :: String -> M String
  loop x | last x == '*' = do
    t <- loop $ init x
    return $ ptr t
  loop x = case x of
    "void" -> return "()"
    "i32" -> return "Int"
    "i8 zeroext" -> return "Word8"
    "i8" -> return "Int8"
    _ -> do
      tbl <- gets tys
      if member x tbl
         then return $ pTyName x
         else error $ "unknown type:" ++ x

hName :: String -> String
hName "" = error "empty name"
hName (x:xs) = toLower x : xs

ptr :: String -> String
ptr x = parens $ unwords ["Ptr", x]

val :: String -> String
val x = unwords ["Val", x]

unDecl :: Decl -> M ()
unDecl x = case x of
  Ty a b -> do
    modify $ \st -> st{ tys = insert a b $ tys st }
    let n = pTyName a
    case pString "anon" n of
      Just ("",_) -> return ()
      _ -> do
        output $ unwords ["data", n]
        output $ unwords ["instance Ty", n, "where tyRec = tyRecFFI", show a, show b]
      
  Val a b -> output $ unwords [a, "= lit", b, "::", "Val Int"]
  Fun a bs c -> do
    t:ts <- mapM lookupType (c:bs)
    let v = hName a
    case ts of
      [] -> output $ unwords $ [v, "= ffi'", show a, "::", "M", parens t]
      _ -> output $ unwords $ [v, "= ffi", show a, "::", parens $ commaSep ts, "-> M", parens t]
    
  Fld a b c d e -> do
    let v = hName a
    tb <- lookupType b
    output $ unwords [v, "= ffi_fld", show c, show e, "::", val $ ptr d, "-> M", parens tb]

pDecl :: String -> Maybe Decl
pDecl = pOr
  [ \s -> do
      ["",b,c,d,e,f] <- pStrings ["@_fld_", " = global ", "* ", "@_rec_", "_rec_"] s
      return $ Fld b (c ++ "*") d e f
  , \s -> do
      ["",b,c,_] <- pStrings ["@_val_", " = global i32 ", ","] s
      return $ Val (fmap toLower b) c
  , \s -> do
      [a,b] <- pStrings [" = type "] s
      return $ Ty a b
  , \s -> do
      ["", b, c, d, ""] <- pStrings ["declare ", " @", "(", ")"] s
      ds <- pSep ", " d
      return $ Fun c ds b
  ]

pTyName :: String -> String
pTyName x = case ma of
  Nothing -> error $ "bad type name parse:" ++ x
  Just a -> a
  where
    ma = do
      ("",d) <- pOr
        [ pString "%struct."
        , pString "%union."
        ] x
      return d

pDecls :: [String] -> [Decl]
pDecls = sort . catMaybes . fmap pDecl
