{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}

module Main(main) where

-- import Data.Char
-- import Data.List(sort, intersperse)
-- import Data.Maybe
-- import Data.Map
import Control.Monad.State
-- import System.IO
-- import System.Process
import Language.C
-- import Language.C.Data.Ident
import Data.List
-- import Data.Maybe
import Language.C.Analysis
-- import Language.C.Analysis.TypeUtils
-- import Control.Arrow
import Data.Map hiding (filter, (\\))
-- import Data.List
import Data.Graph

checkResult :: (Show a) => String -> (Either a b) -> IO b
checkResult label = either (error . (label++) . show) return

declTy :: Declaration a => a -> Ty
declTy = typeToTy . declType

tagDefToTy :: TagDef -> (UserOrAnon, Ty)
tagDefToTy x = case x of
  EnumDef (EnumType a bs _ _) -> (sueRefToUserOrAnon a, Enum $ fmap declId bs)
  CompDef (CompType a b cs _ _) ->
    (sueRefToUserOrAnon a, f [ (declId c, declTy c) | c <- cs ])
    where
      f = if b == StructTag then Struct else Union

typeDefToTy :: TypeDef -> (Id, Ty)
typeDefToTy (TypeDef a b _ _) = (identToString a, typeToTy b)

data UserOrAnon
  = User String
  | Anon Int
    deriving (Show, Eq)

getTyNames = nub . concatMap loop
  where
    loop x = case x of
      Array a -> loop a
      Ptr a -> loop a
      Function a bs -> loop a ++ concatMap loop bs
      Prim{} -> []
      UATy (User a) -> [a]
      UATy (Anon{}) -> error "loop - anonymous name not removed"
      Struct bs -> concatMap loop $ fmap snd bs
      Union bs -> concatMap loop $ fmap snd bs
      Enum{} -> []
  
data Ty
  = Array Ty -- ArraySize
  | Ptr Ty
  | Function Ty [Ty]
  | Prim String
  | UATy UserOrAnon
  | Struct [(Id, Ty)]
  | Union [(Id, Ty)]
  | Enum [String]
    deriving (Show, Eq)

unAnon tbl x = case x of
  User a -> a
  Anon i -> case Data.List.lookup i tbl of
    Just a -> a
    Nothing -> "Anon" ++ show i 
  
unAnonTy f x = case x of
  Array a -> Array (g a)
  Ptr a -> Ptr (g a)
  Function a bs -> Function (g a) (fmap g bs)
  Prim{} -> x
  UATy a -> UATy $ User $ f a
  Struct bs -> Struct [(a, g b) | (a,b) <- bs ]
  Union bs -> Union [(a, g b) | (a,b) <- bs ]
  Enum{} -> x
  where
    g = unAnonTy f
      
idToUser :: (Id, Ty) -> [(Int, Id)]
idToUser (x, y) = case y  of
  Array a -> idToUser (x, a)
  Ptr a -> idToUser (x, a)
  Function a bs -> concat
    [ idToUser (f $ show i, t)
    | (i,t) <- zip [0 :: Int .. ] (a : bs)
    ]
  Struct bs -> concat [ idToUser (f x', t) | (x', t) <- bs ]
  Union bs -> concat [ idToUser (f x', t) | (x', t) <- bs ]
  Prim _ -> []
  UATy (User{}) -> []
  UATy (Anon i) -> [(i, x)]
  Enum{} -> []
  where
    f a = x ++ "_" ++ a

anonToUser :: (UserOrAnon, Ty) -> [(Int, Id)] -> [(Int, Id)]
anonToUser (User a, b) tbl = idToUser (a, b) ++ tbl
anonToUser (Anon i, b) tbl = case Data.List.lookup i tbl of
  Just a -> anonToUser (User a, b) tbl
  Nothing -> anonToUser (User $ error "anonToUser", b) tbl

typeToTy :: Type -> Ty
typeToTy x = case x of
  PtrType a _ _ -> Ptr $ typeToTy a
  ArrayType a b _ _ -> Array (typeToTy a)
  FunctionType a _ -> case a of
    FunType b cs _ -> Function (typeToTy b) (fmap (typeToTy . declType) cs)
    _ -> error "FunctionType"
  TypeDefType (TypeDefRef a _ _) _ _ -> UATy $ User $ identToString a
  DirectType a _ _ -> case a of
    TyVoid -> Prim "void"
    TyIntegral a -> Prim $ show a
    TyFloating a -> Prim $ show a
    TyComplex{} -> error "TyComplex"
    TyEnum a -> UATy $ sueRefToUserOrAnon $ sueRef a
    TyComp a -> UATy $ sueRefToUserOrAnon $ sueRef a
    TyBuiltin a -> case a of
      TyVaList -> Prim "TyVaList"
      TyAny -> Prim "TyAny"
    
sueRefToUserOrAnon :: SUERef -> UserOrAnon
sueRefToUserOrAnon x = case x of
  AnonymousRef a -> Anon $ nameId a
  NamedRef a -> User $ identToString a

type M a = StateT () IO a

type Id = String

declId :: Declaration n => n -> String
declId = identToString . declIdent

ppTy :: Ty -> String
ppTy x = case x of
  Prim "void" -> "()"
  _ -> "Val " ++ loop x
  where
    loop x = case x of
      Prim a -> case a of
        "int" -> "Int"
        "unsigned int" -> "Word"
        "unsigned short" -> "Word16" -- BAL:?
        "long" -> "Int64" -- BAL:?
        "long long" -> "Int128" -- BAL:?
        "unsigned long long" -> "Word128" -- BAL:?
        "unsigned long" -> "Word64" -- BAL:?
        "long double" -> "LongDouble" -- BAL:?
        "double" -> "Double"
        "char" -> "Char"
        "float" -> "Float"
        "TyVaList" -> a
        "void" -> "()"
        "TyAny" -> a
        _ -> error a
      Ptr a -> parens $ "Ptr " ++ loop a
      Array a -> parens $ "Array " ++ loop a
      UATy (User a) -> a
      Function{} -> show x -- BAL:???
      _ -> error $ show x

isVoid zs = zs == [] || zs == [Prim "void"]

ppTyFunc :: Ty -> [Ty] -> String
ppTyFunc y zs = unwords [pre, post]
  where
    pre = case zs of
      _ | isVoid zs -> ""
      [z] -> unwords [ ppTy z, "->" ]
      _ -> unwords [ parens $ commaSep $ fmap ppTy zs, "->" ]
    post = case y of
      Prim "void" -> "M ()"
      _ -> unwords [ "M", parens $ ppTy y ]

isSDL x = case x of
  'S':'D':'L':_ -> True
  _ -> False
  
ppField (x, Struct ys) = mapM_ f $ zip ys [0 :: Int .. ]
  where
    f ((y,t),i) = do
      putStrLn $ unwords [fn, "::", ppTyFunc (Ptr t) [Ptr $ UATy $ User x] ]
      putStrLn $ unwords [fn, "=", "field", show i]
      where
        fn = "v" ++ x ++ "_" ++ y
ppField _ = return ()

ppFunc (x, Function y zs) = do
  putStrLn $ unwords [fn, "::", ppTyFunc y zs]
  putStrLn $ unwords [fn, "=", if isVoid zs then "ffi'" else "ffi", show x]
  where
    fn = "f" ++ x
ppFunc _ = return ()
  
main = do
  eab <- parseCFilePre "foo.c"
  case eab of
    Left{} -> print eab
    Right a -> do
      case runTrav_ $ analyseAST a of
        Left es -> print es
        Right (globals, warnings) -> do
          let funcs :: [(Id, Ty)] = fmap (\a -> (declId a, declTy a)) $ elems $ fst $ splitIdentDecls False $ gObjs globals
          let tydefs :: [(Id, Ty)] = fmap typeDefToTy $ elems $ gTypeDefs globals
          let tagdefs :: [(UserOrAnon, Ty)] = fmap tagDefToTy $ elems $ gTags globals
          let tbl = Data.List.foldr anonToUser (concatMap idToUser tydefs) tagdefs
          let f = unAnon tbl
          let tydefs' =
                [ (a, unAnonTy f b)
                | (a, b) <- tydefs ++ [ (f a, b) | (a,b) <- tagdefs]
                ]
          let funcs' = filter (isSDL . fst) funcs
--          mapM_ ppFunc funcs'
          let (gr, f, g) = graphFromEdges [ (n, a, getTyNames [b]) | n@(a,b) <- tydefs' ]
          let g' = maybe (error "unknown key") id . g
          let f' v = let (a,_,_) = f v in a
          let sdltynames = getTyNames $ fmap snd funcs'
          let tys = fmap f' $ nub $ concatMap (reachable gr) $ fmap g' sdltynames
          mapM_ print tys
 --         print tydefs'
--          mapM_ ppField tydefs'
          return ()
      return ()

-- reachable :: Graph -> Vertex -> [Vertex]

-- listof :: (a -> Maybe b) -> [a] -> [b]
-- listof f = catMaybes . fmap f

-- cDeclExt :: CExternalDeclaration a -> Maybe (CDeclaration a)
-- cDeclExt x = case x of
--   CDeclExt a -> Just a
--   _ -> Nothing

-- stuff = do
--   return ()
             
-- 42 EnumDef
-- isTagEvent (TagEvent (EnumDef{})) = True
-- 63 CompDef
-- isTagEvent (TagEvent (CompDef{})) = True
-- isTagEvent (TypeDefEvent{} ) = True == 0
-- isTagEvent (LocalEvent{} ) = True == 0
-- isTagEvent (DeclEvent{} ) = True == 0
-- isTagEvent _ = True
-- isTagEvent _ = False
  
-- typename can be either a int/void/etc or user or anon

-- typedefref is user

-- type is ptr, array, function constructed or
--         typename* or
--         typedefref

-- tagdef is union/struct with user or anon and [member] or
--           enum type with user or anon and [enum]
-- type M a = StateT St IO a
-- data St = St
--   { tys :: Map Ty TyName
--   , szs :: Map ArraySize SzName
--   }

-- type FunTy = (TyName, [TyName])

-- typeNameToTyName :: TypeName -> M TyName
-- typeNameToTyName x = case x of
--   TyVoid -> "void"
--   TyIntegral a -> show a
--   TyFloating a -> show a
--   TyComp a b _ -> case b of
--     StructTag -> 
--   TyEnum EnumTypeRef	 - sueRef
--   TyComplex{} -> error "TyComplex"
--   TyBuiltin _ -> error "TyBuiltin"

-- typeToTyName :: Type -> M TyName
-- typeToTyName = undefined

-- arraySizeToSzName :: ArraySize -> M SzName
-- arraySizeToSzName = \_ -> return "SOME_SIZE"

-- funTypeToFunTy :: FunType -> M FunTy
-- funTypeToFunTy x = case x of
--   FunType a bs False -> (,) <$> typeToTyName a <*> mapM toTyName bs
--   _ -> error $ "funTypeToFunTy"

-- typeDefRefToTyName :: TypeDefRef -> M TyName
-- typeDefRefToTyName (TypeDefRef a _ _) = return $ identToString a

-- toTyName :: Declaration a => a -> M TyName
-- toTyName = typeToTyName . declType
  
-- typeToTy :: Type -> M Ty
-- typeToTy x = case x of
--   DirectType a _ _ -> TyName <$> typeNameToTyName a
--   PtrType a _ _ -> Ptr <$> typeToTyName a
--   ArrayType a b _ _ -> Array <$> arraySizeToSzName b <*> typeToTyName a
--   FunctionType a _ -> Func <$> funTypeToFunTy a
--   TypeDefType a _ _ -> TyName <$> typeDefRefToTyName a

--  let enums :: [(Id, Type)] = fmap declIdentType $ elems fs
  -- let (anonenums, idenums) = splitSUERef enums
  -- let (anonstructs, idstructs) = splitSUERef structs
  -- let (anonunions, idunions) = splitSUERef unions
--  return $ fmap suerefToString $ keys $ gTags $ filterGlobalDecls isTagEvent globals
--  let bs :: [String] = fmap suerefToString $ keys $ gTags $ filterGlobalDecls isTagEvent globals
  -- let cs :: [String] = fmap identToString $ keys $ gTypeDefs $ filterGlobalDecls isTagEvent globals
  -- let tys = fmap (\(a,b) -> (suerefToString a, fooOfTagDef b)) $ toList $ gTags globals
  -- let blarg = fmap identToString . keys
--  let ds = fmap fst $ filter (\(a,b) -> identToString a == "extern_func") $ toList $ gObjs globals
--  return $ (bs, cs, ds) -- , fmap identToString ds)

-- gTags = 105
-- gObjs = 1475
-- gTypeDefs = 201 (200?)

-- type FieldName = String

-- typeToNames :: Type -> [Name]
-- typeToNames x = case canonicalType x of
--   DirectType a _ _ -> typeNameToNames a
--   PtrType a _ _ -> typeToNames a
--   ArrayType a _ _ _ -> typeToNames a
--   FunctionType a _ -> case a of
--     FunType a bs False -> typeToNames a ++ concat (fmap (typeToNames . declType) bs)
--     _ -> error "FunType"
--   TypeDefType{} -> []

-- sueRefNames x = case x of
--   AnonymousRef a -> [a]
--   _ -> []
  
-- typeNameToNames :: TypeName -> [Name]
-- typeNameToNames x = case x of
--   TyVoid -> []
--   TyIntegral _ -> []
--   TyFloating _ -> []
--   TyComplex _ -> []
--   TyBuiltin _ -> []
--   TyComp a -> sueRefNames $ sueRef a
--   TyEnum a -> sueRefNames $ sueRef a

-- splitSUERef xs = ([ (a,b) | (AnonymousRef a, b) <- xs ], [ (identToString a,b) | (NamedRef a, b) <- xs ])
-- declIdentType x = (declId x, canonicalType $ declType x)

-- declToFoo (Decl a _) = (identToString $ declIdent a, typeOfType $ declType a)
-- varDeclToFoo (VarDecl a _ c) = (identToString $ identOfVarName a, typeOfType c)
-- toFoo :: Declaration n => n -> (String, String)
-- toFoo a = (declToString a, typeOfType $ declType a)
-- isCool x = declLinkage x == ExternalLinkage && head (declToString x) /= '_'

-- identDeclToFoo x = case x of
--   Declaration{} -> "Declaration" -- Decl	object or function declaration
--   ObjectDef{} -> "ObjectDef" -- ObjDef	object definition
--   FunctionDef{} -> "FunctionDef" -- FunDef	function definition
--   EnumeratorDef{} -> "EnumeratorDef" -- Enumerator	definition of an enumerator
-- declToString :: Declaration n => n -> String
-- declToString = identToString . declIdent

-- fooOfTagDef :: TagDef -> String
-- fooOfTagDef x = case x of
--   CompDef (CompType _ b cs _ _) -> show b ++ ":" ++ semiSep (fmap (show . toFoo) cs)
--   EnumDef (EnumType _ bs _ _) -> "EnumDef:" -- ++ commaSep (fmap declToString bs)

-- typeOfTypeDef (TypeDef _ b _ _) = typeOfType b

-- typeOfType x = case x of
--   DirectType a _ _ -> typeOfTypeName a -- "DirectType" -- TypeName TypeQuals Attributes	
--   PtrType a _ _ -> "PtrType:" ++ typeOfType a -- Type TypeQuals Attributes	
--   ArrayType a b _ _ -> "ArrayType:" ++ typeOfType a ++ ":" ++ typeOfArraySize b -- Type ArraySize TypeQuals Attributes	
--   FunctionType a _ -> "FunctionType:" ++ typeOfFunType a -- FunType Attributes	
--   TypeDefType a _ _ -> "TypeDefType:" ++ typeOfTypeDefRef a -- TypeDefRef TypeQuals Attributes	

-- fromCIntConst x = case x of
--   CConst (CIntConst a _) -> show a
--   _ -> "ComputedArraySize"

-- typeOfArraySize x = case x of
--   UnknownArraySize{} -> "UnknownArraySize"
--   ArraySize _ a -> fromCIntConst a
    
-- typeOfTypeDefRef (TypeDefRef a _ _) = identToString a

commaSep :: [String] -> String
commaSep = concat . intersperse ", "
parens :: String -> String
parens xs = "(" ++ xs ++ ")"
semiSep :: [String] -> String
semiSep = unlines . fmap (flip (++) ";")

-- typeOfFunType x = case x of
--   FunType a bs _ ->
--     typeOfType a ++ ":" ++ commaSep (fmap (typeOfType . declType) bs)
--   FunTypeIncomplete a -> error "FunTypeIncomplete" -- typeOfType a
  
-- typeOfTypeName x = case x of
--   TyVoid{} -> "TyVoid" -- TyIntegral IntType	 
--   TyFloating{} -> "TyFloating" -- FloatType	 
--   TyComplex{} -> "TyComplex" -- FloatType	 
--   TyComp a -> typeOfCompTypeRef a
--   TyEnum a -> suerefToString $ sueRef a
--   TyBuiltin{} -> "TyBuiltin" -- BuiltinType	 
--   TyIntegral a -> show a

-- typeOfCompTypeRef (CompTypeRef a b _) = show b ++ ":" ++ suerefToString a
  
-- suerefToString x = identToString $ case x of
--   NamedRef a -> a
--   AnonymousRef a -> mkIdent nopos ("anon" ++ show (nameId a)) a
  --     (globals, 
  --     case runTrav
  --     let a' = fmap (\_ -> ()) a
  --     case a' :: CTranslationUnit () of
  --       CTranslUnit (bs :: [CExternalDeclaration ()]) () -> do
  --         let cs :: [CDeclaration ()] = listof cDeclExt bs
  --         print $ length bs
  --         return ()
  --         -- let zs = [ ([ f | CTypeSpec f <- cs ], [ (e,gs,h,is) | (Just (CDeclr e gs h is ()), _, _) <- ds ]) | CDeclExt (CDecl cs ds ()) <- bs ]
  --         -- print $ zs !! 70
  --         -- print $ zs !! 80
  --         -- print $ zs !! 100
  --         -- -- print $ filter ((==) 0 . length . snd) zs
  --         -- print $ length zs
  --     return ()
  
--     Right (CTranslUnit bs _) -> do
--       let cs = cdeclspecs bs
--       let ds = [ (s,t, map fromField $ maybe [] id mds)
--                | CTypeSpec (CSUType (CStruct t (Just (Ident s _ _)) mds _ _) _) <- concat cs ]
-- --      print (ds !! 1)
--       print ds
--       print $ length ds
--       -- print $ cs !! 1
--       print $ length cs
  -- = CDeclExt (CDeclaration a)
  -- | CFDefExt (CFunctionDef a)
  -- | CAsmExt (CStringLiteral a) a
--    [ b | CFDefExt b <- bs ]
-- cdeclspecs xs = [ (length ys, length zs) | CDeclExt (CDecl ys zs _) <- xs, length zs /= 1 ]
-- cdeclspecs xs = [ ys | CDeclExt (CDecl ys zs _) <- xs ] -- , length zs == 1 ]

-- fromField x = case x of
--   CDecl ts ns _ -> map (h $ map f ts) ns
--   _ -> error $ "unhandled field:" ++ show x
--   where
--     h ts a = case a of
--       (Just (CDeclr (Just (Ident s _ _)) us _ _ _),_,_) -> (s, ts, map g us)
--       _ -> error $ "unhandled field name:" ++ show a
      
--     f a = case a of
--       CTypeSpec (CTypeDef (Ident s _ _) _) -> s
--       CTypeSpec (CSUType (CStruct _ (Just (Ident s _ _)) _ _ _) _) -> s
--       CTypeSpec (CSUType (CStruct _ Nothing _ _ _) _) -> "CStructNothing"
--       CTypeSpec (CIntType _) -> "CIntType"
--       CTypeSpec (CCharType _) -> "CCharType"
--       CTypeSpec (CUnsigType _) -> "CUnsigType"
--       CTypeQual (CConstQual _) -> "CTypeQual"
--       CTypeSpec (CVoidType _) -> "CVoidType"
--       CTypeSpec (CDoubleType _) -> "CDoubleType"
--       CTypeSpec (CFloatType _) -> "CFloatType"
--       _ -> error $ "unhandled type spec:" ++ show a

--     g a = case a of
--       CPtrDeclr [] _ -> "CPtrDeclr"
--       CFunDeclr (Right _) _ _ -> "CFunDeclr"
--       CArrDeclr _ (CArrSize False (CConst (CIntConst i _))) _ -> "CArrDeclr:" ++ show i
--       _ -> error $ "unhandled type declr:" ++ show a
      
-- data CDeclaration a
--   = CDecl [CDeclarationSpecifier a]
--           [(Maybe (CDeclarator a),
--             Maybe (CInitializer a),
--             Maybe (CExpression a))]
--           a
-- data CDeclarationSpecifier a
--   = CStorageSpec (CStorageSpecifier a)
--   | CTypeSpec (CTypeSpecifier a)
--   | CTypeQual (CTypeQualifier a)
-- data CStructureUnion a
--   = CStruct CStructTag
--             (Maybe Ident)
--             (Maybe [CDeclaration a])
--             [CAttribute a]
--             a
-- type CStructUnion = CStructureUnion NodeInfo
-- data CTypeSpecifier a
--   | CSUType (CStructureUnion a) a

-- data CTranslationUnit a = CTranslUnit [CExternalDeclaration a] a

{-
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
      [ "SDL_LoadBMP_RW"
      -- , "SDL_Init"
      -- , "SDL_CreateWindow"
      -- , "SDL_DestroyTexture"
      -- , "SDL_DestroyRenderer"
      -- , "SDL_DestroyWindow"
      -- , "SDL_Quit"
      -- , "SDL_GetWindowSurface"
      -- , "SDL_GetError"
      -- , "SDL_CreateRenderer"
      -- , "SDL_SetRenderDrawColor"
      -- , "SDL_RWFromFile"
      -- , "SDL_ConvertSurface"
      -- , "SDL_CreateTextureFromSurface"
      -- , "SDL_FreeSurface"
      -- , "SDL_PollEvent"
      -- , "SDL_RenderClear"
      -- , "SDL_RenderCopy"
      -- , "SDL_RenderPresent"
      ]
    , ffiFields =
      [ -- ("SDL_Rect", "r", ["x", "y", "w", "h"])
      -- , ("SDL_Surface", "s", ["w", "h"])
      -- , ("SDL_Event", "e", ["type", "key"])
      -- , ("SDL_KeyboardEvent", "ke", ["keysym"])
      -- , ("SDL_Keysym", "ks", ["sym"])
      ]
    , ffiInts =
      [ -- "SDL_QUIT"
      -- , "SDL_KEYDOWN"
      -- , "SDL_INIT_VIDEO"
      -- , "SDLK_UP"
      -- , "SDLK_DOWN"
      -- , "SDLK_LEFT"
      -- , "SDLK_RIGHT"
      -- , "SDL_RENDERER_ACCELERATED"
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
-}
