{-# LANGUAGE OverloadedStrings #-}

{- | Generates the engine API (@Godot.Api.*@) from Godot's
@extension_api.json@, on top of the hs-bindgen interface package and the
hand-written "Godot.Api.Runtime".

Usage: GenApi extension_api.json OUT_DIR [CLASS ...]

Types (builtins, enums, a newtype per class) are generated for the whole
API. Method modules are generated for the named classes and their
ancestors, or for every class when none are named. A method is skipped
when it is virtual or uses a type the generator does not handle yet; the
count is printed at the end.
-}
module Main (main) where

import Control.Monad (forM, forM_, unless, when)
import Data.Aeson (Value (..), eitherDecodeFileStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (isDigit, isLower, isUpper, toLower, toUpper)
import Data.Foldable (toList)
import Data.IORef
import Data.List (intercalate, isPrefixOf, sort, stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Set qualified as Set
import Data.Text qualified as Text
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (..), hPutStr, hPutStrLn, hSetEncoding, stderr, utf8, withFile)

--------------------------------------------------------------------------------
-- JSON access

get :: String -> Value -> Maybe Value
get key (Object o) = KeyMap.lookup (Key.fromString key) o
get _ _ = Nothing

str :: String -> Value -> String
str key v = case get key v of
  Just (String t) -> Text.unpack t
  _ -> error ("missing string " <> key)

strMaybe :: String -> Value -> Maybe String
strMaybe key v = case get key v of
  Just (String t) -> Just (Text.unpack t)
  _ -> Nothing

list :: String -> Value -> [Value]
list key v = case get key v of
  Just (Array a) -> toList a
  _ -> []

int :: String -> Value -> Integer
int key v = case get key v of
  Just (Number n) -> truncate n
  _ -> error ("missing number " <> key)

flag :: String -> Value -> Bool
flag key v = get key v == Just (Bool True)

-- | The first element, or an error naming what was being looked for.
firstOf :: String -> [a] -> a
firstOf _ (x : _) = x
firstOf what [] = error ("extension_api.json: missing " <> what)

--------------------------------------------------------------------------------
-- Environment

data Env = Env
  { classParents :: Map.Map String (Maybe String)
  , enumTypes :: Set.Set String
  , recordSizes :: Map.Map String Int
  , opaqueTypes :: Set.Set String
  , refCounted :: Set.Set String
  -- ^ Classes that inherit @RefCounted@.
  }

-- | How a value crosses a call.
data Kind
  = KBool
  | -- | Fixed-layout value of this many bytes, copied into the call frame.
    KStorable Int
  | KObject
  | -- | Godot-owned representation; passed by pointer to its own storage.
    KOpaque

data Ty = Ty {tyHs :: String, tyKind :: Kind}

-- | Haskell names of builtin types that would clash with Prelude types.
builtinHsName :: String -> String
builtinHsName "String" = "GString"
builtinHsName n = n

resolve :: Env -> String -> Maybe Ty
resolve env t
  | t == "bool" = Just (Ty "P.Bool" KBool)
  | t == "int" = Just (Ty "Int64" (KStorable 8))
  | t == "float" = Just (Ty "P.Double" (KStorable 8))
  | t == "Variant" = Just (Ty "R.Variant" KOpaque)
  | t == "RID" = Just (Ty "RID" (KStorable 8))
  | Just e <- stripPrefix "enum::" t = Just (enum e)
  | Just e <- stripPrefix "bitfield::" t = Just (enum e)
  | "typedarray::" `isPrefixOf` t = Just (Ty "Array" KOpaque)
  | Just size <- Map.lookup t (recordSizes env) = Just (Ty t (KStorable size))
  | t `Set.member` opaqueTypes env = Just (Ty (builtinHsName t) KOpaque)
  | t `Map.member` classParents env = Just (Ty t KObject)
  | otherwise = Nothing
  where
    enum e =
      let name = enumTypeName e
       in Ty (if name `Set.member` enumTypes env then name else "Int64") (KStorable 8)

-- | @Node.ProcessMode@ becomes @Node_ProcessMode@.
enumTypeName :: String -> String
enumTypeName = map (\c -> if c == '.' then '_' else c)

{- | For a Godot type, how to box a Haskell expression of that type into a
variant and how to unbox one (unchecked), as code for a class module.
-}
variantConversion :: Env -> String -> Maybe (String -> String, String -> String)
variantConversion env t = do
  ty <- resolve env t
  let via witness = (\x -> "V.toVariantAs V." <> witness <> " " <> x, \r -> "V.fromVariantAs V." <> witness <> " " <> r)
  pure $ case tyKind ty of
    _ | t == "Variant" -> (\x -> "P.pure " <> x, \r -> "P.pure " <> r)
    KBool -> via "VBool"
    KObject ->
      ( \x -> "V.toVariantAs V.VObject (P.Just (R.convertHandle " <> x <> "))"
      , \r -> "P.maybe (P.pure P.Nothing) R.cast P.=<< V.fromVariantAs V.VObject " <> r
      )
    KOpaque
      | "typedarray::" `isPrefixOf` t -> via "VArray"
      | otherwise -> via ("V" <> t)
    KStorable _
      | t == "int" -> via "VInt"
      | t == "float" -> via "VFloat"
      -- Enums are newtypes over Int64, which is how a variant carries them.
      | "enum::" `isPrefixOf` t || "bitfield::" `isPrefixOf` t ->
          ( \x -> "V.toVariantAs V.VInt (coerce " <> x <> ")"
          , \r -> "coerce P.<$> V.fromVariantAs V.VInt " <> r
          )
      | otherwise -> via ("V" <> t)

ancestors :: Env -> String -> [String]
ancestors env c = c : maybe [] (ancestors env) (fromMaybe Nothing (Map.lookup c (classParents env)))

--------------------------------------------------------------------------------
-- Names

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, _ : rest) -> a : splitOn c rest
  (a, []) -> [a]

camel :: String -> String
camel name = case filter (not . null) (splitOn '_' name) of
  first : rest -> avoidKeyword (lowerFirst first <> concatMap upperFirst rest)
  [] -> "unnamed"
  where
    upperFirst (c : cs) = toUpper c : cs
    upperFirst [] = []
    lowerFirst (c : cs) = toLower c : cs
    lowerFirst [] = []

avoidKeyword :: String -> String
avoidKeyword n = if n `elem` keywords then n <> "_" else n
  where
    keywords =
      [ "case"
      , "class"
      , "data"
      , "default"
      , "deriving"
      , "do"
      , "else"
      , "foreign"
      , "if"
      , "import"
      , "in"
      , "infix"
      , "infixl"
      , "infixr"
      , "instance"
      , "let"
      , "module"
      , "newtype"
      , "of"
      , "then"
      , "type"
      , "where"
      , "forall"
      , "mdo"
      , "family"
      , "role"
      , "pattern"
      , "static"
      , "proc"
      , "rec"
      , "construct"
      , "singleton"
      ]

{- | Whether a value of the @Variant.Type@ enum, such as
@TYPE_PACKED_BYTE_ARRAY@ or @TYPE_TRANSFORM2D@, names the given type.
-}
sameVariantType :: String -> String -> Bool
sameVariantType enumValue godotName =
  filter (/= '_') (drop (length ("TYPE_" :: String)) enumValue) == map toUpper godotName

-- | Escape Haddock markup in text taken from the JSON descriptions.
escape :: String -> String
escape = concatMap $ \c -> if c `elem` ("\\/'`\"@<>$#*_[]" :: String) then ['\\', c] else [c]

docLine :: String -> Maybe String -> String
docLine godotName description =
  "-- | @" <> godotName <> "@" <> case fmap firstSentence description of
    Just d | not (null d) -> ". " <> escape d
    _ -> ""
  where
    firstSentence = unwords . words . takeWhile (/= '\n')

--------------------------------------------------------------------------------
-- Builtin types

genBuiltins :: Value -> Value -> Value -> [(String, Integer)] -> String
genBuiltins api sizes offsets variantTypes =
  unlines $
    [ "{-# LANGUAGE DerivingStrategies #-}"
    , "{-# LANGUAGE DuplicateRecordFields #-}"
    , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
    , "{-# LANGUAGE NoFieldSelectors #-}"
    , ""
    , "-- | Generated from @extension_api.json@. Do not edit."
    , "--"
    , "-- Godot's builtin value types. Types with a fixed layout are records"
    , "-- without selector functions: read their fields with field puns or"
    , "-- record wildcards. The others are opaque"
    , "-- and destroyed by Godot when garbage collected."
    , "module Godot.Api.Builtins where"
    , ""
    , "import Data.Int (Int32)"
    , "import Data.Word (Word64)"
    , "import Foreign.ForeignPtr (ForeignPtr)"
    , "import Foreign.Ptr (FunPtr, Ptr)"
    , "import Foreign.Storable (Storable (..))"
    , "import qualified Godot.Api.Runtime as R"
    , "import System.IO.Unsafe (unsafePerformIO)"
    , ""
    , "-- | A resource handle."
    , "newtype RID = RID Word64"
    , "  deriving newtype (Eq, Ord, Show, Storable)"
    , ""
    ]
      <> concatMap record (list "classes" offsets)
      <> concatMap opaque [b | b <- list "builtin_classes" api, flag "has_destructor" b]
  where
    sizeOf name = firstOf "size" [int "size" s | s <- list "sizes" sizes, str "name" s == name]
    memberType m = case str "meta" m of
      "float" -> "Float"
      "double" -> "Double"
      "int32" -> "Int32"
      other -> other
    record c =
      let name = str "name" c
          members = list "members" c
          vars = ["a" <> show i | i <- [0 .. length members - 1]]
       in [ "data " <> name <> " = " <> name
          , "  { "
              <> intercalate
                "\n  , "
                [str "member" m <> " :: !" <> memberType m | m <- members]
          , "  }"
          , "  deriving stock (Eq, Show)"
          , ""
          , "instance Storable " <> name <> " where"
          , "  sizeOf _ = " <> show (sizeOf name)
          , "  alignment _ = 4"
          , "  peek p ="
          , "    "
              <> name
              <> " <$> "
              <> intercalate " <*> " ["peekByteOff p " <> show (int "offset" m) | m <- members]
          , "  poke p (" <> unwords (name : vars) <> ") ="
          , "    "
              <> intercalate
                " >> "
                ["pokeByteOff p " <> show (int "offset" m) <> " " <> v | (m, v) <- zip members vars]
          , ""
          ]
    opaque b =
      let godotName = str "name" b
          name = builtinHsName godotName
          vt = firstOf godotName [v | (e, v) <- variantTypes, sameVariantType e godotName]
       in [ "-- | Godot's @" <> godotName <> "@."
          , "newtype " <> name <> " = " <> name <> " (ForeignPtr ())"
          , ""
          , "instance R.Opaque " <> name <> " where"
          , "  opaqueSize = " <> show (sizeOf godotName)
          , "  opaqueDestructor = destructor" <> name
          , "  opaqueDefault = default" <> name
          , ""
          , "destructor" <> name <> " :: FunPtr (Ptr () -> IO ())"
          , "destructor" <> name <> " = unsafePerformIO (R.lookupDestructor " <> show vt <> ")"
          , "{-# NOINLINE destructor" <> name <> " #-}"
          , ""
          , "default" <> name <> " :: Ptr () -> IO ()"
          , "default" <> name <> " = unsafePerformIO (R.lookupDefaultConstructor " <> show vt <> ")"
          , "{-# NOINLINE default" <> name <> " #-}"
          , ""
          ]

--------------------------------------------------------------------------------
-- Enums

data EnumDef = EnumDef
  { enumName :: String
  , enumPrefix :: String
  -- ^ Prepended to each value name; empty for global enums.
  , enumBitfield :: Bool
  , enumValues :: [(String, Integer)]
  }

allEnums :: Value -> [EnumDef]
allEnums api =
  [def "" (enumTypeName (str "name" e)) e | e <- list "global_enums" api]
    <> [ def (str "name" c <> "_") (str "name" c <> "_" <> str "name" e) e
       | c <- list "classes" api
       , e <- list "enums" c
       ]
  where
    def prefix name e =
      EnumDef name prefix (flag "is_bitfield" e) [(str "name" v, int "value" v) | v <- list "values" e]

genEnums :: [EnumDef] -> String
genEnums defs =
  unlines $
    [ "{-# LANGUAGE DerivingStrategies #-}"
    , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
    , "{-# LANGUAGE PatternSynonyms #-}"
    , ""
    , "-- | Generated from @extension_api.json@. Do not edit."
    , "--"
    , "-- Godot's enums: a newtype over 'Int64', which is how Godot passes them,"
    , "-- with a pattern synonym per value. Class enums and their values are"
    , "-- prefixed with the class name."
    , "module Godot.Api.Enums where"
    , ""
    , "import Data.Bits (Bits)"
    , "import Data.Int (Int64)"
    , "import Foreign.Storable (Storable)"
    , ""
    ]
      <> go Set.empty defs
  where
    go _ [] = []
    go used (d : rest) =
      let name = enumName d
          patterns =
            [ (p, v)
            | (valueName, v) <- enumValues d
            , let p0 = enumPrefix d <> valueName
            , let p = if any isUpper (take 1 p0) then p0 else name <> "_" <> p0
            ]
          fresh = dedupe used patterns
          used' = foldr (Set.insert . fst) used fresh
       in [ "newtype " <> name <> " = " <> name <> " Int64"
          , "  deriving newtype (Eq, Ord, Show, Storable" <> (if enumBitfield d then ", Bits" else "") <> ")"
          , ""
          ]
            <> concat
              [ [ "pattern " <> p <> " :: " <> name
                , "pattern " <> p <> " = " <> name <> " " <> (if v < 0 then "(" <> show v <> ")" else show v)
                ]
              | (p, v) <- fresh
              ]
            <> [""]
            <> go used' rest
    dedupe _ [] = []
    dedupe used ((p, v) : ps)
      | p `Set.member` used = dedupe used ps
      | otherwise = (p, v) : dedupe (Set.insert p used) ps

--------------------------------------------------------------------------------
-- Variants

-- | How one variant type is represented in Haskell.
data VariantRep = VNilRep | VBoolRep | VStorableRep | VOpaqueRep | VObjectRep

data VariantDef = VariantDef
  { vdNumber :: Integer
  , vdCon :: String
  -- ^ Constructor of the @VType@ witness.
  , vdHs :: String
  , vdRep :: VariantRep
  }

-- | Every variant type, from the values of the @Variant.Type@ enum.
variantDefs :: Env -> Value -> [(String, Integer)] -> [VariantDef]
variantDefs env api variantTypes =
  [ def godotName number
  | (enumName', number) <- variantTypes
  , enumName' /= "TYPE_MAX"
  , let godotName = firstOf enumName' [n | n <- names, sameVariantType enumName' n]
  ]
  where
    names = "Object" : map (str "name") (list "builtin_classes" api)
    def godotName number = case godotName of
      "Nil" -> VariantDef number "VNil" "()" VNilRep
      "bool" -> VariantDef number "VBool" "Bool" VBoolRep
      "int" -> VariantDef number "VInt" "Int64" VStorableRep
      "float" -> VariantDef number "VFloat" "Double" VStorableRep
      "Object" -> VariantDef number "VObject" "(Maybe Object)" VObjectRep
      _
        | godotName `Set.member` opaqueTypes env ->
            VariantDef number ("V" <> godotName) (builtinHsName godotName) VOpaqueRep
        | otherwise -> VariantDef number ("V" <> godotName) godotName VStorableRep

genVariant :: [VariantDef] -> String
genVariant defs =
  unlines $
    [ "{-# LANGUAGE AllowAmbiguousTypes #-}"
    , "{-# LANGUAGE GADTs #-}"
    , "{-# LANGUAGE KindSignatures #-}"
    , "{-# LANGUAGE ScopedTypeVariables #-}"
    , "{-# LANGUAGE StandaloneDeriving #-}"
    , "{-# LANGUAGE TypeApplications #-}"
    , ""
    , "-- | Generated from @extension_api.json@. Do not edit."
    , "--"
    , "-- A Godot @Variant@ holds a value of one of a fixed set of types. 'VType'"
    , "-- names them, each constructor fixing the Haskell type of the contents, so"
    , "-- a variant is a 'Sigma' 'VType': a value paired with a witness of its type."
    , "--"
    , "-- > viewVariant v >>= \\case"
    , "-- >   MkSigma VInt n -> ...      -- n :: Int64"
    , "-- >   MkSigma VVector3 p -> ...  -- p :: Vector3"
    , "-- >   _ -> ..."
    , "module Godot.Api.Variant"
    , "  ( VType (..)"
    , "  , Sigma (..)"
    , "  , VariantValue (..)"
    , "  , toVariant"
    , "  , fromVariant"
    , "  , viewVariant"
    , "  , toVariantAs"
    , "  , fromVariantAs"
    , "  , vtypeNumber"
    , "  ) where"
    , ""
    , "import Data.Int (Int64)"
    , "import Data.Kind (Type)"
    , "import Data.Type.Equality (TestEquality (..), (:~:) (..))"
    , "import Foreign.C.Types (CUInt)"
    , "import Godot.Api.Builtins"
    , "import Godot.Api.Classes (Object)"
    , "import Godot.Api.Runtime (Sigma (..), Variant)"
    , "import qualified Godot.Api.Runtime as R"
    , ""
    , "-- | The types a variant can hold. An object variant may hold no object;"
    , "-- the handle read from one is an instance ID and owns no reference, so"
    , "-- 'R.cast' it to a reference-counted class to keep the object."
    , "data VType :: Type -> Type where"
    ]
      <> ["  " <> vdCon d <> " :: VType " <> vdHs d | d <- defs]
      <> [ ""
         , "deriving stock instance Show (VType a)"
         , ""
         , "deriving stock instance Eq (VType a)"
         , ""
         , "instance TestEquality VType where"
         ]
      <> ["  testEquality " <> vdCon d <> " " <> vdCon d <> " = Just Refl" | d <- defs]
      <> [ "  testEquality _ _ = Nothing"
         , ""
         , "-- | Godot's number for a variant type."
         , "vtypeNumber :: VType a -> CUInt"
         , "vtypeNumber t = case t of"
         ]
      <> ["  " <> vdCon d <> " -> " <> show (vdNumber d) | d <- defs]
      <> [ ""
         , "-- | The Haskell types that are the contents of one kind of variant."
         , "class VariantValue a where"
         , "  vtype :: VType a"
         , ""
         ]
      <> ["instance VariantValue " <> vdHs d <> " where vtype = " <> vdCon d | d <- defs]
      <> [ ""
         , "-- | Box a value."
         , "toVariant :: VariantValue a => a -> IO Variant"
         , "toVariant = toVariantAs vtype"
         , ""
         , "-- | Unbox a variant, if it holds exactly this type. Unlike Godot's own"
         , "-- conversions this never coerces: an int variant is not a 'Double'."
         , "fromVariant :: forall a. VariantValue a => Variant -> IO (Maybe a)"
         , "fromVariant v = do"
         , "  n <- R.variantTypeNumber v"
         , "  if n == vtypeNumber (vtype @a)"
         , "    then Just <$> fromVariantAs vtype v"
         , "    else pure Nothing"
         , ""
         , "-- | A variant's contents together with their type."
         , "viewVariant :: Variant -> IO (Sigma VType)"
         , "viewVariant v = do"
         , "  n <- R.variantTypeNumber v"
         , "  case n of"
         ]
      <> ["    " <> show (vdNumber d) <> " -> MkSigma " <> vdCon d <> " <$> fromVariantAs " <> vdCon d <> " v" | d <- defs]
      <> [ "    _ -> fail (\"Godot.Api.Variant: unknown variant type \" <> show n)"
         , ""
         , "-- | Box a value of the type the witness names."
         , "toVariantAs :: VType a -> a -> IO Variant"
         , "toVariantAs t x = case t of"
         ]
      <> ["  " <> vdCon d <> " -> " <> into d | d <- defs]
      <> [ ""
         , "-- | Unbox a variant as the type the witness names. Unchecked: the variant"
         , "-- must hold that type; use 'fromVariant' or 'viewVariant' otherwise."
         , "fromVariantAs :: VType a -> Variant -> IO a"
         , "fromVariantAs t v = case t of"
         ]
      <> ["  " <> vdCon d <> " -> " <> outOf d | d <- defs]
  where
    num = show . vdNumber
    into d = case vdRep d of
      VNilRep -> "case x of () -> R.nilVariant"
      VBoolRep -> "R.variantFromStorable " <> num d <> " (R.fromBool x)"
      VStorableRep -> "R.variantFromStorable " <> num d <> " x"
      VOpaqueRep -> "R.variantFromOpaque " <> num d <> " x"
      VObjectRep -> "R.variantFromObject " <> num d <> " x"
    outOf d = case vdRep d of
      VNilRep -> "pure ()"
      VBoolRep -> "R.toBool <$> R.variantToStorable " <> num d <> " v"
      VStorableRep -> "R.variantToStorable " <> num d <> " v"
      VOpaqueRep -> "R.variantToOpaque " <> num d <> " v"
      VObjectRep -> "R.variantToObject " <> num d <> " v"

--------------------------------------------------------------------------------
-- Classes

genClasses :: Env -> [String] -> String
genClasses env names =
  unlines $
    [ "{-# LANGUAGE DerivingStrategies #-}"
    , "{-# LANGUAGE GeneralizedNewtypeDeriving #-}"
    , "{-# LANGUAGE MultiParamTypeClasses #-}"
    , ""
    , "-- | Generated from @extension_api.json@. Do not edit."
    , "--"
    , "-- One newtype per engine class, and the inheritance relation between them."
    , "-- The methods of a class are in @Godot.Api.Class.\\<Name\\>@."
    , "--"
    , "-- A handle to a reference-counted class owns a reference and keeps its"
    , "-- object alive. A handle to any other class is an instance ID: using it"
    , "-- after the object has been freed throws 'R.ObjectFreed'."
    , "module Godot.Api.Classes where"
    , ""
    , "import qualified Godot.Api.Runtime as R"
    , ""
    ]
      <> concatMap cls names
  where
    cls name =
      let counted = name `Set.member` refCounted env
          (rep, con) = if counted then ("R.Ref", "R.Counted") else ("R.ObjectId", "R.Managed")
       in [ "newtype " <> name <> " = " <> name <> " " <> rep
          , "  deriving newtype (Eq, Ord, Show)"
          , ""
          , "instance R.GodotClass " <> name <> " where"
          , "  godotClassName = " <> show name
          , "  isRefCounted = " <> show counted
          , "  toHandle (" <> name <> " h) = " <> con <> " h"
          , "  fromHandle (" <> con <> " h) = " <> name <> " h"
          , "  fromHandle _ = error " <> show ("Godot.Api: wrong kind of handle for " <> name)
          , "  {-# INLINE toHandle #-}"
          , "  {-# INLINE fromHandle #-}"
          ]
            <> ["instance R.Inherits " <> name <> " " <> a | a <- ancestors env name]
            <> [""]

data Skipped = Virtual | Vararg | Unsupported String

-- | The definitions for one method, or why it is skipped.
genMethod :: Env -> String -> Value -> Either Skipped [String]
genMethod env cls m
  | flag "is_virtual" m = Left Virtual
  | flag "is_vararg" m = do
      fixed <- traverse (\x -> (,) <$> argTy x <*> variantOf (str "type" x)) (list "arguments" m)
      ret <- traverse (\r -> (,) <$> (fst <$> retTy r) <*> variantOf (str "type" r)) (get "return_value" m)
      pure (renderVararg fixed ret)
  | otherwise = do
      args <- traverse argTy (list "arguments" m)
      ret <- traverse retTy (get "return_value" m)
      pure (render args ret)
  where
    godotName = str "name" m
    name = camel godotName
    worker = name <> "'"
    bind = "bind_" <> name
    isStatic = flag "is_static" m
    argTy a = maybe (Left (Unsupported (str "type" a))) Right (resolve env (str "type" a))
    retTy r = case resolve env (str "type" r) of
      Just t -> Right (t, strMaybe "meta" r == Just "required")
      Nothing -> Left (Unsupported (str "type" r))

    variantOf t = maybe (Left (Unsupported t)) Right (variantConversion env t)

    -- A method taking a variable number of arguments: the declared ones are
    -- typed, the rest are a list of variants, and the call goes through
    -- Godot's dynamic path.
    renderVararg fixed ret =
      let indexed = zip [0 :: Int ..] fixed
          var i = "a" <> show i
          isObject t = case tyKind t of KObject -> True; _ -> False
          argType (i, (t, _)) = if isObject t then "o" <> show i else tyHs t
          constraints =
            ["R.Inherits self " <> cls | not isStatic]
              <> ["R.Inherits o" <> show i <> " " <> tyHs t | (i, (t, _)) <- indexed, isObject t]
          context = case constraints of
            [] -> ""
            cs -> "(" <> intercalate ", " cs <> ") => "
          retType = case ret of
            Nothing -> "()"
            Just (t, _) -> if isObject t then "(P.Maybe " <> tyHs t <> ")" else tyHs t
          params = ["self" | not isStatic] <> map argType indexed <> ["[R.Variant]"]
          binders = unwords (["self" | not isStatic] <> map (var . fst) indexed <> ["rest"])
          boxed = ["v" <> show i | (i, _) <- indexed]
          arguments = "(" <> intercalate " : " (boxed <> ["rest"]) <> ")"
          callLine =
            if isStatic
              then "R.varcall " <> bind <> " nullPtr " <> arguments
              else "R.withObjectPtr self (\\self' -> R.varcall " <> bind <> " self' " <> arguments <> ")"
          result = case ret of
            Nothing -> "P.pure ()"
            Just (_, (_, unbox)) -> unbox "result"
       in [ bind <> " :: Ptr ()"
          , bind <> " = R.methodBind " <> show cls <> " " <> show godotName <> " " <> show (int "hash" m)
          , "{-# NOINLINE " <> bind <> " #-}"
          , ""
          , docLine (cls <> "." <> godotName) (strMaybe "description" m)
          , "--"
          , "-- Takes any number of further arguments, as variants."
          , name <> " :: " <> context <> intercalate " -> " (params <> ["P.IO " <> retType])
          , name <> " " <> binders <> " = do"
          ]
            <> ["  v" <> show i <> " <- " <> box (var i) | (i, (_, (box, _))) <- indexed]
            <> [ "  result <- " <> callLine
               , "  " <> result
               , ""
               ]

    render args ret =
      let n = length args
          indexed = zip [0 :: Int ..] args
          -- Storage slots follow the argument pointer array.
          slotSize t = case tyKind t of
            KBool -> 8
            KStorable s -> (s + 7) `div` 8 * 8
            KObject -> 8
            KOpaque -> 0
          offsets = scanl (+) (n * 8) (map slotSize args)
          retOffset = last offsets
          retSlot = maybe 0 (slotSize . fst) ret
          frameSize = max 8 (retOffset + retSlot)
          var i = "a" <> show i
          argType (i, t) = case tyKind t of
            KObject -> "o" <> show i
            _ -> tyHs t
          constraints =
            ["R.Inherits self " <> cls | not isStatic]
              <> ["R.Inherits o" <> show i <> " " <> tyHs t | (i, t) <- indexed, isObject t]
          isObject t = case tyKind t of KObject -> True; _ -> False
          isOpaque t = case tyKind t of KOpaque -> True; _ -> False
          retType = case ret of
            Nothing -> "()"
            Just (t, required) -> case tyKind t of
              KObject | not required -> "(P.Maybe " <> tyHs t <> ")"
              _ -> tyHs t
          context = case constraints of
            [] -> ""
            cs -> "(" <> intercalate ", " cs <> ") => "
          params = ["self" | not isStatic] <> map argType indexed
          signature extra = context <> intercalate " -> " (extra <> params <> ["P.IO " <> retType])
          binders = unwords (["self" | not isStatic] <> map (var . fst) indexed)
          writeArg ((i, t), off) = case tyKind t of
            KBool -> [poke off ("(R.fromBool " <> var i <> ")"), slot i off]
            KStorable _ -> [poke off (var i), slot i off]
            KObject -> [poke off ("p" <> show i), slot i off]
            KOpaque -> ["R.setArg fr " <> show i <> " (R.opaquePtr " <> var i <> ")"]
          poke off value = "R.pokeAt fr " <> show off <> " " <> value
          slot i off = "R.setArg fr " <> show i <> " (fr `plusPtr` " <> show off <> ")"
          self = if isStatic then "nullPtr" else "self'"
          -- Resolve each object to a pointer for the duration of the call.
          withObjects =
            concat
              ( ["R.withObjectPtr self P.$ \\self' -> " | not isStatic]
                  <> ["R.withObjectPtr " <> var i <> " P.$ \\p" <> show i <> " -> " | (i, t) <- indexed, isObject t]
              )
          call retPtr = "call " <> bind <> " " <> self <> " fr " <> retPtr
          touches = ["R.touch " <> var i | (i, t) <- indexed, isOpaque t]
          retHere = "(fr `plusPtr` " <> show retOffset <> ")"
          (prepare, callLine, result) = case ret of
            Nothing -> ([], call "nullPtr", "P.pure ()")
            Just (t, required) -> case tyKind t of
              KBool -> ([], call retHere, "R.toBool P.<$> R.peekAt fr " <> show retOffset)
              KStorable _ -> ([], call retHere, "R.peekAt fr " <> show retOffset)
              KObject ->
                ( [poke retOffset "(nullPtr :: Ptr ())"]
                , call retHere
                , (if required then "R.adopt" else "R.maybeAdopt") <> " P.=<< R.peekAt fr " <> show retOffset
                )
              KOpaque -> ([], call "ret", "P.pure ()")
          body =
            concatMap writeArg (zip indexed offsets) <> prepare <> [callLine] <> touches <> [result]
          opaqueRet = maybe False (isOpaque . fst) ret
          open =
            (if opaqueRet then "R.newOpaque P.$ \\ret -> " else "")
              <> withObjects
              <> "R.allocaFrame "
              <> show frameSize
              <> " P.$ \\fr -> do"
       in [ bind <> " :: Ptr ()"
          , bind <> " = R.methodBind " <> show cls <> " " <> show godotName <> " " <> show (int "hash" m)
          , "{-# NOINLINE " <> bind <> " #-}"
          , ""
          , worker <> " :: " <> signature ["R.Ptrcall"]
          , unwords (filter (not . null) [worker, "call", binders]) <> " = " <> open
          ]
            <> map ("  " <>) body
            <> [ "{-# INLINE " <> worker <> " #-}"
               , ""
               , docLine (cls <> "." <> godotName) (strMaybe "description" m)
               , name <> " :: " <> signature []
               , name <> " = " <> worker <> " R.ptrcallSafe"
               , "{-# INLINE " <> name <> " #-}"
               , ""
               , "-- | '" <> name <> "' as an unsafe foreign call: faster, but the engine must"
               , "-- not call back into Haskell while it runs."
               , name <> "_unsafe :: " <> signature []
               , name <> "_unsafe = " <> worker <> " R.ptrcallUnsafe"
               , "{-# INLINE " <> name <> "_unsafe #-}"
               , ""
               ]

genClassModule :: Env -> Set.Set String -> Value -> ([Skipped], String)
genClassModule env singletons c =
  ( skipped
  , unlines $
      [ "-- | Generated from @extension_api.json@. Do not edit."
      , "--"
      , docLine name (strMaybe "brief_description" c) <> " Import qualified."
      , "module Godot.Api.Class." <> name <> " where"
      , ""
      , "import Data.Int (Int64)"
      , "import Foreign.Ptr (Ptr, nullPtr, plusPtr)"
      , "import Godot.Api.Builtins"
      , "import Godot.Api.Classes"
      , "import Godot.Api.Enums"
      , "import qualified Godot.Api.Runtime as R"
      , "import qualified Godot.Api.Variant as V"
      , "import Data.Coerce (coerce)"
      , "import qualified Prelude as P"
      , ""
      ]
        <> [ l
           | flag "is_instantiable" c
           , l <-
               [ "-- | A new @" <> name <> "@."
               , "construct :: P.IO " <> name
               , "construct = R.construct"
               , ""
               ]
           ]
        <> [ l
           | name `Set.member` singletons
           , l <-
               [ "-- | The @" <> name <> "@ singleton."
               , "singleton :: P.IO " <> name
               , "singleton = R.singleton"
               , ""
               ]
           ]
        <> concat defs
  )
  where
    name = str "name" c
    results = map (genMethod env name) (list "methods" c)
    skipped = [s | Left s <- results]
    defs = [d | Right d <- results]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  apiPath : out : wanted <- getArgs
  api <- either fail pure =<< eitherDecodeFileStrict apiPath
  let config key = firstOf key [c | c <- list key api, str "build_configuration" c == "float_64"]
      sizes = config "builtin_class_sizes"
      offsets = config "builtin_class_member_offsets"
      variantTypes =
        firstOf
          "Variant.Type"
          [ [(str "name" v, int "value" v) | v <- list "values" e]
          | e <- list "global_enums" api
          , str "name" e == "Variant.Type"
          ]
      classes = list "classes" api
      enums = allEnums api
      sizeOf n = fromIntegral (firstOf n [int "size" s | s <- list "sizes" sizes, str "name" s == n])
      env =
        Env
          { classParents = Map.fromList [(str "name" c, strMaybe "inherits" c) | c <- classes]
          , enumTypes = Set.fromList (map enumName enums)
          , recordSizes = Map.fromList [(str "name" c, sizeOf (str "name" c)) | c <- list "classes" offsets]
          , opaqueTypes = Set.fromList [str "name" b | b <- list "builtin_classes" api, flag "has_destructor" b]
          , refCounted = Set.fromList [str "name" c | c <- classes, flag "is_refcounted" c]
          }
      names = map (str "name") classes
      selected
        | null wanted = Set.fromList names
        | otherwise = Set.fromList (concatMap (ancestors env) wanted)
      singletons = Set.fromList [str "name" s | s <- list "singletons" api]
      write path contents = do
        createDirectoryIfMissing True (takeDirectory (out </> path))
        -- Godot's descriptions contain non-ASCII text; do not depend on the
        -- locale, which is plain C in a nix build.
        withFile (out </> path) WriteMode $ \h -> hSetEncoding h utf8 >> hPutStr h contents
  -- The hand-written runtime calls these methods by hash.
  forM_ [("Object", "notification", 4023243586), ("RefCounted", "init_ref", 2240911060), ("RefCounted", "reference", 2240911060), ("RefCounted", "unreference", 2240911060)] $
    \(cls, method, expected) -> do
      let actual = [int "hash" m | c <- classes, str "name" c == cls, m <- list "methods" c, str "name" m == method]
      unless (actual == [expected :: Integer]) $
        fail (cls <> "." <> method <> " has hash " <> show actual <> " in this API, but Godot.Api.Runtime expects " <> show expected)
  forM_ wanted $ \w ->
    unless (w `Map.member` classParents env) $ fail ("unknown class " <> w)
  write "Godot/Api/Builtins.hs" (genBuiltins api sizes offsets variantTypes)
  write "Godot/Api/Enums.hs" (genEnums enums)
  write "Godot/Api/Classes.hs" (genClasses env names)
  write "Godot/Api/Variant.hs" (genVariant (variantDefs env api variantTypes))
  counts <- newIORef (0 :: Int, 0 :: Int, 0 :: Int, Map.empty :: Map.Map String Int)
  modules <- fmap concat . forM classes $ \c -> do
    let name = str "name" c
    if name `Set.member` selected
      then do
        let (skipped, contents) = genClassModule env singletons c
        write ("Godot/Api/Class" </> name <> ".hs") contents
        forM_ skipped $ \s -> modifyIORef' counts $ \(v, a, u, m) -> case s of
          Virtual -> (v + 1, a, u, m)
          Vararg -> (v, a + 1, u, m)
          Unsupported t -> (v, a, u + 1, Map.insertWith (+) t 1 m)
        let generated = length (list "methods" c) - length skipped
        when (generated < 0) (fail "impossible")
        pure [("Godot.Api.Class." <> name, generated)]
      else pure []
  write "modules.txt" (unlines (sort (map fst modules)))
  (virtual, vararg, unsupported, byType) <- readIORef counts
  hPutStrLn stderr $
    "GenApi: "
      <> show (length modules)
      <> " class modules, "
      <> show (sum (map snd modules))
      <> " methods; skipped "
      <> show virtual
      <> " virtual, "
      <> show vararg
      <> " vararg, "
      <> show unsupported
      <> " with unsupported types "
      <> show (Map.toList byType)
  when (isJust (lookup "" modules)) (fail "impossible")
