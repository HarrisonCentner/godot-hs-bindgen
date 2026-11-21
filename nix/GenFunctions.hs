{-# LANGUAGE OverloadedStrings #-}

{- | Generates @Godot.GDExtension.Functions@: a record holding every function
of the GDExtension interface at its real type, and a loader that fills it.

hs-bindgen gives each interface function a type, but the C header only
names the string to look it up by in a comment. Godot's
@gdextension_interface.json@ lists those names, so this pairs each name
with its type. The function types are copied from the module hs-bindgen
generated; a wrong pairing fails to compile.

Usage: runghc GenFunctions.hs gdextension_interface.json Interface.hs OUT.hs
-}
module Main (main) where

import Data.Aeson (Value (..), eitherDecodeFileStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char (toUpper)
import Data.Foldable (toList)
import Data.List (intercalate, isPrefixOf, stripPrefix)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text qualified as Text
import System.Environment (getArgs)

data Function = Function
  { lookupName :: String
  -- ^ Name passed to @get_proc_address@, e.g. @mem_alloc@.
  , auxType :: String
  -- ^ hs-bindgen's newtype around the function type.
  , since :: String
  , deprecated :: Maybe String
  , summary :: String
  }

main :: IO ()
main = do
  [jsonPath, interfacePath, outPath] <- getArgs
  json <- either fail pure =<< eitherDecodeFileStrict jsonPath
  types <- functionTypes <$> readFile interfacePath
  functions <- maybe (fail "unexpected gdextension_interface.json layout") pure (parse json)
  fields <- traverse (field types) functions
  writeFile outPath (render fields)

{- | The function type of every @..._Aux@ newtype in the generated module,
keyed by newtype name. Each appears as
@  { unwrapNAME :: TYPE@ on one line.
-}
functionTypes :: String -> Map.Map String String
functionTypes = Map.fromList . mapMaybe parseLine . lines
  where
    parseLine line = do
      rest <- stripPrefix "  { unwrap" line
      let (name, sep) = break (== ' ') rest
      ty <- stripPrefix " :: " sep
      if "_Aux" `isSuffixOf'` name then Just (name, ty) else Nothing
    isSuffixOf' suffix s = reverse suffix `isPrefixOf` reverse s

parse :: Value -> Maybe [Function]
parse json = do
  Array entries <- get "interface" json
  traverse parseFunction (toList entries)

parseFunction :: Value -> Maybe Function
parseFunction entry = do
  name <- string =<< get "name" entry
  sinceVersion <- string =<< get "since" entry
  let legacy = string =<< get "legacy_type_name" entry
      description = case get "description" entry of
        Just (Array ls) -> unwords (mapMaybe string (toList ls))
        _ -> ""
      replacement = do
        dep <- get "deprecated" entry
        depSince <- string =<< get "since" dep
        pure $
          "Deprecated since Godot " <> depSince <> case string =<< get "replace_with" dep of
            Just new -> "; use " <> camel new
            Nothing -> ""
  pure
    Function
      { lookupName = name
      , auxType = maybe ("GDExtensionInterface" <> pascal name) id legacy <> "_Aux"
      , since = sinceVersion
      , deprecated = replacement
      , summary = description
      }

get :: String -> Value -> Maybe Value
get key (Object o) = KeyMap.lookup (Key.fromString key) o
get _ _ = Nothing

string :: Value -> Maybe String
string (String t) = Just (Text.unpack t)
string _ = Nothing

pascal :: String -> String
pascal = concatMap capitalise . splitOn '_'
  where
    capitalise (c : cs) = toUpper c : cs
    capitalise [] = []

camel :: String -> String
camel name = case splitOn '_' name of
  first : rest -> first <> pascal (intercalate "_" rest)
  [] -> name

splitOn :: Char -> String -> [String]
splitOn c s = case break (== c) s of
  (a, _ : rest) -> a : splitOn c rest
  (a, []) -> [a]

-- | A function paired with the type hs-bindgen gave it.
field :: Map.Map String String -> Function -> IO (Function, String)
field types f = case Map.lookup (auxType f) types of
  Just ty -> pure (f, ty)
  Nothing -> fail ("no generated type " <> auxType f <> " for " <> lookupName f)

render :: [(Function, String)] -> String
render fields =
  unlines $
    [ "{-# LANGUAGE RecordWildCards #-}"
    , ""
    , "-- | Generated from @gdextension_interface.json@. Do not edit."
    , "--"
    , "-- Every function of the GDExtension interface, at its real type."
    , "module Godot.GDExtension.Functions"
    , "  ( Functions (..)"
    , "  , loadFunctions"
    , "  ) where"
    , ""
    , "import Foreign.C.String (withCString)"
    , "import Foreign.Ptr (castFunPtr, nullFunPtr)"
    , "import Godot.GDExtension.Interface"
    , "import qualified HsBindgen.Runtime.LibC"
    , "import qualified HsBindgen.Runtime.PtrConst as PtrConst"
    , "import qualified HsBindgen.Runtime.Support as BG"
    , "import HsBindgen.Runtime.Support.FunPtr (FromFunPtr, fromFunPtr)"
    , ""
    , "-- | The interface functions, looked up once by 'loadFunctions'."
    , "data Functions = Functions"
    ]
      <> zipWith renderField ("  { " : repeat "  , ") fields
      <> [ "  }"
         , ""
         ]
      <> mapMaybe renderDeprecation fields
      <> [ ""
         , "-- | Look up every interface function. Call it once, from the extension's"
         , "-- entry point, with the @get_proc_address@ argument Godot passes in."
         , "--"
         , "-- A function the running engine does not provide is not an error here;"
         , "-- the corresponding field throws when it is called."
         , "loadFunctions :: GDExtensionInterfaceGetProcAddress -> IO Functions"
         , "loadFunctions getProcAddress = do"
         ]
      <> map renderLookup fields
      <> [ "  pure Functions {..}"
         , "  where"
         , "    getProc ="
         , "      unwrapGDExtensionInterfaceGetProcAddress_Aux"
         , "        (fromFunPtr (unwrapGDExtensionInterfaceGetProcAddress getProcAddress))"
         , ""
         , "    load :: FromFunPtr aux => String -> (aux -> f) -> IO f"
         , "    load name unwrap = do"
         , "      GDExtensionInterfaceFunctionPtr fp <-"
         , "        withCString name (getProc . PtrConst.unsafeFromPtr)"
         , "      pure $"
         , "        if fp == nullFunPtr"
         , "          then error (\"GDExtension interface function not provided by this engine: \" <> name)"
         , "          else unwrap (fromFunPtr (castFunPtr fp))"
         ]

renderField :: String -> (Function, String) -> String
renderField prefix (f, ty) =
  intercalate
    "\n"
    [ prefix <> camel (lookupName f) <> " :: " <> ty
    , "  -- ^ @" <> lookupName f <> "@, since Godot " <> since f <> ". " <> escape (summary f)
    ]

renderDeprecation :: (Function, String) -> Maybe String
renderDeprecation (f, _) = do
  message <- deprecated f
  pure ("{-# DEPRECATED " <> camel (lookupName f) <> " " <> show message <> " #-}")

renderLookup :: (Function, String) -> String
renderLookup (f, _) =
  "  "
    <> camel (lookupName f)
    <> " <- load "
    <> show (lookupName f)
    <> " unwrap"
    <> auxType f

-- | Escape Haddock markup in text taken from the JSON descriptions.
escape :: String -> String
escape = concatMap $ \c -> if c `elem` ("\\/'`\"@<>$#*_[]" :: String) then ['\\', c] else [c]
