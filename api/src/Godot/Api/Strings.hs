-- | Conversions between Haskell strings and Godot's string types.
module Godot.Api.Strings (
  toGString,
  fromGString,
  toStringName,
  fromStringName,
) where

import Foreign
import GHC.Foreign qualified as GHC
import GHC.IO.Encoding (utf8)
import Godot.Api.Builtins (GString, StringName)
import Godot.Api.Runtime qualified as R
import Godot.GDExtension.Functions (Functions (..))
import Godot.GDExtension.Interface
import HsBindgen.Runtime.PtrConst qualified as PtrConst
import HsBindgen.Runtime.Support.FunPtr (fromFunPtr)

-- | A Godot @String@ with the given contents.
toGString :: String -> IO GString
toGString s = GHC.withCStringLen utf8 s $ \(chars, len) ->
  R.newOpaqueWith $ \p ->
    stringNewWithUtf8CharsAndLen
      R.functions
      (GDExtensionUninitializedStringPtr (castPtr p))
      (PtrConst.unsafeFromPtr chars)
      (GDExtensionInt (fromIntegral len))

fromGString :: GString -> IO String
fromGString s = do
  let ptr = GDExtensionConstStringPtr (PtrConst.unsafeFromPtr (castPtr (R.opaquePtr s)))
  -- With no buffer, the result is the length in bytes.
  GDExtensionInt len <- stringToUtf8Chars R.functions ptr nullPtr (GDExtensionInt 0)
  r <- allocaBytes (fromIntegral len + 1) $ \buf -> do
    _ <- stringToUtf8Chars R.functions ptr buf (GDExtensionInt len)
    GHC.peekCStringLen utf8 (buf, fromIntegral len)
  R.touch s
  pure r

{- | A Godot @StringName@: an interned string, used for names of methods,
properties, signals and nodes. Creating one looks it up in the engine's
name table, so keep and reuse the ones needed often.
-}
toStringName :: String -> IO StringName
toStringName s = GHC.withCString utf8 s $ \chars ->
  R.newOpaqueWith $ \p ->
    stringNameNewWithUtf8Chars
      R.functions
      (GDExtensionUninitializedStringNamePtr (castPtr p))
      (PtrConst.unsafeFromPtr chars)

fromStringName :: StringName -> IO String
fromStringName name = do
  -- Through a Variant: wrap the name, then ask Godot to stringify it.
  GDExtensionVariantFromTypeConstructorFunc wrap <-
    getVariantFromTypeConstructor R.functions GDEXTENSION_VARIANT_TYPE_STRING_NAME
  variant <- R.newOpaqueWith @R.Variant $ \v ->
    unwrapGDExtensionVariantFromTypeConstructorFunc_Aux
      (fromFunPtr wrap)
      (GDExtensionUninitializedVariantPtr (castPtr v))
      (GDExtensionTypePtr (castPtr (R.opaquePtr name)))
  R.touch name
  string <- R.newOpaque @GString $ \out ->
    variantStringify
      R.functions
      (GDExtensionConstVariantPtr (PtrConst.unsafeFromPtr (castPtr (R.opaquePtr variant))))
      (GDExtensionStringPtr (castPtr out))
  R.touch variant
  fromGString string
