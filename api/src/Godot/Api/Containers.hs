{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{- | Moving collections between Haskell and Godot.

Godot's @Array@, @Dictionary@ and packed arrays live in engine memory. This
module only builds them from Haskell values and reads them back; work with
the contents in Haskell.
-}
module Godot.Api.Containers (
  -- * Arrays
  arrayFromList,
  arrayToList,
  arrayFromVariants,
  arrayToVariants,

  -- * Dictionaries
  dictionaryFromList,
  dictionaryToList,
  dictionaryFromVariants,
  dictionaryToVariants,

  -- * Packed arrays
  Packed (..),
  packedFromList,
  packedToList,
  packedFromPtr,
  packedLength,
  packedStringsFromList,
  packedStringsToList,
) where

import Control.Monad (forM, forM_, when, (>=>))
import Data.Array qualified as A
import Data.Coerce (coerce)
import Data.Int (Int32, Int64)
import Data.Kind (Type)
import Data.Void (Void)
import Data.Word (Word8)
import Foreign hiding (void)
import Foreign.C (CUInt)
import Godot.Api.Builtins
import Godot.Api.Runtime (Variant)
import Godot.Api.Runtime qualified as R
import Godot.Api.Strings (peekGString, pokeGString)
import Godot.Api.Variant (VType (..), VariantValue, fromVariant, toVariant, vtypeNumber)
import Godot.GDExtension.Functions (Functions (..))
import Godot.GDExtension.Interface
import HsBindgen.Runtime.PtrConst qualified as PtrConst
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
-- Size and resize
--
-- Every container has these two methods, with the same signature. The
-- hashes are checked against the API by the generator.

-- Variant types 27 (Dictionary) to 38, each looked up on first use.
sizeMethods :: A.Array Int R.BuiltinMethod
sizeMethods = A.listArray (27, 38) [unsafePerformIO (R.lookupBuiltinMethod n "size" 3173160232) | n <- [27 .. 38]]
{-# NOINLINE sizeMethods #-}

-- Variant types 28 (Array) to 38; a dictionary has no resize.
resizeMethods :: A.Array Int R.BuiltinMethod
resizeMethods = A.listArray (28, 38) [unsafePerformIO (R.lookupBuiltinMethod n "resize" 848867239) | n <- [28 .. 38]]
{-# NOINLINE resizeMethods #-}

dictionaryKeys :: R.BuiltinMethod
dictionaryKeys = unsafePerformIO (R.lookupBuiltinMethod (vtypeNumber VDictionary) "keys" 4144163970)
{-# NOINLINE dictionaryKeys #-}

size :: (R.Opaque a) => CUInt -> a -> IO Int
size vt a = R.allocaFrame 8 $ \ret -> do
  (sizeMethods A.! fromIntegral vt) (R.opaquePtr a) nullPtr ret 0
  R.touch a
  fromIntegral <$> (R.peekAt ret 0 :: IO Int64)

resize :: (R.Opaque a) => CUInt -> a -> Int -> IO ()
resize vt a n = R.allocaFrame 24 $ \fr -> do
  R.pokeAt fr 8 (fromIntegral n :: Int64)
  R.setArg fr 0 (fr `plusPtr` 8)
  (resizeMethods A.! fromIntegral vt) (R.opaquePtr a) fr (fr `plusPtr` 16) 1
  R.touch a

--------------------------------------------------------------------------------
-- Arrays

-- | An @Array@ of the given values.
arrayFromList :: (VariantValue a) => [a] -> IO Array
arrayFromList = traverse toVariant >=> arrayFromVariants

{- | The elements of an @Array@, or 'Nothing' if any of them is not of the
expected type.
-}
arrayToList :: (VariantValue a) => Array -> IO (Maybe [a])
arrayToList array = sequence <$> (arrayToVariants array >>= traverse fromVariant)

-- | An @Array@ of values of any types.
arrayFromVariants :: [Variant] -> IO Array
arrayFromVariants values = do
  array <- R.newDefault
  resize (vtypeNumber VArray) array (length values)
  forM_ (zip [0 ..] values) $ \(i, value) -> do
    GDExtensionVariantPtr slot <-
      arrayOperatorIndex R.functions (GDExtensionTypePtr (castPtr (R.opaquePtr array))) (GDExtensionInt i)
    R.assignVariant (castPtr slot) value
  R.touch array
  pure array

arrayToVariants :: Array -> IO [Variant]
arrayToVariants array = do
  n <- size (vtypeNumber VArray) array
  values <- forM [0 .. n - 1] $ \i -> do
    GDExtensionVariantPtr slot <-
      arrayOperatorIndexConst
        R.functions
        (GDExtensionConstTypePtr (PtrConst.unsafeFromPtr (castPtr (R.opaquePtr array))))
        (GDExtensionInt (fromIntegral i))
    R.copyVariant (castPtr slot)
  R.touch array
  pure values

--------------------------------------------------------------------------------
-- Dictionaries

{- | A @Dictionary@ with the given entries, in order. A later entry replaces
an earlier one with the same key.
-}
dictionaryFromList :: (VariantValue k, VariantValue v) => [(k, v)] -> IO Dictionary
dictionaryFromList =
  traverse (\(k, v) -> (,) <$> toVariant k <*> toVariant v) >=> dictionaryFromVariants

{- | The entries of a @Dictionary@ in its order, or 'Nothing' if any key or
value is not of the expected type.
-}
dictionaryToList :: (VariantValue k, VariantValue v) => Dictionary -> IO (Maybe [(k, v)])
dictionaryToList dictionary = do
  entries <- dictionaryToVariants dictionary
  sequence <$> forM entries (\(k, v) -> liftA2 (,) <$> fromVariant k <*> fromVariant v)

dictionaryFromVariants :: [(Variant, Variant)] -> IO Dictionary
dictionaryFromVariants entries = do
  dictionary <- R.newDefault
  forM_ entries $ \(key, value) -> do
    -- Indexing with a key that is absent adds it.
    GDExtensionVariantPtr slot <-
      dictionaryOperatorIndex
        R.functions
        (GDExtensionTypePtr (castPtr (R.opaquePtr dictionary)))
        (GDExtensionConstVariantPtr (PtrConst.unsafeFromPtr (castPtr (R.opaquePtr key))))
    R.assignVariant (castPtr slot) value
    R.touch key
  R.touch dictionary
  pure dictionary

dictionaryToVariants :: Dictionary -> IO [(Variant, Variant)]
dictionaryToVariants dictionary = do
  keys <- R.newOpaque $ \ret -> dictionaryKeys (R.opaquePtr dictionary) nullPtr ret 0
  entries <- arrayToVariants keys >>= traverse entry
  R.touch dictionary
  pure entries
  where
    entry key = do
      GDExtensionVariantPtr slot <-
        dictionaryOperatorIndexConst
          R.functions
          (GDExtensionConstTypePtr (PtrConst.unsafeFromPtr (castPtr (R.opaquePtr dictionary))))
          (GDExtensionConstVariantPtr (PtrConst.unsafeFromPtr (castPtr (R.opaquePtr key))))
      value <- R.copyVariant (castPtr slot)
      R.touch key
      pure (key, value)

--------------------------------------------------------------------------------
-- Packed arrays

{- | A packed array: a contiguous block of one element type, which is how
Godot takes bulk data such as vertex positions or instance transforms.
-}
class (R.Opaque p, Storable (Element p)) => Packed p where
  type Element p :: Type

  packedVType :: VType p

  -- | A pointer to the element at an index, for writing.
  packedElement :: p -> Int -> IO (Ptr (Element p))

  -- | A pointer to the element at an index, for reading.
  packedElementConst :: p -> Int -> IO (Ptr (Element p))

packedNumber :: forall p. (Packed p) => CUInt
packedNumber = vtypeNumber (packedVType @p)

packedLength :: forall p. (Packed p) => p -> IO Int
packedLength = size (packedNumber @p)

packedFromList :: forall p. (Packed p) => [Element p] -> IO p
packedFromList elements = withArrayLen elements (flip packedFromPtr)

{- | A packed array holding a copy of that many elements read from the
pointer. One block copy, whatever the length.
-}
packedFromPtr :: forall p. (Packed p) => Ptr (Element p) -> Int -> IO p
packedFromPtr source n = do
  packed <- R.newDefault
  resize (packedNumber @p) packed n
  when (n > 0) $ do
    target <- packedElement packed 0
    copyArray target source n
  R.touch packed
  pure packed

packedToList :: forall p. (Packed p) => p -> IO [Element p]
packedToList packed = do
  n <- packedLength packed
  elements <- if n > 0 then packedElementConst packed 0 >>= peekArray n else pure []
  R.touch packed
  pure elements

-- Index a packed array through one of the interface's accessor functions.
element ::
  (R.Opaque p) =>
  (GDExtensionTypePtr -> GDExtensionInt -> IO r) ->
  (r -> Ptr e) ->
  p ->
  Int ->
  IO (Ptr e)
element index unwrap packed i =
  unwrap <$> index (GDExtensionTypePtr (castPtr (R.opaquePtr packed))) (GDExtensionInt (fromIntegral i))

elementConst ::
  (R.Opaque p) =>
  (GDExtensionConstTypePtr -> GDExtensionInt -> IO r) ->
  (r -> Ptr e) ->
  p ->
  Int ->
  IO (Ptr e)
elementConst index unwrap packed i =
  unwrap
    <$> index
      (GDExtensionConstTypePtr (PtrConst.unsafeFromPtr (castPtr (R.opaquePtr packed))))
      (GDExtensionInt (fromIntegral i))

typePtr :: GDExtensionTypePtr -> Ptr e
typePtr = castPtr . coerce @GDExtensionTypePtr @(Ptr Void)

instance Packed PackedByteArray where
  type Element PackedByteArray = Word8
  packedVType = VPackedByteArray
  packedElement = element (packedByteArrayOperatorIndex R.functions) castPtr
  packedElementConst = elementConst (packedByteArrayOperatorIndexConst R.functions) (castPtr . PtrConst.unsafeToPtr)

instance Packed PackedInt32Array where
  type Element PackedInt32Array = Int32
  packedVType = VPackedInt32Array
  packedElement = element (packedInt32ArrayOperatorIndex R.functions) castPtr
  packedElementConst = elementConst (packedInt32ArrayOperatorIndexConst R.functions) (castPtr . PtrConst.unsafeToPtr)

instance Packed PackedInt64Array where
  type Element PackedInt64Array = Int64
  packedVType = VPackedInt64Array
  packedElement = element (packedInt64ArrayOperatorIndex R.functions) castPtr
  packedElementConst = elementConst (packedInt64ArrayOperatorIndexConst R.functions) (castPtr . PtrConst.unsafeToPtr)

instance Packed PackedFloat32Array where
  type Element PackedFloat32Array = Float
  packedVType = VPackedFloat32Array
  packedElement = element (packedFloat32ArrayOperatorIndex R.functions) castPtr
  packedElementConst = elementConst (packedFloat32ArrayOperatorIndexConst R.functions) (castPtr . PtrConst.unsafeToPtr)

instance Packed PackedFloat64Array where
  type Element PackedFloat64Array = Double
  packedVType = VPackedFloat64Array
  packedElement = element (packedFloat64ArrayOperatorIndex R.functions) castPtr
  packedElementConst = elementConst (packedFloat64ArrayOperatorIndexConst R.functions) (castPtr . PtrConst.unsafeToPtr)

instance Packed PackedVector2Array where
  type Element PackedVector2Array = Vector2
  packedVType = VPackedVector2Array
  packedElement = element (packedVector2ArrayOperatorIndex R.functions) typePtr
  packedElementConst = elementConst (packedVector2ArrayOperatorIndexConst R.functions) typePtr

instance Packed PackedVector3Array where
  type Element PackedVector3Array = Vector3
  packedVType = VPackedVector3Array
  packedElement = element (packedVector3ArrayOperatorIndex R.functions) typePtr
  packedElementConst = elementConst (packedVector3ArrayOperatorIndexConst R.functions) typePtr

instance Packed PackedVector4Array where
  type Element PackedVector4Array = Vector4
  packedVType = VPackedVector4Array
  packedElement = element (packedVector4ArrayOperatorIndex R.functions) typePtr
  packedElementConst = elementConst (packedVector4ArrayOperatorIndexConst R.functions) typePtr

instance Packed PackedColorArray where
  type Element PackedColorArray = Color
  packedVType = VPackedColorArray
  packedElement = element (packedColorArrayOperatorIndex R.functions) typePtr
  packedElementConst = elementConst (packedColorArrayOperatorIndexConst R.functions) typePtr

{- | A @PackedStringArray@. Its elements are Godot strings, not a block of
plain values, so it has its own conversions.
-}
packedStringsFromList :: [String] -> IO PackedStringArray
packedStringsFromList strings = do
  packed <- R.newDefault
  resize (vtypeNumber VPackedStringArray) packed (length strings)
  forM_ (zip [0 ..] strings) $ \(i, string) -> do
    GDExtensionStringPtr slot <-
      packedStringArrayOperatorIndex R.functions (GDExtensionTypePtr (castPtr (R.opaquePtr packed))) (GDExtensionInt i)
    pokeGString (castPtr slot) string
  R.touch packed
  pure packed

packedStringsToList :: PackedStringArray -> IO [String]
packedStringsToList packed = do
  n <- size (vtypeNumber VPackedStringArray) packed
  strings <- forM [0 .. n - 1] $ \i -> do
    GDExtensionStringPtr slot <-
      packedStringArrayOperatorIndexConst
        R.functions
        (GDExtensionConstTypePtr (PtrConst.unsafeFromPtr (castPtr (R.opaquePtr packed))))
        (GDExtensionInt (fromIntegral i))
    peekGString (castPtr slot)
  R.touch packed
  pure strings
