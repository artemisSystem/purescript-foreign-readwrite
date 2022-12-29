module Foreign.ReadWrite
  ( class ReadForeign
  , readForeign
  , class WriteForeign
  , writeForeign

  , undefined

  , Default(..)
  , default
  , MonoidProxy
  , DefaultMonoid

  , IncompleteRecord(..)

  , class ReadForeignRecord
  , readForeignRecordImpl
  , readForeignRecord
  , class WriteForeignRecord
  , writeForeignRecordImpl
  , writeForeignRecord
  ) where

import Prelude

import Control.Monad.Except (catchError)
import Data.Array.NonEmpty (NonEmptyArray, fromArray, toArray)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.TraversableWithIndex (traverseWithIndex)
import Foreign (FT, Foreign, ForeignError(..), fail, isUndefined, readArray, readBoolean, readChar, readInt, readNumber, readString, tagOf, unsafeFromForeign, unsafeToForeign)
import Foreign.Object (Object, insert, lookup, runST, thawST)
import Foreign.Object as Object
import Foreign.Object.ST (new, peek, poke)
import Prim.Row as R
import Prim.RowList (RowList)
import Prim.RowList as RL
import Record as Record
import Record.Builder (Builder)
import Record.Builder as Builder
import Record.Unsafe (unsafeGet)
import Safe.Coerce (coerce)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | Test whether a foreign value is an object
isObject ∷ Foreign → Boolean
isObject v = tagOf v == "Object"

-- | Attempt to coerce a foreign value to an `Object`.
readObject ∷ ∀ m. Monad m ⇒ Foreign → FT m (Object Foreign)
readObject value
  | isObject value = pure $ unsafeFromForeign value
  | otherwise = fail $ TypeMismatch "Object" (tagOf value)

foreign import undefined_ ∷ Foreign

undefined ∷ Foreign
undefined = undefined_

class ReadForeign a where
  readForeign ∷ ∀ m. Monad m ⇒ Foreign → FT m a

class WriteForeign a where
  writeForeign ∷ a → Foreign

instance ReadForeign Foreign where
  readForeign = pure

instance WriteForeign Foreign where
  writeForeign = identity

instance ReadForeign Void where
  readForeign _ = fail (ForeignError "Attempted to read Void from Foreign")

instance WriteForeign Void where
  writeForeign = absurd

-- | Matches `undefined`
instance ReadForeign Unit where
  readForeign a =
    if isUndefined a then pure unit
    else fail (TypeMismatch "undefined" (tagOf a))

-- | Writes `undefined`
instance WriteForeign Unit where
  writeForeign _ = undefined

instance ReadForeign String where
  readForeign = readString

instance WriteForeign String where
  writeForeign = unsafeToForeign ∷ String → Foreign

instance ReadForeign Char where
  readForeign = readChar

instance WriteForeign Char where
  writeForeign = unsafeToForeign ∷ Char → Foreign

instance ReadForeign Boolean where
  readForeign = readBoolean

instance WriteForeign Boolean where
  writeForeign = unsafeToForeign ∷ Boolean → Foreign

instance ReadForeign Number where
  readForeign = readNumber

instance WriteForeign Number where
  writeForeign = unsafeToForeign ∷ Number → Foreign

instance ReadForeign Int where
  readForeign = readInt

instance WriteForeign Int where
  writeForeign = unsafeToForeign ∷ Int → Foreign

instance ReadForeign a ⇒ ReadForeign (Identity a) where
  readForeign = readForeign >>> map (coerce ∷ a → Identity a)

instance WriteForeign a ⇒ WriteForeign (Identity a) where
  writeForeign = (coerce ∷ Identity a → a) >>> writeForeign

instance ReadForeign a ⇒ ReadForeign (Array a) where
  readForeign = readArray >=> traverseWithIndex \i value → readForeign value
    `catchError` (head >>> ErrorAtIndex i >>> fail)

instance WriteForeign a ⇒ WriteForeign (Array a) where
  writeForeign = map writeForeign >>>
    (unsafeToForeign ∷ Array Foreign → Foreign)

instance ReadForeign a ⇒ ReadForeign (NonEmptyArray a) where
  readForeign f = (readForeign f ∷ _ (Array a)) >>= \array →
    case fromArray array of
      Just nonEmptyArray → pure nonEmptyArray
      Nothing → fail (ForeignError "Array is empty")

instance WriteForeign a ⇒ WriteForeign (NonEmptyArray a) where
  writeForeign = toArray >>> writeForeign

instance ReadForeign a ⇒ ReadForeign (Object a) where
  readForeign = readObject >=> traverseWithIndex \k value → readForeign value
    `catchError` (head >>> ErrorAtProperty k >>> fail)

instance WriteForeign a ⇒ WriteForeign (Object a) where
  writeForeign = map writeForeign >>>
    (unsafeToForeign ∷ Object Foreign → Foreign)

instance ReadForeign a ⇒ ReadForeign (Maybe a) where
  readForeign value =
    if isUndefined value then pure Nothing else Just <$> readForeign value

instance WriteForeign a ⇒ WriteForeign (Maybe a) where
  writeForeign = maybe undefined writeForeign

-- | The `ReadForeign` and `WriteForeign` instances for `Default` will
-- | consider the type `default` the default value and read/write it as
-- | `undefined`. So `undefined` read into the type `Default 0 Int` will be
-- | the value `Default 0`, and writing it back to foreign returns `undefined`.
newtype Default ∷ ∀ defaultType. defaultType → Type → Type
newtype Default default a = Default a

type role Default phantom representational

derive instance Newtype (Default default a) _
derive instance Generic (Default default a) _

instance (Show a) ⇒ Show (Default default a) where
  show = genericShow

-- | The default value of a given type. For example, `default :: Default 0 Int`
-- | has the value `Default 0`
default ∷ ∀ default a. Reflectable default a ⇒ Default default a
default = Default $ reflectType (Proxy ∷ _ default)

instance
  ( ReadForeign defaultType
  , Reflectable default defaultType
  ) ⇒
  ReadForeign (Default default defaultType) where
  readForeign value =
    if isUndefined value then pure (Default defaultValue)
    else Default <$> readForeign value
    where
    defaultValue = reflectType (Proxy ∷ _ default)

instance
  ( WriteForeign defaultType
  , Eq defaultType
  , Reflectable default defaultType
  ) ⇒
  WriteForeign (Default default defaultType) where
  writeForeign (Default value) =
    if value == defaultValue then undefined else writeForeign value
    where
    defaultValue = reflectType (Proxy ∷ _ default)

foreign import data MonoidProxy ∷ Type → Type

instance (Monoid a) ⇒ Reflectable (MonoidProxy a) a where
  reflectType _ = mempty ∷ a

type DefaultMonoid a = Default (MonoidProxy a) a

-- | If any extra keys exist in the foreign object being read, it will result in
-- | an error. Preserves key order.
instance
  ( RL.RowToList row rl
  , ReadForeignRecord rl row
  ) ⇒
  ReadForeign (Record row) where
  readForeign = readForeignRecord (Proxy ∷ Proxy rl) true

-- | Preserves key order.
instance
  ( RL.RowToList row rl
  , WriteForeignRecord rl row
  ) ⇒
  WriteForeign (Record row) where
  writeForeign = writeForeignRecord (Proxy ∷ Proxy rl)

newtype IncompleteRecord r = IncompleteRecord r

derive instance Newtype (IncompleteRecord r) _

derive instance Generic (IncompleteRecord r) _

instance Show r ⇒ Show (IncompleteRecord r) where
  show = genericShow

-- | Does not error if extra keys exist in the foreign object being read.
-- | Preserves key order.
instance
  ( RL.RowToList row rl
  , ReadForeignRecord rl row
  ) ⇒
  ReadForeign (IncompleteRecord (Record row)) where
  readForeign = map IncompleteRecord <<<
    readForeignRecord (Proxy ∷ Proxy rl) false

class ReadForeignRecord
  ∷ RowList Type → Row Type → Constraint
class ReadForeignRecord rl row | rl → row where
  readForeignRecordImpl
    ∷ ∀ proxy m
    . Monad m
    ⇒ proxy rl
    → Boolean
    → Object Foreign
    → FT m (Builder {} { | row })

class WriteForeignRecord
  ∷ RowList Type → Row Type → Constraint
class WriteForeignRecord rl row | rl → row where
  writeForeignRecordImpl ∷ ∀ proxy. proxy rl → Record row → Object Foreign

instance readForeignRecordNil ∷ ReadForeignRecord RL.Nil () where
  readForeignRecordImpl _ errorOnExtraKeys object = do
    when errorOnExtraKeys $ forWithIndex_ object \k v → fail
      (ErrorAtProperty k (TypeMismatch "undefined" (tagOf v)))
    pure (identity ∷ Builder _ _)

instance writeForeignRecordNil ∷ WriteForeignRecord RL.Nil () where
  writeForeignRecordImpl _ {} = Object.empty

instance readForeignRecordCons ∷
  ( IsSymbol key
  , R.Cons key focus tail row
  , R.Lacks key tail
  , ReadForeignRecord rlTail tail
  , ReadForeign focus
  ) ⇒
  ReadForeignRecord (RL.Cons key focus rlTail) row where
  readForeignRecordImpl _ errorOnExtraKeys object = do
    let focusForeign = lookup key object # maybe undefined identity
    focus ← readForeign focusForeign `catchError`
      (head >>> ErrorAtProperty key >>> fail)
    tail ←
      readForeignRecordImpl rlTailP errorOnExtraKeys (Object.delete key object)
    pure $ Builder.insert keyP focus <<< tail
    where
    key = reflectSymbol keyP
    keyP = Proxy ∷ Proxy key
    rlTailP = Proxy ∷ Proxy rlTail

instance writeForeignRecordCons ∷
  ( IsSymbol key
  , R.Cons key focus tail row
  , R.Lacks key tail
  , WriteForeignRecord rlTail tail
  , WriteForeign focus
  ) ⇒
  WriteForeignRecord (RL.Cons key focus rlTail) row where
  writeForeignRecordImpl _ record = do
    let
      focusForeign = writeForeign (Record.get keyP record)
      tail = writeForeignRecordImpl rlTailP (Record.delete keyP record)
    insert key focusForeign tail
    where
    key = reflectSymbol keyP
    keyP = Proxy ∷ Proxy key
    rlTailP = Proxy ∷ Proxy rlTail

readForeignRecord
  ∷ ∀ proxy rl row m
  . Monad m
  ⇒ RL.RowToList row rl
  ⇒ ReadForeignRecord rl row
  ⇒ proxy rl
  → Boolean
  → Foreign
  → FT m (Record row)
readForeignRecord _ errorOnExtraKeys value = do
  valueObject ← readObject value
  values ← Builder.buildFromScratch <$>
    readForeignRecordImpl (Proxy ∷ _ rl) errorOnExtraKeys valueObject
  -- The reason we don't just return `values` is that it's alphabetical, so we
  -- piggyback off of the order of `valueObject` to retain the original order,
  -- while adding any keys that should be present in the final record, but were
  -- missing in the foreign object (for example, the `Maybe` instance for
  -- `ReadWriteForeign` allows its field/key to be missing).
  -- This ranks high among the dirtiest stuff I've done in PureScript. Sorry.
  let
    allCorrectKeysAsObject ∷ Object Foreign
    allCorrectKeysAsObject = unsafeCoerce values

    allKeysInCorrectOrder ∷ Object Foreign
    allKeysInCorrectOrder = runST do
      obj ← thawST valueObject
      forWithIndex_ allCorrectKeysAsObject \k v → peek k obj >>= case _ of
        Just _ → poke k (unsafeGet k values) obj
        Nothing → poke k v obj
      pure obj

    finalRecord ∷ Record row
    finalRecord = unsafeCoerce allKeysInCorrectOrder
  pure finalRecord

writeForeignRecord
  ∷ ∀ proxy rl row
  . WriteForeignRecord rl row
  ⇒ proxy rl
  → Record row
  → Foreign
writeForeignRecord _ record = do
  let
    allCorrectKeysAsObject ∷ Object Foreign
    allCorrectKeysAsObject = writeForeignRecordImpl (Proxy ∷ _ rl) record

    allKeysInCorrectOrder ∷ Object Foreign
    allKeysInCorrectOrder = unsafeCoerce record

    finalObj ∷ Object Foreign
    finalObj = runST do
      obj ← new
      forWithIndex_ allKeysInCorrectOrder \k _ →
        case lookup k allCorrectKeysAsObject of
          Just v → unless (isUndefined v) $ void (poke k v obj)
          Nothing → pure unit
      pure obj
  (unsafeToForeign ∷ Object Foreign → Foreign) $ finalObj
