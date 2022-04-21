module Foreign.ReadWrite
  ( class ReadWriteForeign
  , readForeign
  , writeForeign

  , class ReadWriteForeignRecord
  , readForeignRecordImpl
  , readForeignRecord
  , writeForeignRecordImpl
  , writeForeignRecord
  ) where

import Prelude

import Control.Monad.Except (catchError)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (head)
import Data.Maybe (Maybe(..), maybe)
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

foreign import undefined ∷ Foreign

class ReadWriteForeign a where
  readForeign ∷ ∀ m. Monad m ⇒ Foreign → FT m a
  writeForeign ∷ a → Foreign

instance ReadWriteForeign Foreign where
  readForeign = pure
  writeForeign = identity

instance ReadWriteForeign Void where
  readForeign _ = fail (ForeignError "Attempted to read Void from Foreign")
  writeForeign = absurd

instance ReadWriteForeign String where
  readForeign = readString
  writeForeign = unsafeToForeign ∷ String → Foreign

instance ReadWriteForeign Char where
  readForeign = readChar
  writeForeign = unsafeToForeign ∷ Char → Foreign

instance ReadWriteForeign Boolean where
  readForeign = readBoolean
  writeForeign = unsafeToForeign ∷ Boolean → Foreign

instance ReadWriteForeign Number where
  readForeign = readNumber
  writeForeign = unsafeToForeign ∷ Number → Foreign

instance ReadWriteForeign Int where
  readForeign = readInt
  writeForeign = unsafeToForeign ∷ Int → Foreign

instance ReadWriteForeign a ⇒ ReadWriteForeign (Identity a) where
  readForeign = readForeign >>> map (coerce ∷ a → Identity a)
  writeForeign = (coerce ∷ Identity a → a) >>> writeForeign

instance ReadWriteForeign a ⇒ ReadWriteForeign (Array a) where
  readForeign = readArray >=> traverseWithIndex \i value → readForeign value
    `catchError` (head >>> ErrorAtIndex i >>> fail)
  writeForeign = map writeForeign >>>
    (unsafeToForeign ∷ Array Foreign → Foreign)

instance ReadWriteForeign a ⇒ ReadWriteForeign (Object a) where
  readForeign = readObject >=> traverseWithIndex \k value → readForeign value
    `catchError` (head >>> ErrorAtProperty k >>> fail)
  writeForeign = map writeForeign >>>
    (unsafeToForeign ∷ Object Foreign → Foreign)

instance ReadWriteForeign a ⇒ ReadWriteForeign (Maybe a) where
  readForeign value =
    if isUndefined value then pure Nothing else Just <$> readForeign value
  writeForeign = maybe undefined writeForeign

-- | If any extra keys exist in the foreign object being read, it will result in
-- | an error. Preserves key order.
instance
  ( RL.RowToList row rl
  , ReadWriteForeignRecord rl row
  ) ⇒
  ReadWriteForeign (Record row) where
  readForeign = readForeignRecord (Proxy ∷ Proxy rl)
  writeForeign = writeForeignRecord (Proxy ∷ Proxy rl)

class ReadWriteForeignRecord
  ∷ RowList Type → Row Type → Constraint
class ReadWriteForeignRecord rl row | rl → row where
  readForeignRecordImpl
    ∷ ∀ proxy m
    . Monad m
    ⇒ proxy rl
    → Object Foreign
    → FT m (Builder {} { | row })
  writeForeignRecordImpl ∷ ∀ proxy. proxy rl → Record row → Object Foreign

instance rwForeignRecordNil ∷ ReadWriteForeignRecord RL.Nil () where
  readForeignRecordImpl _ object = do
    forWithIndex_ object \k v → fail
      (ErrorAtProperty k (TypeMismatch "undefined" (tagOf v)))
    pure (identity ∷ Builder _ _)
  writeForeignRecordImpl _ {} = Object.empty

instance rwForeignRecordCons ∷
  ( IsSymbol key
  , R.Cons key focus tail row
  , R.Lacks key tail
  , ReadWriteForeignRecord rlTail tail
  , ReadWriteForeign focus
  ) ⇒
  ReadWriteForeignRecord (RL.Cons key focus rlTail) row where
  readForeignRecordImpl _ object = do
    let focusForeign = lookup key object # maybe undefined identity
    focus ← readForeign focusForeign `catchError`
      (head >>> ErrorAtProperty key >>> fail)
    tail ← readForeignRecordImpl rlTailP (Object.delete key object)
    pure $ Builder.insert keyP focus <<< tail
    where
    key = reflectSymbol keyP
    keyP = Proxy ∷ Proxy key
    rlTailP = Proxy ∷ Proxy rlTail
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
  ⇒ ReadWriteForeignRecord rl row
  ⇒ proxy rl
  → Foreign
  → FT m (Record row)
readForeignRecord _ value = do
  valueObject ← readObject value
  values ← Builder.buildFromScratch <$> readForeignRecordImpl (Proxy ∷ _ rl)
    valueObject
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
  . ReadWriteForeignRecord rl row
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
