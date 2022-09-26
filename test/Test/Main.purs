module Test.Main where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Foreign (F, renderForeignError, unsafeToForeign)
import Foreign.ReadWrite (undefined, writeForeign)
import Test.Spec (it, pending')
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec, runSpecT)
import Unsafe.Reference (unsafeRefEq)

-- TODO: TESTS ARE WIP

type Record1 =
  { a ∷ Int
  , b ∷ Int
  , c ∷ String
  }

runF ∷ ∀ a. F (Aff a) → Aff a
runF = runExcept >>> case _ of
  Right a → a
  Left err → throwError (error $ foldMap renderForeignError err)

main ∷ Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  pending' "Preserves key ordering when reading" do
    let
      foreignValue = unsafeToForeign { c: "MyString", a: 1, b: 2 }
    pure unit