module Test.Main where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Effect (Effect)
import Effect.Aff (Aff, error, launchAff_)
import Foreign (F, renderForeignError, unsafeToForeign)
import Test.Spec (it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)

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
main = launchAff_ $ runF $ runSpecT defaultConfig [ consoleReporter ] do
  it "Preserves key ordering when reading" do
    let
      foreignValue = unsafeToForeign { c: "MyString", a: 1, b: 2 }
    pure unit