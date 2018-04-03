module Test.Main where

import Prelude

import Control.Monad.Aff (Fiber, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Test.BoomBoom.Generic as Test.BoomBoom.Generic
import Test.BoomBoom.Generic.Interpret as Test.BoomBoom.Generic.Interpret
import Test.BoomBoom.Strings as Test.BoomBoom.Strings
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)


main ∷
  ∀ eff.
   Eff ( console ∷ CONSOLE, testOutput ∷ TESTOUTPUT , avar ∷ AVAR | eff)
     (Fiber ( console ∷ CONSOLE , testOutput ∷ TESTOUTPUT , avar ∷ AVAR | eff) Unit)
main = launchAff $ do
  liftEff <<< runTest $ do
    Test.BoomBoom.Strings.suite
    Test.BoomBoom.Generic.suite
    Test.BoomBoom.Generic.Interpret.suite
