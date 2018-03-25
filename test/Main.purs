module Test.Main where

import Prelude

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Test.BoomBoom.Generic as Test.BoomBoom.Generic
import Test.BoomBoom.String as Test.BoomBoom.String
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: Eff ( console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR ) Unit
main = launchAff_ $ do
  liftEff <<< runTest $ do
    Test.BoomBoom.String.suite
    Test.BoomBoom.Generic.suite
