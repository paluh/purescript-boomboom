module Test.Main where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff.Class (liftEff)
import Test.BoomBoom.String as Test.BoomBoom.String
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)


main = launchAff $ do
  liftEff <<< runTest $ do
    suite "BoomBoom.String" Test.BoomBoom.String.suite
