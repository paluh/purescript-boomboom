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
--     shutterStockSearchSuite searchJson
--     shutterStockImageSuite imageJson
--     suite "Routing handles" do
--       test "basic paths" $ do
--         let
--           root = Root
--           login = Login
--           logout = Logout
-- 
--         equal "/login" (toUrl router Login)
--         equal (Just Login) (fromUrl router "/login")
--         equal (Just Logout) (fromUrl router "/logout")
--         equal "/" (toUrl router root)
-- 
-- -- int ∷ BoomBoom String Int
-- -- int = BoomBoom $ BoomBoomD $
-- --   { prs: \t → {a: _, tok: _ } <$> (fromString <<< takeDigits $ t) <@> stripDigits t
-- --   , ser: show
-- --   }
-- --   where
-- --   isDigit = (_ `elem` (toCharArray "0123456789"))
-- --   takeDigits = takeWhile isDigit
-- --   stripDigits = dropWhile isDigit
-- -- 
-- -- lit ∷ ∀ a. String → BoomBoomD String String a Unit
-- -- lit s = BoomBoomD $
-- --   { prs: \t → {a: unit, tok: _} <$> stripPrefix (Pattern s) t
-- --   , ser: const s
-- --   }
-- -- 
-- -- parse ∷ ∀ a tok. BoomBoom tok a → (tok → Maybe a)
-- -- parse (BoomBoom (BoomBoomD { prs })) = (_.a <$> _) <$> prs
-- -- 
-- -- serialize ∷ ∀ a tok. BoomBoom tok a → (a → tok)
-- -- serialize (BoomBoom (BoomBoomD { ser })) = ser
-- -- 
-- -- record = BoomBoom $
-- --   {x: _, y: _}
-- --   <$> _.x >- int
-- --   <* lit "/"
-- --   <*> _.y >- int
-- -- 
-- -- variant'
-- --   = buildVariant
-- --   $ addChoice (SProxy ∷ SProxy "one") int
-- --   >>> addChoice (SProxy ∷ SProxy "two") record
-- --   >>> addChoice (SProxy ∷ SProxy "zero") (BoomBoom $ pure unit)
-- -- 
-- -- record'
-- --   = buildRecord
-- --   $ addField (SProxy ∷ SProxy "x") int
-- --   >>> lit' "/"
-- --   >>> addField (SProxy ∷ SProxy "y") int
-- -- 
-- -- lit' ∷ ∀ a r. String → BoomBoomD' String a r r
-- -- lit' s = BoomBoomD' $ const id <$> lit s
-- -- 
-- -- 
-- -- main :: forall e. Eff (console :: CONSOLE | e) Unit
-- -- main = do
-- --   logShow (serialize variant' (inj (SProxy ∷ SProxy "zero") unit))
-- --   logShow (serialize variant' (inj (SProxy ∷ SProxy "one") 8))
-- --   logShow (serialize variant' (inj (SProxy ∷ SProxy "two") {x: 8, y: 9}))
-- -- 
-- --   traceAnyA (parse variant' "two8/9")
-- --   traceAnyA (parse variant' "zero")
-- --   traceAnyA (parse variant' "one8")
-- -- 
-- --   logShow (serialize record' {x: 8, y: 9})
-- --   traceAnyA (parse record' "89/88")
-- -- 
