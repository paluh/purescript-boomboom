module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (elem)
import Data.Int (fromString)
import Data.Maybe (Maybe)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.String (Pattern(..), dropWhile, stripPrefix, takeWhile, toCharArray)
import Global.Unsafe (unsafeStringify)

-- | __D__ from diverging as a' can diverge from a
newtype BoomBoomD tok a' a = BoomBoomD
  { prs ∷ tok → Maybe { a ∷ a, tok ∷ tok }
  , ser ∷ a' → tok
  }
derive instance newtypeBoomBoomD ∷ Newtype (BoomBoomD tok a' a) _
derive instance functorBoomBoomD ∷ Functor (BoomBoomD tok a')

diverge ∷ ∀ a a' tok. (a' → a) → BoomBoom tok a → BoomBoomD tok a' a
diverge d (BoomBoom (BoomBoomD { prs, ser })) = BoomBoomD { prs, ser: d >>> ser }

infixl 5 diverge as >-

instance applyBoomBoomD ∷ (Semigroup tok) ⇒ Apply (BoomBoomD tok a') where
  apply (BoomBoomD b1) (BoomBoomD b2) = BoomBoomD { prs, ser }
    where
    prs t = do
      { a: f, tok: t' } ← b1.prs t
      { a, tok: t'' } ← b2.prs t'
      pure { a: f a, tok: t'' }
    ser a' = b1.ser a' <> b2.ser a'

instance applicativeBoomBoomD ∷ (Monoid tok) ⇒ Applicative (BoomBoomD tok a') where
  pure a = BoomBoomD { prs: pure <<< const { a, tok: mempty }, ser: const mempty }

-- instance altBoomBoomD ∷ Alt (BoomBoomD tok a') where


newtype BoomBoom tok a = BoomBoom (BoomBoomD tok a a)
derive instance newtypeBoomBoom ∷ Newtype (BoomBoom tok a) _

type BoomBoomStr a = BoomBoom String a

int ∷ BoomBoomStr Int
int = BoomBoom $ BoomBoomD $
  { prs: \t → {a: _, tok: _ } <$> (fromString <<< takeDigits $ t) <@> stripDigits t
  , ser: show
  }
  where
  isDigit = (_ `elem` (toCharArray "0123456789"))
  takeDigits = takeWhile isDigit
  stripDigits = dropWhile isDigit

lit ∷ ∀ a. String → BoomBoomD String a Unit
lit s = BoomBoomD $
  { prs: \t → {a: unit, tok: _} <$> stripPrefix (Pattern s) t
  , ser: const s
  }

parse ∷ ∀ a tok. BoomBoom tok a → (tok → Maybe a)
parse (BoomBoom (BoomBoomD { prs })) = (_.a <$> _) <$> prs

serialize ∷ ∀ a tok. BoomBoom tok a → (a → tok)
serialize (BoomBoom (BoomBoomD { ser })) = ser

path :: BoomBoomStr { x :: Int, y :: Int }
path = BoomBoom $
  { x: _, y: _ }
    <$> (_.x >- int)
    <* lit "test"
    <*> (_.y >- int)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ unsafeStringify (parse path "8080test200")
  log (serialize path { x: 300, y: 800 })
