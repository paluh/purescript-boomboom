module Main where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array (elem)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Record (get, insert)
import Data.String (Pattern(..), dropWhile, stripPrefix, takeWhile, toCharArray)
import Data.Symbol (reflectSymbol)
import Data.Variant (Variant, default, inj, on)
import Debug.Trace (traceAnyA)
import Global.Unsafe (unsafeStringify)
import Type.Prelude (class IsSymbol, class RowLacks, SProxy(..))

-- | __D__ from diverging as a' can diverge from a
newtype BoomBoomD tok a' a = BoomBoomD
  { prs ∷ tok → Maybe { a ∷ a, tok ∷ tok }
  , ser ∷ a' → Maybe tok
  }
derive instance newtypeBoomBoomD ∷ Newtype (BoomBoomD tok a' a) _
derive instance functorBoomBoomD ∷ Functor (BoomBoomD tok a')

diverge ∷ ∀ a a' tok. (a' → a) → BoomBoom tok a → BoomBoomD tok a' a
diverge d (BoomBoom (BoomBoomD { prs, ser })) = BoomBoomD { prs, ser: d >>> ser }

infixl 5 diverge as >-

divergeMaybe ∷ ∀ a a' tok. (a' → Maybe a) → BoomBoom tok a → BoomBoomD tok a' a
divergeMaybe d (BoomBoom (BoomBoomD { prs, ser })) = BoomBoomD { prs, ser: d >=> ser }

infixl 5 divergeMaybe as >?

instance applyBoomBoomD ∷ (Semigroup tok) ⇒ Apply (BoomBoomD tok a') where
  apply (BoomBoomD b1) (BoomBoomD b2) = BoomBoomD { prs, ser }
    where
    prs t = do
      { a: f, tok: t' } ← b1.prs t
      { a, tok: t'' } ← b2.prs t'
      pure { a: f a, tok: t'' }
    ser a = (<>) <$> b1.ser a <*> b2.ser a

instance applicativeBoomBoomD ∷ (Monoid tok) ⇒ Applicative (BoomBoomD tok a') where
  pure a = BoomBoomD { prs: pure <<< const { a, tok: mempty }, ser: const (Just mempty) }

instance altBoomBoom ∷ (Semigroup tok) ⇒ Alt (BoomBoomD tok a') where
  alt (BoomBoomD b1) (BoomBoomD b2) = BoomBoomD { prs, ser }
    where
    -- | Piece of premature optimization ;-)
    prs tok = case b1.prs tok of
      Nothing → b2.prs tok
      r → r
    ser a = case b1.ser a of
      Nothing → b2.ser a
      r → r

newtype BoomBoomD' tok a r r' = BoomBoomD' (BoomBoomD tok a (r → r'))

instance semigroupoidBoomBoomD' ∷ (Semigroup tok) ⇒ Semigroupoid (BoomBoomD' tok a) where
  compose (BoomBoomD' (BoomBoomD b1)) (BoomBoomD' (BoomBoomD b2)) = BoomBoomD' $ BoomBoomD $
    { prs: \tok → do
        {a: r, tok: tok'} ← b2.prs tok
        {a: r', tok: tok''} ← b1.prs tok'
        pure {a: r' <<< r, tok: tok''}
    , ser: \a → (<>) <$> b1.ser a <*> b2.ser a
    }

addField ∷ ∀ a n r r' s s' tok
  . RowCons n a s s'
  ⇒ RowLacks n s
  ⇒ RowCons n a r r'
  ⇒ RowLacks n r
  ⇒ IsSymbol n
  ⇒ SProxy n
  → BoomBoom tok a
  → BoomBoomD' tok { | s'} { | r } { | r'}
addField p (BoomBoom (BoomBoomD b)) = BoomBoomD' $ BoomBoomD $
  { prs: \t → (\{a, tok} → { a: \r → insert p a r, tok: tok }) <$> b.prs t
  , ser: \r → b.ser (get p r)
  }

buildRecord
  ∷ ∀ r tok
  . BoomBoomD' tok r {} r
  → BoomBoom tok r
buildRecord (BoomBoomD' (BoomBoomD b)) = BoomBoom $ BoomBoomD
  { prs: \tok → do
      {a: r2r, tok: tok'} ← b.prs tok
      pure {a: r2r {}, tok: tok'}
  , ser: b.ser
  }


newtype BoomBoom tok a = BoomBoom (BoomBoomD tok a a)
derive instance newtypeBoomBoom ∷ Newtype (BoomBoom tok a) _


prefix ∷ ∀ a. String → BoomBoom String a → BoomBoomD String a a
prefix s b = lit s *> unwrap b

int ∷ BoomBoom String Int
int = BoomBoom $ BoomBoomD $
  { prs: \t → {a: _, tok: _ } <$> (fromString <<< takeDigits $ t) <@> stripDigits t
  , ser: show >>> Just
  }
  where
  isDigit = (_ `elem` (toCharArray "0123456789"))
  takeDigits = takeWhile isDigit
  stripDigits = dropWhile isDigit

lit ∷ ∀ a. String → BoomBoomD String a Unit
lit s = BoomBoomD $
  { prs: \t → {a: unit, tok: _} <$> stripPrefix (Pattern s) t
  , ser: const (Just s)
  }

parse ∷ ∀ a tok. BoomBoom tok a → (tok → Maybe a)
parse (BoomBoom (BoomBoomD { prs })) = (_.a <$> _) <$> prs

serialize ∷ ∀ a tok. BoomBoom tok a → (a → Maybe tok)
serialize (BoomBoom (BoomBoomD { ser })) = ser

path :: BoomBoom String { x :: Int, y :: Int }
path = BoomBoom $
  { x: _, y: _ }
    <$> _.x >- int
    <* lit "test"
    <*> _.y >- int

data Three = Zero | One Int | Two Int Int

variant n b = lit (reflectSymbol n) *> ((inj n) <$> unv >? b)
  where
  unv =
    default Nothing
    # on n Just

one = One <$> (unone >? int)
  where
  unone (One i) = Just i
  unone _ = Nothing

two = Two <$> (untwo' >? int) <* lit "/" <*> (untwo'' >? int)
  where
  untwo' (Two i _) = Just i
  untwo' _ = Nothing
  untwo'' (Two _ i) = Just i
  untwo'' _ = Nothing

three = BoomBoom $
  (lit "one" *> one) <|> (lit "two" *> two) <|> (lit "zero" *> pure Zero)

record = BoomBoom $
  {x: _, y: _}
  <$> _.x >- int
  <* lit "/"
  <*> _.y >- int

three' :: forall t318.
   BoomBoom String
     (Variant
        ( one :: Int
        , two :: { x :: Int
                 , y :: Int
                 }
        , zero :: Unit
        )
     )
three' = BoomBoom $
  variant (SProxy ∷ SProxy "one") int
  <|> variant (SProxy ∷ SProxy "two") record
  <|> variant (SProxy ∷ SProxy "zero") (BoomBoom $ pure unit)


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ unsafeStringify (parse path "8080test200")
  logShow (serialize path { x: 300, y: 800 })

  logShow (serialize three (Two 8 9))
  logShow (serialize three Zero)
  logShow (serialize three (One 8))

  traceAnyA (parse three "zero")
  traceAnyA (parse three "one8")
  traceAnyA (parse three "two8/9")


  logShow (serialize three' (inj (SProxy ∷ SProxy "two") {x: 8, y: 9}))
  logShow (serialize three' (inj (SProxy ∷ SProxy "zero") unit))
  logShow (serialize three' (inj (SProxy ∷ SProxy "one") 8))

  traceAnyA (parse three' "zero")
  traceAnyA (parse three' "one8")
  traceAnyA (parse three' "two8/9")
