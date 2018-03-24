module Main where

import Prelude

import Control.Alt (class Alt, (<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log, logShow)
import Data.Array (elem)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Record (get, insert)
import Data.String (Pattern(..), dropWhile, stripPrefix, takeWhile, toCharArray)
import Data.Symbol (reflectSymbol)
import Data.Variant (Variant, case_, default, inj, on)
import Debug.Trace (traceAnyA)
import Global.Unsafe (unsafeStringify)
import Type.Prelude (class IsSymbol, class RowLacks, SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

-- | __D__ from diverging as a' can diverge from a
newtype BoomBoomD tok' tok a' a = BoomBoomD
  { prs ∷ tok → Maybe { a ∷ a, tok ∷ tok }
  , ser ∷ a' → tok'
  }
derive instance newtypeBoomBoomD ∷ Newtype (BoomBoomD tok' tok a' a) _
derive instance functorBoomBoomD ∷ Functor (BoomBoomD tok' tok a')

diverge ∷ ∀ a a' tok. (a' → a) → BoomBoom tok a → BoomBoomD tok tok a' a
diverge d (BoomBoom (BoomBoomD { prs, ser })) = BoomBoomD { prs, ser: d >>> ser }

infixl 5 diverge as >-

instance applyBoomBoomD ∷ (Semigroup tok) ⇒ Apply (BoomBoomD tok tok a') where
  apply (BoomBoomD b1) (BoomBoomD b2) = BoomBoomD { prs, ser }
    where
    prs t = do
      { a: f, tok: t' } ← b1.prs t
      { a, tok: t'' } ← b2.prs t'
      pure { a: f a, tok: t'' }
    ser = (<>) <$> b1.ser <*> b2.ser

instance applicativeBoomBoomD ∷ (Monoid tok) ⇒ Applicative (BoomBoomD tok tok a') where
  pure a = BoomBoomD { prs: pure <<< const { a, tok: mempty }, ser: const mempty }

instance altBoomBoom ∷ (Semigroup tok) ⇒ Alt (BoomBoomD tok tok a') where
  alt (BoomBoomD b1) (BoomBoomD b2) = BoomBoomD { prs, ser }
    where
    -- | Piece of premature optimization ;-)
    prs tok = case b1.prs tok of
      Nothing → b2.prs tok
      r → r
    ser = b1.ser

newtype BoomBoomD' tok a r r' = BoomBoomD' (BoomBoomD tok tok a (r → r'))

instance semigroupoidBoomBoomD' ∷ (Semigroup tok) ⇒ Semigroupoid (BoomBoomD' tok a) where
  compose (BoomBoomD' (BoomBoomD b1)) (BoomBoomD' (BoomBoomD b2)) = BoomBoomD' $ BoomBoomD $
    { prs: \tok → do
        {a: r, tok: tok'} ← b2.prs tok
        {a: r', tok: tok''} ← b1.prs tok'
        pure {a: r' <<< r, tok: tok''}
    , ser: (<>) <$> b1.ser <*> b2.ser
    }

instance categoryBoomBoomD' ∷ (Monoid tok) ⇒ Category (BoomBoomD' tok a) where
  id = BoomBoomD' $ BoomBoomD $
    { prs: \tok → pure { a: id, tok }
    , ser: const mempty
    }

-- onB
--   ∷ ∀ tok r' r a n
--   . RowCons n a r r'
--   ⇒ IsSymbol n
--   ⇒ SProxy n
--   → BoomBoom tok a
--   → BoomBoom tok (Variant r)
--   → BoomBoom tok (Variant r')
-- onB p (BoomBoom (BoomBoomD b)) (BoomBoom (BoomBoomD v)) = BoomBoom $ BoomBoomD $
--   { prs: \t → case b.prs t of
--       Nothing → v.prs t
--       Just {a, tok} → Just { a: inj p a, tok }
--   , ser: \a → (on p b.ser v.ser) a
--   }

newtype BoomBoomD'' tok a r r' = BoomBoomD'' (BoomBoomD ((r' → tok) → tok) tok r a)
instance semigroupoidBoomBoomD'' ∷ (Semigroup tok) ⇒ Semigroupoid (BoomBoomD'' tok a) where
  compose (BoomBoomD'' (BoomBoomD b1)) (BoomBoomD'' (BoomBoomD b2)) = BoomBoomD'' $ BoomBoomD $
    { prs: \tok → b1.prs tok <|> b2.prs tok
    , ser: \a c2t →
        b2.ser a \b →
          b1.ser b \c →
            c2t c
    }

addChoice
  ∷ forall t561 t578 t579 a r r' s s' n tok
  . RowCons n a r r'
  ⇒ RowCons n a s s'
  ⇒ IsSymbol n
  ⇒ SProxy n
  → BoomBoom String a
  → BoomBoomD'' String (Variant s') (Variant r') (Variant r)
addChoice p (BoomBoom (BoomBoomD b)) = BoomBoomD'' $ BoomBoomD $
  { prs: stripPrefix (Pattern prefix) >=> b.prs >=> \{a, tok} → pure { a: inj p a, tok }
  , ser: \v c →
      (on p (const prefix <> b.ser) c) v
  }
  where
  prefix = reflectSymbol p

buildVariant
  ∷ ∀ a tok
  . BoomBoomD'' tok a a (Variant ())
  → BoomBoom tok a
buildVariant (BoomBoomD'' (BoomBoomD {prs, ser})) = BoomBoom $ BoomBoomD $
  { prs
  , ser: \v → ser v case_
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
  { prs: \t → b.prs t <#> \{a, tok} →
      { a: \r → insert p a r, tok }
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

newtype BoomBoom tok a = BoomBoom (BoomBoomD tok tok a a)
derive instance newtypeBoomBoom ∷ Newtype (BoomBoom tok a) _

int ∷ BoomBoom String Int
int = BoomBoom $ BoomBoomD $
  { prs: \t → {a: _, tok: _ } <$> (fromString <<< takeDigits $ t) <@> stripDigits t
  , ser: show
  }
  where
  isDigit = (_ `elem` (toCharArray "0123456789"))
  takeDigits = takeWhile isDigit
  stripDigits = dropWhile isDigit

lit ∷ ∀ a. String → BoomBoomD String String a Unit
lit s = BoomBoomD $
  { prs: \t → {a: unit, tok: _} <$> stripPrefix (Pattern s) t
  , ser: const s
  }

parse ∷ ∀ a tok. BoomBoom tok a → (tok → Maybe a)
parse (BoomBoom (BoomBoomD { prs })) = (_.a <$> _) <$> prs

serialize ∷ ∀ a tok. BoomBoom tok a → (a → tok)
serialize (BoomBoom (BoomBoomD { ser })) = ser

record = BoomBoom $
  {x: _, y: _}
  <$> _.x >- int
  <* lit "/"
  <*> _.y >- int

variant'
  = buildVariant
  $ addChoice (SProxy ∷ SProxy "one") int
  >>> addChoice (SProxy ∷ SProxy "two") record
  >>> addChoice (SProxy ∷ SProxy "zero") (BoomBoom $ pure unit)

record'
  = buildRecord
  $ addField (SProxy ∷ SProxy "x") int
  >>> lit' "/"
  >>> addField (SProxy ∷ SProxy "y") int

lit' ∷ ∀ a r. String → BoomBoomD' String a r r
lit' s = BoomBoomD' $ const id <$> lit s


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  logShow (serialize variant' (inj (SProxy ∷ SProxy "zero") unit))
  logShow (serialize variant' (inj (SProxy ∷ SProxy "one") 8))
  logShow (serialize variant' (inj (SProxy ∷ SProxy "two") {x: 8, y: 9}))

  traceAnyA (parse variant' "two8/9")
  traceAnyA (parse variant' "zero")
  traceAnyA (parse variant' "one8")

  logShow (serialize record' {x: 8, y: 9})
  traceAnyA (parse record' "89/88")

