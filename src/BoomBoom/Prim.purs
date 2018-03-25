module BoomBoom.Prim where

import Prelude

import Control.Alt (class Alt, (<|>))
import Data.Either (Either(Right, Left))
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Record (get, insert)
import Data.Variant (Variant, inj, on)
import Partial.Unsafe (unsafeCrashWith)
import Type.Prelude (class IsSymbol, class RowLacks, SProxy)

-- | Our core type - nearly an iso:
-- | `{ ser: a → tok, prs: tok → Maybe a }`
newtype BoomBoom tok a = BoomBoom (BoomBoomD tok a a)
derive instance newtypeBoomBoom ∷ Newtype (BoomBoom tok a) _

-- | __D__ from diverging as `a'` can diverge from `a`.
-- | It is enough to express `Applicative` for parsing
-- | and two `Semigroupoids` in both directions (`ser` and `prs`).
newtype BoomBoomD tok a' a = BoomBoomD
  -- | Should I wrap this function in some parser type?
  { prs ∷ tok → Maybe { a ∷ a, tok ∷ tok }
  , ser ∷ a' → tok
  }
derive instance newtypeBoomBoomD ∷ Newtype (BoomBoomD tok a' a) _
derive instance functorBoomBoomD ∷ Functor (BoomBoomD tok a')

-- | `divergeA` together with `BoomBoomD` `Applicative`
-- | instance form quite nice API to create by hand
-- | `BoomBooms` for records (or other product types):
-- |
-- | recordB ∷ BoomBoom String {x :: Int, y :: Int}
-- | recordB = BoomBoom $
-- |   {x: _, y: _}
-- |   <$> _.x >- int
-- |   <* lit "/"
-- |   <*> _.y >- int
-- |
-- | It can be tedious and leaves responsibility to accordingly
-- | pick and create elements of a product. For example I could
-- | easily replace `_.x` with `_.y` (and get two `_.y`) by mistake
-- | and it won't be detected by compiler.
-- |
-- | There is helper defined down below which handles record
-- | case more securely.
divergeA ∷ ∀ a a' tok. (a' → a) → BoomBoom tok a → BoomBoomD tok a' a
divergeA d (BoomBoom (BoomBoomD { prs, ser })) = BoomBoomD { prs, ser: d >>> ser }

infixl 5 divergeA as >-

instance applyBoomBoomD ∷ (Semigroup tok) ⇒ Apply (BoomBoomD tok a') where
  apply (BoomBoomD b1) (BoomBoomD b2) = BoomBoomD { prs, ser }
    where
    prs t = do
      { a: f, tok: t' } ← b1.prs t
      { a, tok: t'' } ← b2.prs t'
      pure { a: f a, tok: t'' }
    ser = (<>) <$> b1.ser <*> b2.ser

instance applicativeBoomBoomD ∷ (Monoid tok) ⇒ Applicative (BoomBoomD tok a') where
  pure a = BoomBoomD { prs: pure <<< const { a, tok: mempty }, ser: const mempty }

-- | This `Alt` instance is also somewhat dangerous - it allows
-- | you to define inconsistent `BoomBoom` in case for example
-- | of your sum type so you can get `tok's` `mempty` as a result
-- | of serialization which is not parsable.
instance altBoomBoom ∷ (Monoid tok) ⇒ Alt (BoomBoomD tok a') where
  alt (BoomBoomD b1) (BoomBoomD b2) = BoomBoomD { prs, ser }
    where
    -- | Piece of premature optimization ;-)
    prs tok = case b1.prs tok of
      Nothing → b2.prs tok
      r → r
    ser = (<>) <$> b1.ser <*> b2.ser

-- | Enter the world of two categories which fully keep track of
-- | `BoomBoom` divergence and allow us define nice combinators
-- | for variant and record build up.
-- |
-- | You should not really worry about these two types because
-- | their are underlying machinery for higher level
-- | combinators `addChoice`/`buildVariant` and `addField`/`buildRecord`.
newtype RecordBuilder tok a r r' = RecordBuilder (BoomBoomD tok a (r → r'))

instance semigroupoidRecordBuilder ∷ (Semigroup tok) ⇒ Semigroupoid (RecordBuilder tok a) where
  compose (RecordBuilder (BoomBoomD b1)) (RecordBuilder (BoomBoomD b2)) = RecordBuilder $ BoomBoomD
    { prs: \tok → do
        { a: r, tok: tok' } ← b2.prs tok
        { a: r', tok: tok'' } ← b1.prs tok'
        pure {a: r' <<< r, tok: tok''}
    , ser: (<>) <$> b2.ser <*> b1.ser
    }

instance categoryRecordBuilder ∷ (Monoid tok) ⇒ Category (RecordBuilder tok a) where
  id = RecordBuilder $ BoomBoomD
    { prs: \tok → pure { a: id, tok }
    , ser: const mempty
    }

-- | For sure serializer could be expressed much easier
-- | (like: `a → (b → t) → t`) but we want to use
-- | `BooBoomD` for this and we've got `ser: a'-> tok`
-- | with moving `a'` part to our disposition. So welcome in:
-- |        __The Callback Hell__
-- |
-- |...don't worry it is just plumbing ;-)
newtype VariantBuilder tok a v v' = VariantBuilder (BoomBoomD tok ((v → v') → tok) a)

instance semigroupoidVariantBuilder ∷ (Semigroup tok) ⇒ Semigroupoid (VariantBuilder tok a) where
  compose (VariantBuilder (BoomBoomD b1)) (VariantBuilder (BoomBoomD b2)) = VariantBuilder $ BoomBoomD
    { prs: \tok → b2.prs tok <|> b1.prs tok
    , ser: \a2c2t → b2.ser (\a2b → b1.ser (\b2c → a2c2t (a2b >>> b2c)))
    }

-- | Our category allows us to step by step
-- | contract our variant:
-- |
-- |     (((Either a tok → Either b tok) → tok) → tok)
-- | >>> (((Either b tok → Either c tok) → tok) → tok)
-- | =   (((Either a tok → Either c tok) → tok) → tok)
-- |
-- | Where `a, b, c` is our contracting variant
-- | series.
-- |
addChoice
  ∷ forall a r r' s s' n tok
  . RowCons n a r' r
  ⇒ RowCons n a s s'
  ⇒ IsSymbol n
  ⇒ Semigroup tok
  ⇒ Eq tok
  ⇒ SProxy n
  -- | Please provide unique prefix for this choice
  -- | so it can be parsed back.
  → BoomBoom tok Unit
  → BoomBoom tok a
  -- | Don't worry about this result signature
  -- | just finish your variant build up chain with
  -- | `buildVariant` call and it will turn into nice
  -- | and friendly `BoomBoom`.
  → VariantBuilder tok (Variant s') (Either (Variant r) tok) (Either (Variant r') tok)
addChoice p prefix (BoomBoom (BoomBoomD b)) = VariantBuilder $ choice
  where
  (BoomBoom (BoomBoomD prefix')) = prefix
  choice = BoomBoomD
    { prs: \t → do
        {a, tok } ← prefix'.prs t
        {a: a', tok: tok'} ← b.prs tok
        pure { a: inj p a', tok: tok' }
    , ser: \a2eb2tok → a2eb2tok (case _ of
        Left v → on p (Right <<< (const (prefix'.ser unit) <> b.ser)) Left v
        Right tok → Right tok)
    }

-- | We are getting our final serializer here - a function which returns
-- | `Either Void tok` so we can just pick right thing from it:
-- |
-- | ser ∷ (((Either (Variant r) tok → Either (Variant ()) tok) → tok) → tok)
buildVariant
  ∷ ∀ tok r
  . VariantBuilder tok (Variant r) (Either (Variant r) tok) (Either (Variant ()) tok)
  → BoomBoom tok (Variant r)
buildVariant (VariantBuilder (BoomBoomD {prs, ser})) = BoomBoom $ BoomBoomD
  { prs
  , ser: \v → ser (\a2t2t → (case (a2t2t (Left v)) of
      (Left _) → unsafeCrashWith "BoomBoom.Prim.buildVariant: empty variant?"
      (Right tok) → tok))
  }

addField ∷ ∀ a n p p' s s' tok
  . RowCons n a s s'
  ⇒ RowLacks n s
  ⇒ RowCons n a p p'
  ⇒ RowLacks n p
  ⇒ IsSymbol n
  ⇒ SProxy n
  → BoomBoom tok a
  → RecordBuilder tok {|s'} {|p} {|p'}
addField p (BoomBoom (BoomBoomD b)) = RecordBuilder $ BoomBoomD
  -- | XXX: Let's move to Data.Record.Builder
  -- | in next release
  { prs: \t → b.prs t <#> \{a, tok} →
      { a: \r → insert p a r, tok }
  , ser: \r → b.ser (get p r)
  }

buildRecord
  ∷ ∀ r tok
  . RecordBuilder tok r {} r
  → BoomBoom tok r
buildRecord (RecordBuilder (BoomBoomD b)) = BoomBoom $ BoomBoomD
  { prs: \tok → do
      {a: r2r, tok: tok'} ← b.prs tok
      pure {a: r2r {}, tok: tok'}
  , ser: b.ser
  }

serialize ∷ ∀ a tok. BoomBoom tok a → (a → tok)
serialize (BoomBoom (BoomBoomD { ser })) = ser

parse ∷ ∀ a tok. BoomBoom tok a → (tok → Maybe a)
parse (BoomBoom (BoomBoomD { prs })) = (_.a <$> _) <$> prs

-- | Currently only single helper so... it is still here ;-)
-- | `Newtype` `wrap/unwrap`
xrap
  ∷ ∀ a n tok
  . Newtype n a
  ⇒ BoomBoom tok a
  → BoomBoom tok n
xrap b = BoomBoom $ wrap <$> unwrap >- b

