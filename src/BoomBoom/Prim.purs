module BoomBoom where

import Prelude

import Control.Apply (lift2)
import Data.Functor.Invariant (class Invariant, imap)
import Data.Maybe (Maybe(..))
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Profunctor (class Profunctor, dimap)
import Data.Record as R
import Data.Variant (Variant)
import Data.Variant as V
import Type.Prelude (class IsSymbol, class RowLacks, class TypeEquals, SProxy, from)

-- | Our core type - nearly an iso:
-- | `{ ser: a → tok, prs: tok → Maybe { a :: a, tok :: tok } }`
newtype BoomBoom tok a = BoomBoom (BoomBoomD tok a a)
derive instance newtypeBoomBoom ∷ Newtype (BoomBoom tok a) _

instance invariantBoomBoom :: Invariant (BoomBoom tok) where
  imap f g (BoomBoom b) = BoomBoom (dimap g f b)

instance semigroupBoomBoom :: (Semigroup tok, TypeEquals a Unit) => Semigroup (BoomBoom tok a) where
  append (BoomBoom (BoomBoomD b1)) (BoomBoom (BoomBoomD b2)) = BoomBoom $ BoomBoomD
    { prs: b1.prs >=> _.tok >>> b2.prs
    , ser: lift2 (<>) b1.ser b2.ser
    }

instance monoidBoomBoom :: (Monoid tok, TypeEquals a Unit) => Monoid (BoomBoom tok a) where
  mempty = BoomBoom $ BoomBoomD
    { prs: Just <<< { a: from unit, tok: _ }
    , ser: mempty
    }

-- | Sequence two BoomBooms, keeping the right result.
seqR ::
  forall tok a.
    Semigroup tok =>
  BoomBoom tok Unit ->
  BoomBoom tok a ->
  BoomBoom tok a
seqR (BoomBoom (BoomBoomD b1)) (BoomBoom (BoomBoomD b2)) = BoomBoom $ BoomBoomD
  { prs: b1.prs >=> _.tok >>> b2.prs
  , ser: \a -> b1.ser unit <> b2.ser a
  }

-- | Sequence two BoomBooms, keeping the left result.
seqL ::
  forall tok a.
    Semigroup tok =>
  BoomBoom tok a ->
  BoomBoom tok Unit ->
  BoomBoom tok a
seqL (BoomBoom (BoomBoomD b1)) (BoomBoom (BoomBoomD b2)) = BoomBoom $ BoomBoomD
  { prs: b1.prs >=> \{ a, tok } -> b2.prs tok <#> \{ tok: tok' } -> { a, tok: tok' }
  , ser: \a -> b1.ser a <> b2.ser unit
  }

-- | __D__ from diverging as `a'` can diverge from `a`.
newtype BoomBoomD tok a' a = BoomBoomD
  -- | Should I wrap this function in some parser type?
  { prs ∷ tok → Maybe { a ∷ a, tok ∷ tok }
  , ser ∷ a' → tok
  }
derive instance newtypeBoomBoomD ∷ Newtype (BoomBoomD tok a' a) _
derive instance functorBoomBoomD ∷ Functor (BoomBoomD tok a')

instance profunctorBoomBoomD :: Profunctor (BoomBoomD tok) where
  dimap f g (BoomBoomD b) = BoomBoomD
    { prs: b.prs >>> map \{ a, tok } -> { a: g a, tok }
    , ser: f >>> b.ser
    }

type BoomBoomR tok r = BoomBoom tok (Record r)

addFieldAfter ::
  forall tok s t r r'.
    IsSymbol s =>
    RowCons s t r r' =>
    RowLacks s r =>
    Semigroup tok =>
  SProxy s ->
  BoomBoom tok t ->
  BoomBoomR tok r ->
  BoomBoomR tok r'
addFieldAfter s (BoomBoom (BoomBoomD this)) (BoomBoom (BoomBoomD rest)) =
  BoomBoom $ BoomBoomD
    { prs: rest.prs >=> \{ a: a', tok } ->
        this.prs tok <#> \{ a, tok: tok' } ->
          { a: R.insert s a a', tok: tok' }
    , ser: \a -> rest.ser (R.delete s a) <> this.ser (R.get s a)
    }

addFieldBefore ::
  forall tok s t r r'.
    IsSymbol s =>
    RowCons s t r r' =>
    RowLacks s r =>
    Semigroup tok =>
  SProxy s ->
  BoomBoom tok t ->
  BoomBoomR tok r ->
  BoomBoomR tok r'
addFieldBefore s (BoomBoom (BoomBoomD this)) (BoomBoom (BoomBoomD rest)) =
  BoomBoom $ BoomBoomD
    { prs: this.prs >=> \{ a, tok } ->
        rest.prs tok <#> \{ a: a', tok: tok' } ->
          { a: R.insert s a a', tok: tok' }
    , ser: \a -> this.ser (R.get s a) <> rest.ser (R.delete s a)
    }

end :: forall tok. Eq tok => Monoid tok => BoomBoomR tok ()
end = BoomBoom $ BoomBoomD
  { prs: \tok -> if tok == mempty then Just { a: {}, tok: mempty } else Nothing
  , ser: \{} -> mempty
  }

exhaustive :: forall tok a. Eq tok => Monoid tok => BoomBoom tok a -> BoomBoom tok a
exhaustive b = b `seqL` imap (const unit) (const {}) end

type BoomBoomV tok r = BoomBoom tok (Variant r)

addCase ::
  forall tok s t r e r'.
    IsSymbol s =>
    RowCons s t r r' =>
    RowLacks s r =>
    Union r e r' =>
  SProxy s ->
  BoomBoom tok t ->
  BoomBoomV tok r ->
  BoomBoomV tok r'
addCase s (BoomBoom (BoomBoomD this)) (BoomBoom (BoomBoomD rest)) =
  BoomBoom $ BoomBoomD
    { prs: \tok -> case rest.prs tok of
        Nothing -> this.prs tok <#>
          \{ a, tok: tok' } -> { a: V.inj s a, tok: tok' }
        Just { a, tok: tok' } -> Just { a: V.expand a, tok: tok' }
    , ser: V.on s this.ser rest.ser
    }

baseCase :: forall tok.
  BoomBoomV tok ()
baseCase = BoomBoom $ BoomBoomD
  { prs: const Nothing
  , ser: V.case_
  }

serialize ∷ ∀ a tok. BoomBoom tok a → (a → tok)
serialize (BoomBoom (BoomBoomD { ser })) = ser

parse ∷ ∀ a tok. BoomBoom tok a → (tok → Maybe a)
parse (BoomBoom (BoomBoomD { prs })) = (_.a <$> _) <$> prs

-- | `Newtype` `wrap/unwrap`
xrap
  ∷ ∀ a n tok
  . Newtype n a
  ⇒ BoomBoom tok a
  → BoomBoom tok n
xrap = imap wrap unwrap
