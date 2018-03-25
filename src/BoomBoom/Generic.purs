module BoomBoom.Generic where

import BoomBoom

import Data.Monoid (class Monoid, class Semigroup)
import Data.Record as R
import Data.Variant (Variant)
import Prelude (class Eq, Unit)
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(..), SProxy(..))
import Type.Row (Cons, Nil)

-- | `ir` - input record with `BoomBooms` as fields
-- | `irl` - input row list which "traverses" given record
-- | `tok` - serialization output (parsing input) - required to generate variant prefix
-- | `builder` - final `BoomBoom` builder
class VariantBoomBoom irl ir tok cases | irl → ir tok cases where
  variantImpl ∷ RLProxy irl → (∀ n. IsSymbol n ⇒ SProxy n → BoomBoom tok Unit) → Record ir → BoomBoom tok (Variant cases)

instance variantNil :: VariantBoomBoom Nil ir tok () where
  variantImpl _ _ _ = baseCase

instance variantCons ::
  ( IsSymbol s
  , Semigroup tok
  , RowCons s (BoomBoom tok t) ir' ir
  , RowCons s t cases cases'
  , RowLacks s cases
  , Union cases u cases'
  , VariantBoomBoom irl ir tok cases
  ) => VariantBoomBoom (Cons s (BoomBoom tok t) irl) ir tok cases' where
    variantImpl _ toPrefix ir = addCase s (toPrefix s `seqR` R.get s ir)
      (variantImpl (RLProxy :: RLProxy irl) toPrefix ir)
      where s = SProxy :: SProxy s

variant
  ∷ ∀ irl ir tok cases
  . RowToList ir irl
  ⇒ VariantBoomBoom irl ir tok cases
  ⇒ (∀ n. IsSymbol n ⇒ SProxy n → BoomBoom tok Unit)
  → Record ir
  → BoomBoomV tok cases
variant toPrefix r = variantImpl (RLProxy ∷ RLProxy irl) toPrefix r

-- | `ir` - input record with `BoomBooms` as fields
-- | `irl` - input row list which "traverses" given record
-- | `builder` - final `BoomBoom` builder
class RecordBoomBoom irl ir tok fields | irl → ir tok fields where
  recordImpl ∷ RLProxy irl → Record ir → BoomBoom tok (Record fields)

instance recordNil :: (Eq tok, Monoid tok) => RecordBoomBoom Nil ir tok () where
  recordImpl _ _ = end

instance recordCons ::
  ( IsSymbol s
  , RowCons s (BoomBoom tok t) ir' ir
  , RowCons s t fields fields'
  , RowLacks s fields
  , Semigroup tok
  , RecordBoomBoom irl ir tok fields
  ) => RecordBoomBoom (Cons s (BoomBoom tok t) irl) ir tok fields' where
    recordImpl _ r = addFieldBefore s (R.get s r) tail
      where
      s = SProxy ∷ SProxy s
      tail = recordImpl (RLProxy ∷ RLProxy irl) r

record
  ∷ ∀ irl ir tok fields
  . RowToList ir irl
  ⇒ RecordBoomBoom irl ir tok fields
  ⇒ Record ir
  → BoomBoomR tok fields
record = recordImpl (RLProxy ∷ RLProxy irl)
