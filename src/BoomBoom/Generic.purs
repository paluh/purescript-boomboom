module BoomBoom.Generic where

import Prelude

import BoomBoom.Prim (BoomBoom, RecordBuilder(..), VariantBuilder, addChoice, addField, buildRecord, buildVariant)
import Data.Either (Either)
import Data.Monoid (class Monoid)
import Data.Record (get)
import Data.Variant (Variant)
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(..), SProxy(..))
import Type.Row (Cons, Nil)

-- | `ir` - input record with `BoomBooms` as fields
-- | `irl` - input row list which "traverses" given record
-- | `tok` - serialization output (parsing input) - required to generate variant prefix
-- | `builder` - final `BoomBoom` builder
class VariantBoomBoom irl ir tok builder | irl → tok, irl → builder where
  variantImpl ∷ RLProxy irl → (∀ n. IsSymbol n ⇒ SProxy n → BoomBoom tok Unit) → {|ir} → builder

instance a_variantSingleton
  ∷ ( RowCons n (BoomBoom tok fb) ir' ir
    , RowCons n fb p p'
    , RowCons n fb s s'
    , IsSymbol n
    , Semigroup tok
    , Eq tok
    )
  ⇒ VariantBoomBoom
      (Cons n (BoomBoom tok fb) Nil)
      ir
      tok
      (VariantBuilder tok (Variant p') (Either (Variant s') tok) (Either (Variant s) tok))
  where
  variantImpl _ toPrefix r = addChoice _n p (get _n r)
    where
    _n = SProxy ∷ SProxy n
    p = toPrefix _n

instance b_variantCons
  ∷ ( RowCons n (BoomBoom tok fb) ir' ir
    , RowCons n fb p p'
    , RowCons n fb s s'
    , IsSymbol n
    , Semigroup tok
    , Eq tok
    , VariantBoomBoom tail ir tok (VariantBuilder tok (Variant p') (Either (Variant s'') tok) (Either (Variant s') tok))
    )
  ⇒ VariantBoomBoom
      (Cons n (BoomBoom tok fb) tail)
      ir
      tok
      (VariantBuilder tok (Variant p') (Either (Variant s'') tok) (Either (Variant s) tok))
  where
  variantImpl _ toPrefix r = addChoice _n p (get _n r) <<< tail
    where
    _n = SProxy ∷ SProxy n
    p = toPrefix _n
    _irl = RLProxy ∷ RLProxy tail
    tail = variantImpl _irl toPrefix r

variant
  ∷ ∀ r rl r' tok
  . RowToList r rl
  ⇒ VariantBoomBoom rl r tok (VariantBuilder tok (Variant r') (Either (Variant r') tok) (Either (Variant ()) tok))
  ⇒ (∀ n. IsSymbol n ⇒ SProxy n → BoomBoom tok Unit)
  → {|r}
  → BoomBoom tok (Variant r')
variant toPrefix r = buildVariant (variantImpl (RLProxy ∷ RLProxy rl) toPrefix r)


-- | `ir` - input record with `BoomBooms` as fields
-- | `irl` - input row list which "traverses" given record
-- | `builder` - final `BoomBoom` builder
class RecordBoomBoom irl ir builder | irl → builder where
  recordImpl ∷ RLProxy irl → {|ir} → builder

instance a_recordNil ∷ (Monoid tok) ⇒ RecordBoomBoom Nil ir (RecordBuilder tok s p p) where
  recordImpl _ _ = id

instance b_recordCons
  ∷ ( RowCons n (BoomBoom tok b) ir' ir
    , RowCons n b p p'
    , RowLacks n p
    , RowCons n b s s'
    , RowLacks n s
    , IsSymbol n
    , Semigroup tok
    , RecordBoomBoom tail ir (RecordBuilder tok {|s'} {|p'} {|p''})
    )
  ⇒ RecordBoomBoom
      (Cons n (BoomBoom tok b) tail)
      ir
      (RecordBuilder tok {|s'} {|p} {|p''})
  where
  recordImpl _ r = addField _n (get _n r) >>> tail
    where
    _n = SProxy ∷ SProxy n
    tail = recordImpl (RLProxy ∷ RLProxy tail) r

record
  ∷ ∀ p r rl tok
  . RowToList r rl
  ⇒ RecordBoomBoom rl r (RecordBuilder tok {|p} {} {|p})
  ⇒ {|r}
  → BoomBoom tok {|p}
record r = buildRecord (recordImpl (RLProxy ∷ RLProxy rl) r)
