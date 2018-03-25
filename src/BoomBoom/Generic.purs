module BoomBoom.Generic where

import Prelude

import BoomBoom.Prim (BoomBoom, VariantBuilder(..), addChoice, buildVariant)
import Data.Either (Either)
import Data.Record (get)
import Data.Variant (Variant)
import Type.Prelude (class IsSymbol, class RowToList, RLProxy(..), SProxy(..))
import Type.Row (Cons, Nil)

-- | `ir` - input record with `BoomBooms` as fields
-- | `irl` - input row list which "traverses" given record
-- | `tok` - serialization output (parsing input) - required to generate variant prefix
-- | `builder` - final `BoomBoom` builder
class BoomBoom irl ir tok builder | irl → tok, irl → builder where
  variantImpl ∷ RLProxy irl → (∀ n. IsSymbol n ⇒ SProxy n → BoomBoom tok Unit) → {|ir} → builder

instance a_boomBoomSingleton
  ∷ ( RowCons n (BoomBoom tok fb) ir' ir
    , RowCons n fb p p'
    , RowCons n fb s s'
    , IsSymbol n
    , Semigroup tok
    , Eq tok
    )
  ⇒ BoomBoom
      (Cons n (BoomBoom tok fb) Nil)
      ir
      tok
      (VariantBuilder tok (Variant p') (Either (Variant s') tok) (Either (Variant s) tok))
  where
  variantImpl _ toPrefix r = addChoice _n p (get _n r)
    where
    _n = SProxy ∷ SProxy n
    p = toPrefix _n

instance b_boomBoomCons
  ∷ ( RowCons n (BoomBoom tok fb) ir' ir
    , RowCons n fb p p'
    , RowCons n fb s s'
    , IsSymbol n
    , Semigroup tok
    , Eq tok
    , BoomBoom tail ir tok (VariantBuilder tok (Variant p') (Either (Variant s'') tok) (Either (Variant s') tok))
    )
  ⇒ BoomBoom
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
  ⇒ BoomBoom rl r tok (VariantBuilder tok (Variant r') (Either (Variant r') tok) (Either (Variant ()) tok))
  ⇒ (∀ n. IsSymbol n ⇒ SProxy n → BoomBoom tok Unit)
  → {|r}
  → BoomBoom tok (Variant r')
variant toPrefix r = buildVariant (variantImpl (RLProxy ∷ RLProxy rl) toPrefix r)

-- 
--     )
--   ⇒ BoomBoom ((Cons n b) tail) ir n tok (VariantBuilder tok p' b b')
--   where
--   record _ toPrefix r = addField _n p (get _n r) >>> tailBuilder
--     where
--     _n = SProxy ∷ SProxy n
--     p = toPrefix _n
--     tailBuilder = record (RLProxy ∷ RLProxy tail) r toPrefix
-- 
-- -- record ∷ ∀ r. (RowToList r rl, BoomBoom rl r b) ⇒ r → (SProxy s → BoomBoom tok Unit) → b
-- -- record r prefix = recordImpl (RLProxy ∷ RLProxy rl) r b
