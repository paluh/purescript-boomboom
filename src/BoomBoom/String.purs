module BoomBoom.String where

import Prelude

import BoomBoom.Generic (class VariantBoomBoom)
import BoomBoom.Generic (variant) as Generic
import BoomBoom.Prim (BoomBoom(BoomBoom), BoomBoomD(BoomBoomD), addChoice) as Prim
import BoomBoom.Prim (VariantBuilder)
import Data.Array (singleton, uncons)
import Data.Either (Either)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Type.Prelude (class IsSymbol, class RowToList, SProxy, reflectSymbol)

type Tok = Array String
type BoomBoom a = Prim.BoomBoom Tok a
type BoomBoomD a' a = Prim.BoomBoomD Tok a' a


addChoice
  ∷ forall a r r' s s' n
  . RowCons n a r' r
  ⇒ RowCons n a s s'
  ⇒ IsSymbol n
  ⇒ SProxy n
  → BoomBoom a
  → VariantBuilder
      Tok
      (Variant s')
      (Either (Variant r) Tok)
      (Either (Variant r') Tok)
addChoice p = Prim.addChoice p (_lit p)

variant
  ∷ ∀ r rl r'
  . RowToList r rl
  ⇒ VariantBoomBoom rl r Tok (VariantBuilder Tok (Variant r') (Either (Variant r') Tok) (Either (Variant ()) Tok))
  ⇒ {|r}
  → BoomBoom (Variant r')
variant = Generic.variant _lit

int ∷ BoomBoom Int
int = Prim.BoomBoom $ Prim.BoomBoomD $
  { prs: uncons >=> (\{ head, tail } → { a: _, tok: tail } <$> fromString head)
  , ser: singleton <<< show
  }

litD ∷ ∀ a'. String -> BoomBoomD a' Unit
litD tok = Prim.BoomBoomD
  { prs: uncons >=> \{ head, tail } → if head == tok
      then
        Just { a: unit, tok: tail }
      else
        Nothing
  , ser: const (singleton tok)
  }

lit ∷ String -> BoomBoom Unit
lit = Prim.BoomBoom <<< litD

_litD ∷ ∀ a' n. IsSymbol n ⇒ SProxy n → BoomBoomD a' Unit
_litD = litD <<< reflectSymbol

_lit ∷ ∀ n. IsSymbol n ⇒ SProxy n → BoomBoom Unit
_lit = Prim.BoomBoom <<< _litD

string ∷ BoomBoom String
string = Prim.BoomBoom $ Prim.BoomBoomD $
  { prs: uncons >=> (\{ head, tail } → pure { a: head, tok: tail })
  , ser: singleton
  }

