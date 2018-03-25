module BoomBoom.String where

import Prelude

import BoomBoom as BB
import Data.Int (fromString)
import Data.List (List, Pattern(..), singleton, uncons)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.String as String
import Data.Variant (Variant)
import Type.Prelude (class IsSymbol, class RowLacks, SProxy, reflectSymbol)

type Tok = List String
type BoomBoom a = BB.BoomBoom Tok a
type BoomBoomD a' a = BB.BoomBoomD Tok a' a
type BoomBoomR r = BB.BoomBoomR Tok r
type BoomBoomV r = BB.BoomBoomV Tok r

addStringPrefix ::
  forall a.
  String.Pattern ->
  BB.BoomBoom String a ->
  BB.BoomBoom String a
addStringPrefix l (BB.BoomBoom (BB.BoomBoomD this)) = BB.BoomBoom $ BB.BoomBoomD
  { prs: \tok -> String.stripPrefix l tok >>= this.prs
  , ser: \a -> this.ser a <> unwrap l
  }

addStringSuffix ::
  forall a.
  String.Pattern ->
  BB.BoomBoom String a ->
  BB.BoomBoom String a
addStringSuffix l (BB.BoomBoom (BB.BoomBoomD this)) = BB.BoomBoom $ BB.BoomBoomD
  { prs: this.prs >=> \{ a, tok } ->
      String.stripPrefix l tok <#> \tok' -> { a, tok: tok' }
  , ser: \a -> this.ser a <> unwrap l
  }

addListPrefix ::
  forall p a.
    Eq p =>
  List.Pattern p ->
  BB.BoomBoom (List.List p) a ->
  BB.BoomBoom (List.List p) a
addListPrefix l (BB.BoomBoom (BB.BoomBoomD this)) = BB.BoomBoom $ BB.BoomBoomD
  { prs: \tok -> List.stripPrefix l tok >>= this.prs
  , ser: \a -> unwrap l <> this.ser a
  }


addPrefixedCase
  ∷ forall a r u r' n
  . RowCons n a r' r
  ⇒ RowLacks n r'
  ⇒ Union r' u r
  ⇒ IsSymbol n
  ⇒ SProxy n
  → BoomBoom a
  → BoomBoom (Variant r')
  → BoomBoom (Variant r)
addPrefixedCase p = BB.addCase p <<< addListPrefix
  (Pattern (singleton (reflectSymbol p)))

int ∷ BoomBoom Int
int = BB.BoomBoom $ BB.BoomBoomD $
  { prs: uncons >=> (\{ head, tail } → { a: _, tok: tail } <$> fromString head)
  , ser: singleton <<< show
  }

litD ∷ ∀ a'. String -> BoomBoomD a' Unit
litD tok = BB.BoomBoomD
  { prs: uncons >=> \{ head, tail } → if head == tok
      then
        Just { a: unit, tok: tail }
      else
        Nothing
  , ser: const (singleton tok)
  }

lit ∷ String -> BoomBoom Unit
lit = BB.BoomBoom <<< litD

_litD ∷ ∀ a' n. IsSymbol n ⇒ SProxy n → BoomBoomD a' Unit
_litD = litD <<< reflectSymbol

_lit ∷ ∀ n. IsSymbol n ⇒ SProxy n → BoomBoom Unit
_lit = BB.BoomBoom <<< _litD

string ∷ BoomBoom String
string = BB.BoomBoom $ BB.BoomBoomD $
  { prs: uncons >=> (\{ head, tail } → pure { a: head, tok: tail })
  , ser: singleton
  }
