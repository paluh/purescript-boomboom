module BoomBoom.String where

import Prelude

import BoomBoom.Prim (BoomBoom(BoomBoom), BoomBoomD(BoomBoomD), BoomBoomSerFn, addChoice, lit) as Prim
import Data.Array (singleton, uncons)
import Data.Either (Either)
import Data.Int (fromString)
import Data.Variant (Variant)
import Type.Prelude (class IsSymbol, SProxy, reflectSymbol)

type BoomBoom a = Prim.BoomBoom (Array String) a
type BoomBoomD a' a = Prim.BoomBoomD (Array String) a' a

addChoice
  ∷ forall a r r' s s' n tok
  . RowCons n a r' r
  ⇒ RowCons n a s s'
  ⇒ IsSymbol n
  ⇒ SProxy n
  → BoomBoom a
  → Prim.BoomBoomSerFn
      (Array String)
      (Variant s')
      (Either (Variant r) (Array String))
      (Either (Variant r') (Array String))
addChoice p = Prim.addChoice p (reflectSymbol >>> singleton)

int ∷ BoomBoom Int
int = Prim.BoomBoom $ Prim.BoomBoomD $
  { prs: uncons >=> (\{ head, tail } → { a: _, tok: tail } <$> fromString head)
  , ser: singleton <<< show
  }

lit :: ∀ a'. String -> BoomBoomD a' Unit
lit s = Prim.lit [s]

string ∷ BoomBoom String
string = Prim.BoomBoom $ Prim.BoomBoomD $
  { prs: uncons >=> (\{ head, tail } → pure { a: head, tok: tail })
  , ser: singleton
  }
