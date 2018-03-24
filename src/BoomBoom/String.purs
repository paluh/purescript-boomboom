module BoomBoom.String where

import Prelude

import BoomBoom.Prim (BoomBoom(..), BoomBoomD(BoomBoomD), lit) as Prim
import Data.Array (singleton, uncons)
import Data.Int (fromString)

type BoomBoom a = Prim.BoomBoom (Array String) a
type BoomBoomD a' a = Prim.BoomBoomD (Array String) a' a

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
