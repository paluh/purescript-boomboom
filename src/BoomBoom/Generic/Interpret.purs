module BoomBoom.Generic.Interpret where

import Prelude

import BoomBoom.Prim (BoomBoom(..), RecordBuilder(..), addField, buildRecord)
import BoomBoom.String (int)
import Data.Either (Either)
import Data.Monoid (class Monoid)
import Data.Record as Data.Record
import Data.Record.Builder as Record.Builder

import Data.Variant (Variant)
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(..), SProxy(..))
import Type.Row (Cons, Nil)

-- | Only root of our tree is not a field of a record.
-- | This kind allows us to represent this option.
foreign import kind FieldName
foreign import data Root ∷ FieldName
foreign import data Field ∷ Symbol → FieldName

data FunProxy (interpreter ∷ Symbol) (fieldName ∷ FieldName) = FunProxy

class MapRecord interpreter rl i o | rl → o where
  mapRecord ∷ SProxy interpreter → RLProxy rl → Record i → o

instance b_mapRecordConsNil
  ∷ ( Fun interpreter (Field fieldName) field (builder o o')
    , IsSymbol interpreter
    , IsSymbol fieldName
    , RowCons fieldName field i' i
    , Semigroupoid builder
    )
  ⇒ MapRecord interpreter (Cons fieldName field Nil) i (builder o o') where
  mapRecord _ _ r = fun (FunProxy ∷ FunProxy interpreter (Field fieldName)) (Data.Record.get _n r)
    where
    _n = SProxy ∷ SProxy fieldName

instance a_mapRecordNil
  ∷ Category builder
  ⇒ MapRecord interpreter Nil i (builder o o) where
  mapRecord _ _ r = id

instance c_mapRecordCons
  ∷ ( Fun interpreter (Field fieldName) field (builder o o')
    , IsSymbol interpreter
    , IsSymbol fieldName
    , RowCons fieldName field i' i
    , MapRecord interpreter tail i (builder o' o'')
    , Semigroupoid builder
    )
  ⇒ MapRecord interpreter (Cons fieldName field tail) i (builder o o'')
  where
  mapRecord interpreter _ r = fun (FunProxy ∷ FunProxy interpreter (Field fieldName)) (Data.Record.get _n r) >>> tail
    where
    _n = SProxy ∷ SProxy fieldName
    tail = mapRecord interpreter (RLProxy ∷ RLProxy tail) r

data R r = R (Record r)
data B a = B a

class Fun interpreter fieldName a b | interpreter fieldName a → b where
  fun ∷ FunProxy interpreter fieldName → a → b

instance funR
  ∷ ( Alg interpreter "R" fieldName r' r''
    , RowToList r rl
    , MapRecord interpreter rl r r')
  ⇒ Fun interpreter fieldName (R r) r''
  where
  fun _ (R r) = alg (AlgProxy ∷ AlgProxy interpreter "R" fieldName) $ (mapRecord (SProxy ∷ SProxy interpreter) (RLProxy ∷ RLProxy rl) r)

instance funB
  ∷ (Alg interpreter "B" fieldName a a')
  -- | Fun "unwrap" (Field "x") (BoomBoom) (REcordBuilder)
  -- | Alg "unwrap "B" (Field "x") (BoomBoom) (REcordBuilder)
  ⇒ Fun interpreter fieldName (B a) a'
  where
  fun _ (B a) = alg (AlgProxy ∷ AlgProxy interpreter "B" fieldName) $ a

data AlgProxy (interpreter ∷ Symbol) (constructor ∷ Symbol) (fieldName ∷ FieldName) = AlgProxy

class Alg interpreter constructor fieldName a b | interpreter constructor fieldName → a, interpreter fieldName constructor a → b where
  alg ∷ AlgProxy interpreter constructor fieldName → a → b

instance algUnwrapRootR ∷ Alg "unwrap" "R" Root (Record.Builder.Builder {} {|r}) {|r} where
  alg _ r = Record.Builder.build r {}

instance algUnwrapRootB ∷ Alg "unwrap" "B" Root a a where
  alg _ a = a

instance algUnwrapFieldR
  ∷ ( IsSymbol fieldName
    , RowLacks fieldName prs
    , RowCons fieldName {|r} prs prs'
    )
  ⇒ Alg "unwrap" "R" (Field fieldName) (Record.Builder.Builder {} {|r}) (Record.Builder.Builder {|prs} {|prs'})
  where
  alg _ r = Record.Builder.insert (SProxy ∷ SProxy fieldName) (Record.Builder.build r {})

instance algUnwrapFieldB
  ∷ ( IsSymbol fieldName
    , RowCons fieldName a prs prs'
    , RowLacks fieldName prs
    )
  ⇒ Alg "unwrap" "B" (Field fieldName) a (Record.Builder.Builder {|prs} {|prs'})
  where
  alg _ a = Record.Builder.insert (SProxy ∷ SProxy fieldName) a


t' = fun (FunProxy ∷ FunProxy "unwrap" Root) (B int)

t'' = fun (FunProxy ∷ FunProxy "unwrap" Root) (R { })

t''' = fun (FunProxy ∷ FunProxy "unwrap" Root) (R { x: B int })

t'''' :: { z :: { a :: Int
        }
 , y :: Int
 , x :: Int
 }t'''' = fun (FunProxy ∷ FunProxy "unwrap" Root) (R { x: B 8, y: B 8, z: R { a: B 9 }})


-- data AlgProxy (interpreter ∷ Symbol) (constructor ∷ Symbol) (fieldName ∷ FieldName) = AlgProxy

-- class Alg interpreter constructor fieldName a b | interpreter constructor fieldName → a, interpreter fieldName constructor a → b where
--  alg ∷ AlgProxy interpreter constructor fieldName → a → b

instance algBoomBoomRootR ∷ Alg "boomboom" "R" Root (RecordBuilder tok r {} r) (BoomBoom tok r) where
  alg _ r = buildRecord r

instance algBoomBoomRootB ∷ Alg "boomboom" "B" Root (BoomBoom tok a) (BoomBoom tok a) where
  alg _ a = a

instance algBoomBoomFieldR
  ∷ ( RowCons fieldName r ser' ser
    , RowLacks fieldName ser'
    , IsSymbol fieldName
    , RowLacks fieldName prs
    , RowCons fieldName r prs prs'
    )
  ⇒ Alg "boomboom" "R" (Field fieldName) (RecordBuilder tok r {} r) (RecordBuilder tok {|ser} {|prs} {|prs'})
  where
  alg _ r = addField (SProxy ∷ SProxy fieldName) (buildRecord r)

instance algBoomBoomFieldB
  ∷ ( RowCons fieldName a ser' ser
    , RowLacks fieldName ser'
    , IsSymbol fieldName
    , RowCons fieldName a prs prs'
    , RowLacks fieldName prs
    )
  ⇒ Alg "boomboom" "B" (Field fieldName) (BoomBoom tok a) (RecordBuilder tok {|ser} {|prs} {|prs'})
  where
  alg _ a = addField (SProxy ∷ SProxy fieldName) a


-- 
-- -- instance alg
-- 
-- -- instance funB ∷ Fun "unwrap" (B a) a where
-- --   fun _ (B a) = a
-- 
-- -- -- instance funIntR ∷ Fun (R Int)
-- -- --   fun (
-- -- -- 
-- -- 
-- 
-- -- type Interpret =
-- --   { r ∷ i → o
-- --   , b ∷ i → o
-- --   }
-- 
-- 
-- -- data MonoidC d a b = MonoidC d
-- -- 
-- -- instance monoidCategory ∷ (Semigroup m) ⇒ Semigroupoid (MonoidC m) where
-- --   compose (MonoidC m1) (MonoidC m2) = MonoidC (m2 <> m1)
-- -- 
-- -- newtype Counter = Counter { variants ∷ Int, records ∷ Int, boombooms ∷ Int }
-- -- 
-- -- instance semigroupCounter ∷ Semigroup Counter where
-- --   append (Counter c1) (Counter c2) = Counter { variants, records, boombooms }
-- --     where
-- --     variants = c1.variants + c2.variants
-- --     records = c1.records + c2.records
-- --     boombooms = c1.boombooms + c2.boombooms
-- -- 
-- 
-- 
-- 
-- -- instance cataRecord ∷ (Alg (f (Record r')) a, Functor f, RowToList r rl, MapRecord rl r r') ⇒ Alg (f (Record r)) a where
-- --   alg fr = alg (map (mapRecord (RLProxy ∷ RLProxy rl)) fr)
-- 
-- 
-- -- class (Alg (f b) b, Alg a b) ⇐ Cata (f a) b where
-- --   cata ∷ fa → a
-- -- 
-- -- instance Cata
-- -- instance cata
-- 
-- 
-- -- newtype VariantNode r = VariantNode (Record r)
-- -- newtype RecordNode r = RecordNode (Record r)
-- -- 
-- -- variantNode = VariantNode
-- -- recordNode = RecordNode
-- -- 
-- -- class BoomBoomInterpret node boomboom where
-- --   boomBoomInterpret ∷ node → boomboom
-- -- 
-- -- instance recordBoomBoomInterpret ∷ BoomBoomInterpret (RecordNode r) (BoomBoom
-- -- 
-- -- 
-- -- variant
-- --   ∷ ∀ r rl r' tok
-- --   . RowToList r rl
-- --   ⇒ VariantBoomBoom rl r tok (VariantBuilder tok (Variant r') (Either (Variant r') tok) (Either (Variant ()) tok))
-- --   ⇒ (∀ n. IsSymbol n ⇒ SProxy n → BoomBoom tok Unit)
-- --   → VariantNode r
-- --   → BoomBoom tok (Variant r')
-- -- variant toPrefix r = buildVariant (variantImpl (RLProxy ∷ RLProxy rl) toPrefix r)
-- -- 
-- -- record
-- --   ∷ ∀ p r rl tok
-- --   . RowToList r rl
-- --   ⇒ RecordBoomBoom rl r (RecordBuilder tok {|p} {} {|p})
-- --   ⇒ RecordNode r
-- --   → BoomBoom tok {|p}
-- -- record r = buildRecord (recordImpl (RLProxy ∷ RLProxy rl) r)
