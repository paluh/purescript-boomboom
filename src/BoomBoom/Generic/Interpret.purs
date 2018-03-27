module BoomBoom.Generic.Interpret where

import Prelude

import BoomBoom.Prim (BoomBoom(BoomBoom), addChoice, addField, buildRecord, buildVariant) as Prim
import BoomBoom.Prim (BoomBoom, RecordBuilder, VariantBuilder)
import BoomBoom.String (int)
import Data.Either (Either)
import Data.Monoid (class Monoid)
import Data.Record as Data.Record
import Data.Record.Builder as Record.Builder
import Data.Variant (Variant)
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(..), SProxy(..))
import Type.Row (Cons, Nil)

data R r = R (Record r)
data V r = V (Record r)
data B a = B a

-- | Only root of our tree is not a field of a record.
-- | This kind allows us to represent this option.
foreign import kind Field
foreign import data Root ∷ Field
-- | Allow case analysis on constrctor and field name
foreign import data Field ∷ Symbol → Symbol → Field

data FunProxy (interpreter ∷ Symbol) (field ∷ Field) = FunProxy

data MapProxy (interpreter ∷ Symbol) (constructor ∷ Symbol) = MapProxy

class MapRecord interpreter constructor rl i o | interpreter rl → o where
  mapRecord ∷ MapProxy interpreter constructor → RLProxy rl → Record i → o

instance a_mapRecordNil
  ∷ Category builder
  ⇒ MapRecord interpreter constructor Nil i (builder o o) where
  mapRecord _ _ r = id

instance b_mapRecordConsNil
  ∷ ( Fun interpreter (Field constructor fieldName) field (builder o o')
    , IsSymbol interpreter
    , IsSymbol fieldName
    , RowCons fieldName field i' i
    , Semigroupoid builder
    )
  ⇒ MapRecord interpreter constructor (Cons fieldName field Nil) i (builder o o') where
  mapRecord _ _ r = fun (FunProxy ∷ FunProxy interpreter (Field constructor fieldName)) (Data.Record.get _n r)
    where
    _n = SProxy ∷ SProxy fieldName

instance c_mapRecordCons
  ∷ ( Fun interpreter (Field constructor fieldName) field (builder o o')
    , IsSymbol interpreter
    , IsSymbol fieldName
    , RowCons fieldName field i' i
    , MapRecord interpreter constructor tail i (builder o' o'')
    , Semigroupoid builder
    )
  ⇒ MapRecord interpreter constructor (Cons fieldName field tail) i (builder o o'')
  where
  mapRecord mp _ r = fun (FunProxy ∷ FunProxy interpreter (Field constructor fieldName)) (Data.Record.get _n r) >>> tail
    where
    _n = SProxy ∷ SProxy fieldName
    tail = mapRecord mp (RLProxy ∷ RLProxy tail) r

class Fun interpreter field a b | interpreter field a → b where
  fun ∷ FunProxy interpreter field  → a → b

instance funR
  ∷ ( Alg interpreter field "R" r' r''
    , RowToList r rl
    , MapRecord interpreter "R" rl r r')
  ⇒ Fun interpreter field (R r) r''
  where
  fun _ (R r) = alg (AlgProxy ∷ AlgProxy interpreter field "R") $ (mapRecord (MapProxy ∷ MapProxy interpreter "R") (RLProxy ∷ RLProxy rl) r)

instance funV
  ∷ ( Alg interpreter field "V" r' r''
    , RowToList r rl
    , MapRecord interpreter "V" rl r r')
  ⇒ Fun interpreter field (V r) r''
  where
  fun _ (V r) = alg (AlgProxy ∷ AlgProxy interpreter field "V") $ (mapRecord (MapProxy ∷ MapProxy interpreter "V") (RLProxy ∷ RLProxy rl) r)

instance funB
  ∷ (Alg interpreter field "B" a a')
  ⇒ Fun interpreter field (B a) a'
  where
  fun _ (B a) = alg (AlgProxy ∷ AlgProxy interpreter field "B") $ a

data AlgProxy (interpreter ∷ Symbol) (field ∷ Field) (constructor ∷ Symbol) = AlgProxy
-- 
-- -- | I'm not sure if this functional dependencie can be so restrictive
-- -- | but it is not clear how to express empty record cases without
-- -- | the first one
class Alg interpreter field constructor a b | interpreter field constructor → a, interpreter field constructor a → b where
  alg ∷ AlgProxy interpreter field constructor → a → b

instance algUnwrapRootR ∷ Alg "unwrap" Root "R" (Record.Builder.Builder {} {|r}) {|r} where
  alg _ r = Record.Builder.build r {}

instance algUnwrapRootV ∷ Alg "unwrap" Root "V" (Record.Builder.Builder {} {|r}) {|r} where
  alg _ r = Record.Builder.build r {}

instance algUnwrapRootB ∷ Alg "unwrap" Root "B" a a where
  alg _ a = a

instance algUnwrapFieldV
  ∷ ( IsSymbol fieldName
    , RowLacks fieldName prs
    , RowCons fieldName {|r} prs prs'
    )
  ⇒ Alg "unwrap" (Field parent fieldName) "V" (Record.Builder.Builder {} {|r}) (Record.Builder.Builder {|prs} {|prs'})
  where
  alg _ r = Record.Builder.insert (SProxy ∷ SProxy fieldName) (Record.Builder.build r {})

instance algUnwrapFieldR
  ∷ ( IsSymbol fieldName
    , RowLacks fieldName prs
    , RowCons fieldName {|r} prs prs'
    )
  ⇒ Alg "unwrap" (Field parent fieldName) "R" (Record.Builder.Builder {} {|r}) (Record.Builder.Builder {|prs} {|prs'})
  where
  alg _ r = Record.Builder.insert (SProxy ∷ SProxy fieldName) (Record.Builder.build r {})

instance algUnwrapFieldB
  ∷ ( IsSymbol fieldName
    , RowCons fieldName a prs prs'
    , RowLacks fieldName prs
    )
  ⇒ Alg "unwrap" (Field parent fieldName) "B" a (Record.Builder.Builder {|prs} {|prs'})
  where
  alg _ a = Record.Builder.insert (SProxy ∷ SProxy fieldName) a

u' = fun (FunProxy ∷ FunProxy "unwrap" Root) (B int)

u'' = fun (FunProxy ∷ FunProxy "unwrap" Root) (R { })

u''' = fun (FunProxy ∷ FunProxy "unwrap" Root) (R { x: B int })

u'''' = fun (FunProxy ∷ FunProxy "unwrap" Root) (R { x: B 8, y: B 8, z: V { a: B 9 }})


instance algBoomBoomRootR ∷ Alg "boomboom" Root "R" (RecordBuilder tok r {} r) (BoomBoom tok r) where
  alg _ r = Prim.buildRecord r

instance algBoomBoomRootV ∷ Alg "boomboom" Root "V" (VariantBuilder tok (Variant r) (Either (Variant r) tok) (Either (Variant ()) tok)) (BoomBoom tok (Variant r)) where
  alg _ r = Prim.buildVariant r

instance algBoomBoomRootB ∷ Alg "boomboom" Root "B" (BoomBoom tok a) (BoomBoom tok a) where
  alg _ a = a

class AddField parent name a b | parent a → b where
  addField ∷ SProxy parent → SProxy name → a → b

instance addFieldRecord
  ∷ ( RowCons name a s' s
    , RowLacks name s'
    , RowCons name a p p'
    , RowLacks name p
    , IsSymbol name
    )
  ⇒ AddField "R" name (BoomBoom tok a) (RecordBuilder tok {|s} {|p} {|p'}) where
  addField _ _ a = Prim.addField (SProxy ∷ SProxy name) a

instance addFieldVariant
  ∷ ( RowCons name a r' r
    , RowLacks name r'
    , RowCons name a s' s
    , RowLacks name s'
    , IsSymbol name
    , Monoid tok
    , Eq tok
    )
  ⇒ AddField "V" name (BoomBoom tok a) (VariantBuilder tok (Variant s) (Either (Variant r) tok) (Either (Variant r') tok))
  where
  addField _ _ b = Prim.addChoice (SProxy ∷ SProxy name) (Prim.BoomBoom (pure unit)) b

instance algBoomBoomFieldR
  ∷ (AddField parent fieldName (BoomBoom tok {|r}) b)
  ⇒ Alg "boomboom" (Field parent fieldName) "R" (RecordBuilder tok {|r} {} {|r}) b
  where
  alg _ r = addField  (SProxy ∷ SProxy parent) (SProxy ∷ SProxy fieldName) (Prim.buildRecord r)

instance algBoomBoomFieldV
  ∷ (AddField parent fieldName (BoomBoom tok (Variant r)) b)
  ⇒ Alg "boomboom" (Field parent fieldName) "V" (VariantBuilder tok (Variant r) (Either (Variant r) tok) (Either (Variant ()) tok)) b
  where
  alg _ r = addField (SProxy ∷ SProxy parent) (SProxy ∷ SProxy fieldName) (Prim.buildVariant r)

instance algBoomBoomFieldB
  ∷ (AddField parent fieldName (BoomBoom tok a) b)
  ⇒ Alg "boomboom" (Field parent fieldName) "B" (BoomBoom tok a) b
  where
    alg _ a = addField (SProxy ∷ SProxy parent) (SProxy ∷ SProxy fieldName) a


b1 = fun (FunProxy ∷ FunProxy "boomboom" Root) (B int)

b2 = fun (FunProxy ∷ FunProxy "boomboom" Root) (R { })

b3 = fun (FunProxy ∷ FunProxy "boomboom" Root) (R { x: R {} })

b4 = fun (FunProxy ∷ FunProxy "boomboom" Root) (V { x: B int })

b5 = fun (FunProxy ∷ FunProxy "boomboom" Root) (V { y: R {}})

b6 = fun (FunProxy ∷ FunProxy "boomboom" Root) (V { y: R { z: B int, v: B int}})

-- instance algVariantsRootR ∷ Alg "variants" Root "R" (RecordBuilder tok r {} r) (BoomBoom tok r) where
--   alg _ r = Prim.buildRecord r

-- instance algVariantsRootV ∷ Alg "variants" Root "V" (Record.Builder.Builder {} {|r}) {|r} where
--   alg _ r = Record.Builder.build r {}
-- 
-- instance algVaiantsRootB ∷ Alg "variants" Root "B" (BoomBoom tok a) (a → a) where
--   alg _ _ = id




-- -- -- data MonoidC d a b = MonoidC d
-- -- -- 
-- -- -- instance monoidCategory ∷ (Semigroup m) ⇒ Semigroupoid (MonoidC m) where
-- -- --   compose (MonoidC m1) (MonoidC m2) = MonoidC (m2 <> m1)
-- -- -- 
-- -- -- newtype Counter = Counter { variants ∷ Int, records ∷ Int, boombooms ∷ Int }
-- -- -- 
-- -- -- instance semigroupCounter ∷ Semigroup Counter where
-- -- --   append (Counter c1) (Counter c2) = Counter { variants, records, boombooms }
-- -- --     where
-- -- --     variants = c1.variants + c2.variants
-- -- --     records = c1.records + c2.records
-- -- --     boombooms = c1.boombooms + c2.boombooms
-- -- -- 
-- -- 
-- -- 
-- -- 
-- -- -- instance cataRecord ∷ (Alg (f (Record r')) a, Functor f, RowToList r rl, MapRecord rl r r') ⇒ Alg (f (Record r)) a where
-- -- --   alg fr = alg (map (mapRecord (RLProxy ∷ RLProxy rl)) fr)
-- -- 
-- -- 
-- -- -- class (Alg (f b) b, Alg a b) ⇐ Cata (f a) b where
-- -- --   cata ∷ fa → a
-- -- -- 
-- -- -- instance Cata
-- -- -- instance cata
-- -- 
-- -- 
-- -- -- newtype VariantNode r = VariantNode (Record r)
-- -- -- newtype RecordNode r = RecordNode (Record r)
-- -- -- 
-- -- -- variantNode = VariantNode
-- -- -- recordNode = RecordNode
-- -- -- 
-- -- -- class BoomBoomInterpret node boomboom where
-- -- --   boomBoomInterpret ∷ node → boomboom
-- -- -- 
-- -- -- instance recordBoomBoomInterpret ∷ BoomBoomInterpret (RecordNode r) (BoomBoom
-- -- -- 
-- -- -- 
-- -- -- variant
-- -- --   ∷ ∀ r rl r' tok
-- -- --   . RowToList r rl
-- -- --   ⇒ VariantBoomBoom rl r tok (VariantBuilder tok (Variant r') (Either (Variant r') tok) (Either (Variant ()) tok))
-- -- --   ⇒ (∀ n. IsSymbol n ⇒ SProxy n → BoomBoom tok Unit)
-- -- --   → VariantNode r
-- -- --   → BoomBoom tok (Variant r')
-- -- -- variant toPrefix r = buildVariant (variantImpl (RLProxy ∷ RLProxy rl) toPrefix r)
-- -- -- 
-- -- -- record
-- -- --   ∷ ∀ p r rl tok
-- -- --   . RowToList r rl
-- -- --   ⇒ RecordBoomBoom rl r (RecordBuilder tok {|p} {} {|p})
-- -- --   ⇒ RecordNode r
-- -- --   → BoomBoom tok {|p}
-- -- -- record r = buildRecord (recordImpl (RLProxy ∷ RLProxy rl) r)
