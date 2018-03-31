module BoomBoom.Generic.Interpret where

import Prelude

import BoomBoom.Prim (addChoice, addField, buildRecord, buildVariant) as Prim
import BoomBoom.Prim (BoomBoom, RecordBuilder, VariantBuilder)
import BoomBoom.String (_lit)
import Data.Either (Either)
import Data.Monoid (class Monoid)
import Data.Record (get)
import Data.Record as Data.Record
import Data.Record.Builder as Record.Builder
import Data.Variant (Variant, inj)
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(RLProxy), SProxy(SProxy))
import Type.Row (Cons, Nil, kind RowList)

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

-- | "unwrap" interpreter which just
-- | drops "R", "V" and "B" construtors

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
    , Prefix tok
    )
  ⇒ AddField "V" name (BoomBoom tok a) (VariantBuilder tok (Variant s) (Either (Variant r) tok) (Either (Variant r') tok))
  where
  addField _ _ b = Prim.addChoice (SProxy ∷ SProxy name) (prefix (SProxy ∷ SProxy name)) b

class Prefix tok where
  prefix ∷ ∀ name. (IsSymbol name) ⇒ SProxy name → BoomBoom tok Unit

instance prefixString ∷ Prefix (Array String) where
  prefix = _lit

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


newtype ApplicativeCat appl cat a b = ApplicativeCat (appl (cat a b))

instance semigroupoidApplicativeCat ∷ (Apply appl, Semigroupoid cat) ⇒ Semigroupoid (ApplicativeCat appl cat) where
  compose (ApplicativeCat ac1) (ApplicativeCat ac2) = ApplicativeCat $ (<<<) <$> ac1 <*> ac2 

type ReaderCat v cat a b = ApplicativeCat ((→) v) cat a b


instance algVariantsRootR
  ∷ ( RowToList output ol
    , ClosedRow ol builder
    )
  ⇒ Alg "variants" Root "R" (ApplicativeCat ((→) {|builder}) Record.Builder.Builder {} {|output}) ({|builder} → {|output})
  where
    alg _ (ApplicativeCat r2rb) = \r → (Record.Builder.build (r2rb r) {})


instance algVariantsRootV
  ∷ ( RowToList builder bl
    , ClosedRow bl input
    )
  ⇒ Alg
    "variants"
    Root
    "V"
    (ApplicativeCat ((→) (Variant input → Variant input)) Record.Builder.Builder {} {|builder})
    {|builder}
  where
    alg _ (ApplicativeCat v2rb) = Record.Builder.build (v2rb id) {}

instance algVariantsRB
  ∷ ( RowCons fieldName a i i'
    , RowLacks fieldName i
    , IsSymbol fieldName
    , RowCons fieldName a o o'
    , RowLacks fieldName o
    )
  ⇒ Alg
    "variants"
    (Field "R" fieldName)
    "B"
    (BoomBoom tok a)
    (ApplicativeCat ((→) {|i'}) Record.Builder.Builder {|o} {|o'})
  where
    alg _ _ = ApplicativeCat (\i → Record.Builder.insert _fieldName (get _fieldName i))
      where
        _fieldName = SProxy ∷ SProxy fieldName

instance algVariantsRR
  ∷ ( RowCons fieldName {|subbuilder} builder builder'
    , RowLacks fieldName builder
    , IsSymbol fieldName
    , RowCons fieldName {|suboutput} output output'
    , RowLacks fieldName output
    )
  ⇒ Alg
    "variants"
    (Field "R" fieldName)
    "R"
    (ApplicativeCat ((→) {|subbuilder}) Record.Builder.Builder {} {|suboutput})
    (ApplicativeCat ((→) {|builder'}) Record.Builder.Builder {|output} {|output'})
  where
    alg _ (ApplicativeCat i2rb) =
      ApplicativeCat (\i → Record.Builder.insert _fieldName (toSubrecord (get _fieldName i)))
      where
        _fieldName = SProxy ∷ SProxy fieldName
        toSubrecord i = Record.Builder.build (i2rb i) {}

instance algVariantsRV
  ∷ ( RowCons fieldName ({|builder} → Variant v) i i'
    , RowLacks fieldName i
    , IsSymbol fieldName
    , RowCons fieldName (Variant v) o o'
    , RowLacks fieldName o
    , RowToList builder bl
    , ClosedRow bl v
    )
  ⇒ Alg
    "variants"
    (Field "R" fieldName)
    "V"
    (ApplicativeCat ((→) (Variant v → Variant v)) Record.Builder.Builder {} {|builder})
    (ApplicativeCat ((→) {|i'}) Record.Builder.Builder {|o} {|o'})
  where
    alg _ (ApplicativeCat v2rb) = ApplicativeCat useSubvariants
      where
        _fieldName = SProxy ∷ SProxy fieldName
        useSubvariants i =
          let
            onSubvariants = get _fieldName i
            toSubvariants = (Record.Builder.build (v2rb id) {})
          in
            Record.Builder.insert _fieldName (onSubvariants toSubvariants)

instance algVariantsVB
  ∷ ( RowCons fieldName a v v'
    , RowLacks fieldName v
    , IsSymbol fieldName
    , RowCons fieldName (a → result) r r'
    , RowLacks fieldName r
    )
  ⇒ Alg
    "variants"
    (Field "V" fieldName)
    "B"
    (BoomBoom tok a)
    (ApplicativeCat ((→) (Variant v' → result)) Record.Builder.Builder {|r} {|r'})
  where
    alg _ _ = ApplicativeCat (\v2r → Record.Builder.insert _fieldName (v2r <<< inj _fieldName))
      where
        _fieldName = SProxy ∷ SProxy fieldName

instance algVariantsVR
  ∷ ( RowCons fieldName ({|builder} → result) o o'
    , RowLacks fieldName o
    , IsSymbol fieldName
    , RowCons fieldName {|output} v v'
    , RowLacks fieldName v
    , RowToList output ol
    , ClosedRow ol builder
    )
  ⇒ Alg
    "variants"
    (Field "V" fieldName)
    "R"
    (ApplicativeCat ((→) {|builder}) Record.Builder.Builder {} {|output})
    (ApplicativeCat ((→) (Variant v' → result)) Record.Builder.Builder {|o} {|o'})
  where
    alg _ (ApplicativeCat i2rb) =
      ApplicativeCat (\v2r → Record.Builder.insert _fieldName (\r → v2r (inj _fieldName $ toSubrecord r)))
      where
        _fieldName = SProxy ∷ SProxy fieldName
        toSubrecord i = Record.Builder.build (i2rb i) {}

instance algVariantsVV
  ∷ ( RowCons fieldName a v v'
    , RowLacks fieldName v
    , IsSymbol fieldName
    , RowCons fieldName {|r} n n'
    , RowLacks fieldName n
    )
  ⇒ Alg
    "variants"
    (Field "V" fieldName)
    "V"
    (ApplicativeCat ((→) (a → result)) Record.Builder.Builder {} {|r})
    (ApplicativeCat ((→) (Variant v' → result)) Record.Builder.Builder {|n} {|n'})
  where
    alg _ (ApplicativeCat v2rb) = ApplicativeCat (\v2r → Record.Builder.insert _fieldName (Record.Builder.build (v2rb (v2r <<< inj _fieldName)) {}))
      where
        _fieldName = SProxy ∷ SProxy fieldName

-- | If you set of labels are known in other RowList
-- | you can restrict your "open row" to it.
class ClosedRow (list ∷ RowList) (row ∷ # Type) | list → row

instance closedRowNil ∷ ClosedRow Nil ()

instance closedRowCons ∷ (RowCons name a row' row,  ClosedRow tail row') ⇒ ClosedRow (Cons name x tail) row

