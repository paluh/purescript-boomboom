module BoomBoom.Generic.Interpret where

import Prelude

import BoomBoom (BoomBoom)
import BoomBoom (addChoice, addField, buildRecord, buildVariant, CoproductBuilder, ProductBuilder) as B
import BoomBoom.Strings (_lit)
import Data.Either (Either)
import Data.List (List)
import Data.Monoid (class Monoid)
import Data.Record (get)
import Data.Record as Data.Record
import Data.Record.Builder as Record.Builder
import Data.Variant (Variant, inj)
import Type.Prelude (class IsSymbol, class RowLacks, class RowToList, RLProxy(RLProxy), SProxy(SProxy))
import Type.Row (Cons, Nil, kind RowList)

-- | Here we are defining machinery for "tree of records" interpretation.
-- | As we are defining this tree on the type level and operating on it
-- | through type classes this stragegy should not suffer
-- | from the "expression problem" and should be extensible -
-- | you should be able to extend given node set
-- | but also interpretation set for predefined nodes.
-- |
-- | This interpreting machinery allows to run transformations similar to
-- | catamorphisms but also less elegant/simple scenarios which require
-- | information about parent node too.
-- |

data R a = R (Record a)
data V a = V (Record a)
data B t = B t

-- | Only root of our tree is not a field of a record.
-- | This kind allows us to represent this option.
foreign import kind Field
foreign import data Root ∷ Field
-- | Allow case analysis on constrctor:
-- | 1. Parent name - it will be changed to parent
-- | type soon. It provides a way to
-- | break usual "locality" of *morphisms
-- | algebras which is required in case of
-- | BoomBooms generation.
-- | 2. Field name which we are in.
foreign import data Field ∷ Symbol → Symbol → Field

data InterpretProxy (interpreter ∷ Symbol) (field ∷ Field) = InterpretProxy

data MapProxy (interpreter ∷ Symbol) (parent ∷ Symbol) = MapProxy

class MapRecord interpreter parent il i o | interpreter il → o where
  mapRecord ∷ MapProxy interpreter parent → RLProxy il → Record i → o

instance a_mapRecordNil
  ∷ Category builder
  ⇒ MapRecord interpreter parent Nil i (builder o o) where
  mapRecord _ _ r = id

instance b_mapRecordConsNil
  ∷ ( Interpret interpreter (Field parent fieldName) field (builder o o')
    , IsSymbol interpreter
    , IsSymbol fieldName
    , RowCons fieldName field i' i
    , Semigroupoid builder
    )
  ⇒ MapRecord interpreter parent (Cons fieldName field Nil) i (builder o o') where
  mapRecord _ _ r = interpretImpl context (Data.Record.get _n r)
    where
    _n = SProxy ∷ SProxy fieldName
    context = InterpretProxy ∷ InterpretProxy interpreter (Field parent fieldName)

instance c_mapRecordCons
  ∷ ( Interpret interpreter (Field parent fieldName) field (builder o o')
    , IsSymbol interpreter
    , IsSymbol fieldName
    , RowCons fieldName field i' i
    , MapRecord interpreter parent tail i (builder o' o'')
    , Semigroupoid builder
    )
  ⇒ MapRecord interpreter parent (Cons fieldName field tail) i (builder o o'')
  where
  mapRecord mp _ r = interpretImpl (InterpretProxy ∷ InterpretProxy interpreter (Field parent fieldName)) (Data.Record.get _n r) >>> tail
    where
    _n = SProxy ∷ SProxy fieldName
    tail = mapRecord mp (RLProxy ∷ RLProxy tail) r

class Interpret interpreter field a b | interpreter field a → b where
  interpretImpl ∷ InterpretProxy interpreter field  → a → b

instance interpretR
  ∷ ( Alg interpreter field (R r) r' r''
    , RowToList r rl
    , MapRecord interpreter "R" rl r r')
  ⇒ Interpret interpreter field (R r) r''
  where
  interpretImpl _ (R r) = alg (AlgProxy ∷ AlgProxy interpreter field (R r)) $ (mapRecord (MapProxy ∷ MapProxy interpreter "R") (RLProxy ∷ RLProxy rl) r)

instance interpretV
  ∷ ( Alg interpreter field (V r) r' r''
    , RowToList r rl
    , MapRecord interpreter "V" rl r r')
  ⇒ Interpret interpreter field (V r) r''
  where
  interpretImpl _ (V r) = alg (AlgProxy ∷ AlgProxy interpreter field (V r)) $ (mapRecord (MapProxy ∷ MapProxy interpreter "V") (RLProxy ∷ RLProxy rl) r)

instance interpretB
  ∷ (Alg interpreter field (B a) a a')
  ⇒ Interpret interpreter field (B a) a'
  where
  interpretImpl _ (B a) = alg (AlgProxy ∷ AlgProxy interpreter field (B a)) $ a

-- | We are storing here:
-- | * name of our interpreter
-- | * field information (parent constructor name + field name)
-- | * original term type (like in paramorphism)
data AlgProxy (interpreter ∷ Symbol) (field ∷ Field) (term ∷ Type) = AlgProxy

-- | I'm not sure about these functional dependencies but without them we have a problem...
class Alg interpreter field term a b | interpreter field term → a, interpreter field term a → b where
  alg ∷ AlgProxy interpreter field term → a → b

-- | "unwrap" interpreter which just
-- | drops "R", "V" and "B" construtors

instance algUnwrapRootR ∷ Alg "unwrap" Root (R o) (Record.Builder.Builder {} {|r}) {|r} where
  alg _ r = Record.Builder.build r {}

instance algUnwrapRootV ∷ Alg "unwrap" Root (V o) (Record.Builder.Builder {} {|r}) {|r} where
  alg _ r = Record.Builder.build r {}

instance algUnwrapRootB ∷ Alg "unwrap" Root (B o) a a where
  alg _ a = a

instance algUnwrapFieldV
  ∷ ( IsSymbol fieldName
    , RowLacks fieldName prs
    , RowCons fieldName {|r} prs prs'
    )
  ⇒ Alg "unwrap" (Field parent fieldName) (V o) (Record.Builder.Builder {} {|r}) (Record.Builder.Builder {|prs} {|prs'})
  where
  alg _ r = Record.Builder.insert (SProxy ∷ SProxy fieldName) (Record.Builder.build r {})

instance algUnwrapFieldR
  ∷ ( IsSymbol fieldName
    , RowLacks fieldName prs
    , RowCons fieldName {|r} prs prs'
    )
  ⇒ Alg "unwrap" (Field parent fieldName) (R o) (Record.Builder.Builder {} {|r}) (Record.Builder.Builder {|prs} {|prs'})
  where
  alg _ r = Record.Builder.insert (SProxy ∷ SProxy fieldName) (Record.Builder.build r {})

instance algUnwrapFieldB
  ∷ ( IsSymbol fieldName
    , RowCons fieldName a prs prs'
    , RowLacks fieldName prs
    )
  ⇒ Alg "unwrap" (Field parent fieldName) (B o) a (Record.Builder.Builder {|prs} {|prs'})
  where
  alg _ a = Record.Builder.insert (SProxy ∷ SProxy fieldName) a

-- | "boomboom" interpreter which builds a BoomBoom from our tree
-- | where `V` represents variant, `R` represents record and `B` holds
-- | `BoomBoom`.
instance algBoomBoomRootR ∷ Alg "boomboom" Root (R o) (B.ProductBuilder tok r {} r) (BoomBoom tok r) where
  alg _ r = B.buildRecord r

instance algBoomBoomRootV ∷ Alg "boomboom" Root (V o) (B.CoproductBuilder tok (Variant r) (Either (Variant r) tok) (Either (Variant ()) tok)) (BoomBoom tok (Variant r)) where
  alg _ r = B.buildVariant r

instance algBoomBoomRootB ∷ Alg "boomboom" Root (B o) (BoomBoom tok a) (BoomBoom tok a) where
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
  ⇒ AddField "R" name (BoomBoom tok a) (B.ProductBuilder tok {|s} {|p} {|p'}) where
  addField _ _ a = B.addField (SProxy ∷ SProxy name) a

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
  ⇒ AddField "V" name (BoomBoom tok a) (B.CoproductBuilder tok (Variant s) (Either (Variant r) tok) (Either (Variant r') tok))
  where
  addField _ _ b = B.addChoice (SProxy ∷ SProxy name) (prefix (SProxy ∷ SProxy name)) b

class Prefix tok where
  prefix ∷ ∀ name. (IsSymbol name) ⇒ SProxy name → BoomBoom tok Unit

instance prefixString ∷ Prefix (List String) where
  prefix = _lit

instance algBoomBoomFieldR
  ∷ (AddField parent fieldName (BoomBoom tok {|r}) b)
  ⇒ Alg "boomboom" (Field parent fieldName) (R o) (B.ProductBuilder tok {|r} {} {|r}) b
  where
  alg _ r = addField  (SProxy ∷ SProxy parent) (SProxy ∷ SProxy fieldName) (B.buildRecord r)

instance algBoomBoomFieldV
  ∷ (AddField parent fieldName (BoomBoom tok (Variant r)) b)
  ⇒ Alg "boomboom" (Field parent fieldName) (V o) (B.CoproductBuilder tok (Variant r) (Either (Variant r) tok) (Either (Variant ()) tok)) b
  where
  alg _ r = addField (SProxy ∷ SProxy parent) (SProxy ∷ SProxy fieldName) (B.buildVariant r)

instance algBoomBoomFieldB
  ∷ (AddField parent fieldName (BoomBoom tok a) b)
  ⇒ Alg "boomboom" (Field parent fieldName) (B o) (BoomBoom tok a) b
  where
    alg _ a = addField (SProxy ∷ SProxy parent) (SProxy ∷ SProxy fieldName) a

newtype ApplicativeCat appl cat a b = ApplicativeCat (appl (cat a b))

instance semigroupoidApplicativeCat ∷ (Apply appl, Semigroupoid cat) ⇒ Semigroupoid (ApplicativeCat appl cat) where
  compose (ApplicativeCat ac1) (ApplicativeCat ac2) = ApplicativeCat $ (<<<) <$> ac1 <*> ac2 

type ReaderCat v cat a b = ApplicativeCat ((→) v) cat a b

-- | "builder" interpreter - it produces helper function or record
-- | which can be used to produce value for serialization. In other
-- | words it simplifies nested variants generation. Check
-- | `tests/BoomBoom/Generic/Intepret.purs` for examples.
instance algBuilderRootR
  ∷ ( RowToList output ol
    , SameLabels ol builder
    )
  ⇒ Alg "builder" Root (R o) (ApplicativeCat ((→) {|builder}) Record.Builder.Builder {} {|output}) ({|builder} → {|output})
  where
    alg _ (ApplicativeCat r2rb) = \r → (Record.Builder.build (r2rb r) {})

instance algBuilderRootV
  ∷ ( RowToList builder bl
    , SameLabels bl input
    )
  ⇒ Alg
    "builder"
    Root
    (V o)
    (ApplicativeCat ((→) (Variant input → Variant input)) Record.Builder.Builder {} {|builder})
    {|builder}
  where
    alg _ (ApplicativeCat v2rb) = Record.Builder.build (v2rb id) {}

instance algBuilderRB
  ∷ ( RowCons fieldName a builder builder'
    , RowLacks fieldName builder
    , IsSymbol fieldName
    , RowCons fieldName a output output'
    , RowLacks fieldName output
    )
  ⇒ Alg
    "builder"
    (Field "R" fieldName)
    (B o)
    (BoomBoom tok a)
    (ApplicativeCat ((→) {|builder'}) Record.Builder.Builder {|output} {|output'})
  where
    alg _ _ = ApplicativeCat (\i → Record.Builder.insert _fieldName (get _fieldName i))
      where
        _fieldName = SProxy ∷ SProxy fieldName

instance algBuilderRR
  ∷ ( RowCons fieldName {|subbuilder} builder builder'
    , RowLacks fieldName builder
    , IsSymbol fieldName
    , RowCons fieldName {|suboutput} output output'
    , RowLacks fieldName output
    )
  ⇒ Alg
    "builder"
    (Field "R" fieldName)
    (R o)
    (ApplicativeCat ((→) {|subbuilder}) Record.Builder.Builder {} {|suboutput})
    (ApplicativeCat ((→) {|builder'}) Record.Builder.Builder {|output} {|output'})
  where
    alg _ (ApplicativeCat i2rb) =
      ApplicativeCat (\i → Record.Builder.insert _fieldName (toSubrecord (get _fieldName i)))
      where
        _fieldName = SProxy ∷ SProxy fieldName
        toSubrecord i = Record.Builder.build (i2rb i) {}

instance algBuilderRV
  ∷ ( RowCons fieldName ({|subbuilder} → Variant v) builder builder'
    , RowLacks fieldName builder
    , IsSymbol fieldName
    , RowCons fieldName (Variant v) output output'
    , RowLacks fieldName output
    , RowToList subbuilder bl
    , SameLabels bl v
    )
  ⇒ Alg
    "builder"
    (Field "R" fieldName)
    (V o)
    (ApplicativeCat ((→) (Variant v → Variant v)) Record.Builder.Builder {} {|subbuilder})
    (ApplicativeCat ((→) {|builder'}) Record.Builder.Builder {|output} {|output'})
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

instance algBuilderVB
  ∷ ( RowCons fieldName a v v'
    , RowLacks fieldName v
    , IsSymbol fieldName
    , RowCons fieldName (a → result) r r'
    , RowLacks fieldName r
    )
  ⇒ Alg
    "builder"
    (Field "V" fieldName)
    (B o)
    (BoomBoom tok a)
    (ApplicativeCat ((→) (Variant v' → result)) Record.Builder.Builder {|r} {|r'})
  where
    alg _ _ = ApplicativeCat (\v2r → Record.Builder.insert _fieldName (v2r <<< inj _fieldName))
      where
        _fieldName = SProxy ∷ SProxy fieldName

instance algBuilderVR
  ∷ ( RowCons fieldName ({|subbuilder} → result) output output'
    , RowLacks fieldName output
    , IsSymbol fieldName
    , RowCons fieldName {|suboutput} v v'
    , RowLacks fieldName v
    , RowToList suboutput ol
    , SameLabels ol subbuilder
    )
  ⇒ Alg
    "builder"
    (Field "V" fieldName)
    (R o)
    (ApplicativeCat ((→) {|subbuilder}) Record.Builder.Builder {} {|suboutput})
    (ApplicativeCat ((→) (Variant v' → result)) Record.Builder.Builder {|output} {|output'})
  where
    alg _ (ApplicativeCat i2rb) =
      ApplicativeCat (\v2r → Record.Builder.insert _fieldName (\r → v2r (inj _fieldName $ toSubrecord r)))
      where
        _fieldName = SProxy ∷ SProxy fieldName
        toSubrecord i = Record.Builder.build (i2rb i) {}

instance algBuilderVV
  ∷ ( RowCons fieldName a v v'
    , RowLacks fieldName v
    , IsSymbol fieldName
    , RowCons fieldName {|r} n n'
    , RowLacks fieldName n
    )
  ⇒ Alg
    "builder"
    (Field "V" fieldName)
    (V o)
    (ApplicativeCat ((→) (a → result)) Record.Builder.Builder {} {|r})
    (ApplicativeCat ((→) (Variant v' → result)) Record.Builder.Builder {|n} {|n'})
  where
    alg _ (ApplicativeCat v2rb) =
      ApplicativeCat (\v2r → Record.Builder.insert _fieldName (Record.Builder.build (v2rb (v2r <<< inj _fieldName)) {}))
      where
        _fieldName = SProxy ∷ SProxy fieldName

-- -- | If your set of labels is known and you can provide RowList
-- -- | with it you can restrict your input "open row" to it.
class SameLabels (list ∷ RowList) (row ∷ # Type) | list → row
instance sameLabelsNil ∷ SameLabels Nil ()
instance sameLabelsCons ∷ (RowCons name a row' row,  SameLabels tail row') ⇒ SameLabels (Cons name x tail) row

interpret ∷ ∀ a b interpreter. IsSymbol interpreter ⇒ Interpret interpreter Root a b ⇒ SProxy interpreter → a → b
interpret _ = interpretImpl (InterpretProxy ∷ InterpretProxy interpreter Root)
