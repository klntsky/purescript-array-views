module Test.Main where

import Data.Array as A
import Data.ArrayView as AV
import Data.ArrayView.Internal
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Profunctor.Strong ((***))
import Data.Traversable (class Foldable, class Traversable, for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Prelude (class Applicative, class Apply, class Bind, class Eq, class Functor, class Monad, class Monoid, class Semigroup, class Show, class Ord, Unit, compare, const, discard, flip, eq, map, mod, negate, not, pure, show, unit, ($), (&&), (+), (<), (<$>), (<>), (==), (>), (>=), (>>>))
import Test.Assert (assert, assertEqual, assertThrows)
import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.QuickCheck.Laws.Control (checkApplicative, checkApply, checkBind, checkMonad)
import Test.QuickCheck.Laws.Data (checkEq, checkFoldable, checkFunctor, checkMonoid, checkSemigroup)
import Type.Proxy (Proxy(..), Proxy2(..))

-- * set to true to get verbose logs
debug :: Boolean
debug = false


-- * some boilerplate required to bypass OrphanInstances.
newtype ArbitraryAV a = ArbitraryAV (ArrayView a)

instance arbitraryArbitraryAV :: Arbitrary a => Arbitrary (ArbitraryAV a) where
   arbitrary = ArbitraryAV <$> (fromArray <$> arbitrary)

derive instance newtypeArbitraryAV :: Newtype (ArbitraryAV a) _
derive newtype instance semigroupArbitraryAV :: Semigroup (ArbitraryAV a)
derive newtype instance monoidArbitraryAV :: Monoid (ArbitraryAV a)
derive newtype instance eqArbitraryAV :: Eq a => Eq (ArbitraryAV a)
derive newtype instance foldableArbitraryAV :: Foldable ArbitraryAV
derive newtype instance functorArbitraryAV :: Functor ArbitraryAV
derive newtype instance applyArbitraryAV :: Apply ArbitraryAV
derive newtype instance bindArbitraryAV :: Bind ArbitraryAV
derive newtype instance applicativeArbitraryAV :: Applicative ArbitraryAV
derive newtype instance monadArbitraryAV :: Monad ArbitraryAV
derive newtype instance traversableArbitraryAV :: Traversable ArbitraryAV


main :: Effect Unit
main = do
  checkAssertions
  checkEdgeCases
  checkLaws
  checkArrayToView


checkLaws :: Effect Unit
checkLaws = do
  let prx1 = Proxy :: Proxy (ArbitraryAV Int)
      prx2 = Proxy2 :: Proxy2 ArbitraryAV
  checkSemigroup prx1
  checkMonoid prx1
  checkEq prx1
  checkFoldable prx2
  checkFunctor prx2
  checkApply prx2
  checkBind prx2
  checkMonad prx2
  checkApplicative prx2


-- Covering edge cases
checkEdgeCases :: Effect Unit
checkEdgeCases = do

  let arrs = [ -- check for off-by-one errors
               Tuple [1,2,3,4] [1,2,3,5]
             , Tuple [1,2,3,4] [1,2,3,3]
             , Tuple [1,2,3,5] [1,2,3,4]
             , Tuple [1,2,3,3] [1,2,3,4]

             , Tuple [1,2,3,4] [0,2,3,4]
             , Tuple [1,2,3,4] [2,2,3,4]
             , Tuple [0,2,3,4] [1,2,3,4]
             , Tuple [2,2,3,4] [1,2,3,4]

             , Tuple [1,2,3,4] [1,2,3,4,5]
             , Tuple [1,2,3,4] [1,2,3,4,3]
             , Tuple [1,2,3,4,3] [1,2,3,4]
             , Tuple [1,2,3,4,5] [1,2,3,4]

               -- check that `compare` traverses the structure
               -- in correct order (i.e. left-to-right)
             , Tuple [1,1,1,1,1] [1,0,1,2,1]
             , Tuple [1,1,1,1,1] [1,2,1,0,1]
             , Tuple [1,1,1,1,1] [1,0,1,2,1,1]
             , Tuple [1,1,1,1,1] [1,2,1,0,1,1]

             , Tuple [] []
             , Tuple [] [1]
             , Tuple [] [1,2,3]
             , Tuple [1] []
             , Tuple [1,2,3] []
             ]

  for_ arrs \(Tuple xs ys) -> do
    logDebug ("xs: " <> show xs <> ", " <>
              "ys: " <> show ys)

    -- in Eq instance definition
    assertEqual { expected: xs == ys
                , actual: fromArray xs == fromArray ys }

    -- in Ord instance defintion
    assertEqual { expected: xs `compare` ys
                , actual: fromArray xs `compare` fromArray ys }


checkArrayToView :: Effect Unit
checkArrayToView = do
  let arr = [1,2,3]
  -- check that view converts between Array and ArrayView
  assertEqual { expected: (AV.use (AV.use :: Array Int -> ArrayView Int) ::
                              ArrayView Int -> Array Int) (fromArray arr)
              , actual: arr }
  assertEqual { expected: (AV.use (AV.use :: ArrayView Int -> Array Int) ::
                              Array Int -> ArrayView Int) arr
              , actual: fromArray arr }


checkAssertions :: Effect Unit
checkAssertions = do
  -- Good old assertion testing.
  -- In our case we need to check some properties after slicing, to ensure that
  -- indices are correct.

  -- For all possible lengths...
  for_ (0 A... 12) \len -> do
    let a  = A.range 1 len
        av = AV.range 1 len

    assertEqual { expected: a
                , actual: toArray (fromArray a) }

    logDebug ("-----------------------\n" <>
              " a: "   <> show a <>
              " av: "  <> inspect av)

    -- for all possible indices i & j...
    for_ (-10 A... 10) \i -> do
      for_ (-10 A...10) \j -> do
        -- ...check that slices from i to j are equal
        let aslice = A.slice i j a
            avslice = AV.slice i j av

        logDebug (" len: " <> show len <>
                  " i: "   <> show i <>
                  " j: "   <> show j <>
                  " aslice: " <> show aslice <>
                  " avslice: " <> inspect avslice)

        checkSlices i j avslice aslice

        -- test functions that require additional index
        for_ (-5 A... 10) \ix -> do
          checkWithIndex ix avslice aslice

        -- test functions that require a predicate
        for_ [ (_ > 5)
             , const false
             , const true
             , (\x -> x `mod` 2 == 1)
             , (\x -> x `mod` 2 == 0)
             , (_ < 5) ] \pred -> do
          checkWithPredicate pred avslice aslice

        -- test functions that require two array views
        for_ (0 A... 3) \n -> do
          checkCombinations
            (AV.take n avslice)
            (AV.drop n avslice)
            (A.take n aslice)
            (A.drop n aslice)


checkWithPredicate :: forall a.
                      Eq a => Show a =>
                      (a -> Boolean) -> ArrayView a -> Array a -> Effect Unit
checkWithPredicate pred avslice aslice = do
  -- span
  assertEqual { expected: fixInitRest (A.span pred aslice)
              , actual: AV.span pred avslice }

  -- takeWhile
  assertEquals
    (AV.takeWhile pred avslice)
    (A.takeWhile pred aslice)

  -- dropWhile
  assertEquals
    (AV.dropWhile pred avslice)
    (A.dropWhile pred aslice)

  -- filter
  assertEquals
    (AV.filter pred avslice)
    (A.filter pred aslice)

  -- filterA
  assertEqual { expected: A.filterA (pred >>> A.singleton) aslice
              , actual: map toArray (AV.filterA (pred >>> A.singleton) avslice) }

  -- partition
  assertEqual { expected: fixYesNo (A.partition pred aslice)
              , actual: AV.partition pred avslice }

  -- mapMaybe
  let g n = if pred n then Just n else Nothing
  assertEquals
    (AV.mapMaybe g avslice)
    (A.mapMaybe g aslice)

  -- catMaybes
  assertEquals
    (AV.catMaybes (map g avslice))
    (A.catMaybes (map g aslice))


checkWithIndex :: Int -> ArrayView Int -> Array Int -> Effect Unit
checkWithIndex ix avslice aslice = do
  for_ (-10 A... 10) \i -> do
    -- slice
    assertEquals
      (AV.slice i ix avslice)
      (A.slice  i ix aslice)

  -- take
  assertEquals
    (AV.take ix avslice)
    (A.take ix aslice)

  -- takeEnd
  assertEquals
    (AV.takeEnd ix avslice)
    (A.takeEnd ix aslice)

  -- drop
  assertEquals
    (AV.drop ix avslice)
    (A.drop ix aslice)

  -- dropEnd
  assertEquals
    (AV.dropEnd ix avslice)
    (A.dropEnd ix aslice)

  -- index
  assertEqual { expected: A.index aslice ix
              , actual: AV.index avslice ix }

  -- unsafeIndex
  if ix >= 0 && ix < A.length aslice
    then
    assertEqual { expected: unsafePartial (A.unsafeIndex aslice ix)
                , actual: unsafePartial (AV.unsafeIndex avslice ix) }
    else
    assertThrows \_ ->
    assertEqual { expected: unsafePartial (A.unsafeIndex aslice ix)
                , actual: unsafePartial (AV.unsafeIndex avslice ix) }

  -- elemIndex
  assertEqual { expected: A.elemIndex ix (aslice <> aslice)
              , actual: AV.elemIndex ix  (avslice <> avslice) }

  -- elemLastIndex
  assertEqual { expected: A.elemLastIndex ix (aslice <> aslice)
              , actual: AV.elemLastIndex ix  (avslice <> avslice) }

  -- findIndex
  assertEqual { expected: A.findIndex (_ == ix) (aslice <> aslice)
              , actual: AV.findIndex (_ == ix)  (avslice <> avslice) }

  -- findLastIndex
  assertEqual { expected: A.findIndex (_ == ix) (aslice <> aslice)
              , actual: AV.findIndex (_ == ix)  (avslice <> avslice) }

  -- insertAt
  assertEqualsMaybe
    (AV.insertAt ix ix avslice)
    (A.insertAt ix ix aslice)

  -- deleteAt
  assertEqualsMaybe
    (AV.deleteAt ix avslice)
    (A.deleteAt ix aslice)

  -- updateAt
  assertEqualsMaybe
    (AV.updateAt ix 0 avslice)
    (A.updateAt ix 0 aslice)

  -- updateAtIndices
  let ixs = [Tuple ix 0, Tuple (ix+1) 1]
  assertEquals
    (AV.updateAtIndices ixs avslice)
    (A.updateAtIndices ixs aslice)

  -- modifyAt
  assertEqualsMaybe
    (AV.modifyAt ix (_ + 1) avslice)
    (A.modifyAt ix (_ + 1) aslice)

  -- alterAt
  assertEqualsMaybe
    (AV.alterAt ix pure avslice)
    (A.alterAt ix pure aslice)


checkSlices :: Int -> Int -> ArrayView Int -> Array Int -> Effect Unit
checkSlices i j avslice aslice = do

  -- Eq
  assert (avslice == avslice)

  -- Ord
  assertEqual { expected: aslice `compare` aslice
              , actual: avslice `compare` avslice }
  assertEqual { expected: A.reverse aslice `compare` aslice
              , actual: AV.reverse avslice `compare` avslice }

  -- fromArray <<< toArray == identity
  assertEqual { expected: avslice
              , actual: fromArray (toArray avslice) }

  assertEqual { expected: aslice
              , actual: toArray avslice }

  assertEqual { expected: avslice
              , actual: fromArray aslice }

  -- force
  assertEquals
    (AV.force avslice)
    aslice

  -- null
  assertEqual { expected: A.null aslice
              , actual: AV.null avslice }

  -- length
  assertEqual { expected: A.length aslice
              , actual: AV.length avslice }

  -- cons
  assertEqual { expected: A.cons 0 aslice
              , actual: toArray (AV.cons 0 avslice) }

  -- snoc
  assertEqual { expected: A.snoc aslice 0
              , actual: toArray (AV.snoc avslice 0) }

  -- insert
  for_ (-1 A... 1) \e -> do
    assertEquals
      (AV.insert e avslice)
      (A.insert e aslice)

  -- insertBy
  for_ (-1 A... 1) \e -> do
    assertEquals
      (AV.insertBy (flip compare) e avslice)
      (A.insertBy  (flip compare) e aslice)

  -- head
  assertEqual { expected: A.head aslice
              , actual: AV.head avslice }

  -- last
  assertEqual { expected: A.last aslice
              , actual: AV.last avslice }

  -- tail
  assertEqual { expected: A.tail aslice
              , actual: map toArray (AV.tail avslice) }

  -- init
  assertEqual { expected: A.init aslice
              , actual: map toArray (AV.init avslice) }

  -- uncons
  assertEqual { expected: A.uncons aslice
              , actual: map fixTail (AV.uncons avslice) }

  -- unsnoc
  assertEqual { expected: A.unsnoc aslice
              , actual: map fixInit (AV.unsnoc avslice) }

  -- modifyAtIndices
  assertEquals
    (AV.modifyAtIndices avslice (_ + 1) avslice)
    (A.modifyAtIndices aslice (_ + 1) aslice)

  -- reverse
  assertEquals
    (AV.reverse avslice)
    (A.reverse aslice)

  -- concat
  assertEqual { expected: fromArray $ A.concat $ map (const aslice) aslice
              , actual: AV.concat $ map (const avslice) avslice }

  -- concatMap
  assertEquals
    (AV.concatMap pure avslice)
    (A.concatMap pure aslice)

  -- mapWithIndex
  assertEquals
    (AV.mapWithIndex (\n el -> Tuple n el) avslice)
    (A.mapWithIndex (\n el -> Tuple n el) aslice)

  -- sort
  assertEquals
    (AV.sort avslice)
    (A.sort aslice)

  -- sortBy
  assertEquals
    (AV.sortBy (flip compare) avslice)
    (A.sortBy (flip compare) aslice)

  -- sortWith
  assertEquals
    (AV.sortWith negate avslice)
    (A.sortWith negate aslice)

  -- group
  assertEqual { actual: AV.group avslice
              , expected: fromArray $ map fromNonEmptyArray $ A.group aslice }

  -- group'
  assertEqual { actual: AV.group' avslice
              , expected: fromArray $ map fromNonEmptyArray $ A.group' aslice }

  -- groupBy
  assertEqual { actual: AV.groupBy (map not eq) avslice
              , expected: fromArray $ map fromNonEmptyArray $ A.groupBy (map not eq) aslice }

  -- nub
  assertEquals
    (AV.nub avslice)
    (A.nub aslice)

  -- nubEq
  assertEquals
    (AV.nubEq avslice)
    (A.nubEq aslice)

  -- nubBy
  assertEquals
    (AV.nubBy (flip compare) avslice)
    (A.nubBy (flip compare) aslice)

  -- nubByEq
  assertEquals
    (AV.nubByEq (\x y -> x > y) avslice)
    (A.nubByEq  (\x y -> x > y) aslice)

  -- union
  assertEquals
    (AV.union (AV.take 2 avslice) (AV.drop 4 avslice))
    (A.union (A.take 2 aslice) (A.drop 4 aslice))

  -- unionBy
  assertEquals
    (AV.unionBy (\x y -> x > y) (AV.take 2 avslice) (AV.drop 4 avslice))
    (A.unionBy  (\x y -> x > y) (A.take 2 aslice)   (A.drop 4 aslice))

  -- delete, deleteBy
  for_ avslice \el -> do
    assertEquals
      (AV.delete el avslice)
      (A.delete el aslice)

    assertEquals
      (AV.deleteBy (\x y -> x > y) el avslice)
      (A.deleteBy (\x y -> x > y) el aslice)

  -- foldM
  let fold_f = (\a b -> [a + b])
  assertEqual { actual: AV.foldM fold_f 1 avslice
              , expected: A.foldM fold_f 1 aslice }

  let fold_f' = (\a b -> Just $ a + b)
  -- foldRecM
  assertEqual { actual:   AV.foldRecM fold_f' 1 avslice
              , expected: A.foldRecM  fold_f' 1 aslice }


checkCombinations :: forall a. Ord a => Show a =>
                     ArrayView a -> ArrayView a ->
                     Array a -> Array a -> Effect Unit
checkCombinations av1 av2 a1 a2 = do
  -- difference
  assertEquals
    (AV.difference av1 av2)
    (A.difference a1 a2)

  -- intersect
  assertEquals
    (AV.intersect av1 av2)
    (A.intersect a1 a2)

  -- intersectBy
  assertEquals
    (AV.intersectBy (\x y -> x > y) av1 av2)
    (A.intersectBy (\x y -> x > y) a1 a2)

  -- zipWith
  assertEquals
    (AV.zipWith (\x y -> x > y) av1 av2)
    (A.zipWith (\x y -> x > y) a1 a2)

  -- zipWIthA
  assertEqual { actual: AV.zipWithA (\x y -> [x > y]) av1 av2
              , expected: map fromArray $ A.zipWithA (\x y -> [x > y]) a1 a2 }

  -- zip
  assertEquals
    (AV.zip av1 av2)
    (A.zip a1 a2)

  -- unzip
  assertEqual { actual: AV.unzip (AV.zip av1 av2)
              , expected: fromArray *** fromArray $ A.unzip (A.zip a1 a2) }


fixYesNo :: forall a.
            { no :: Array a,     yes :: Array a } ->
            { no :: ArrayView a, yes :: ArrayView a }
fixYesNo { yes, no } = { yes: fromArray yes, no: fromArray no }

fixTail :: forall a.
           { tail :: ArrayView a, head :: a } ->
           { tail :: Array a,     head :: a }
fixTail { head, tail } = { head, tail: toArray tail }

fixInit :: forall a.
           { init :: ArrayView a, last :: a } ->
           { init :: Array a,     last :: a }
fixInit { last, init } = { last, init: toArray init }

fixInitRest :: forall a.
               { init :: Array a ,    rest :: Array a} ->
               { init :: ArrayView a, rest :: ArrayView a }
fixInitRest { init, rest } = { init: fromArray init
                             , rest: fromArray rest }


assertEquals :: forall a. Eq a => Show a =>
                ArrayView a -> Array a -> Effect Unit
assertEquals av a = do
  assertEqual { expected: av
              , actual: fromArray a }
  assertEqual { expected: a
              , actual: toArray av }

assertEqualsMaybe :: forall a. Eq a => Show a =>
                     Maybe (ArrayView a) -> Maybe (Array a) -> Effect Unit
assertEqualsMaybe av a = do
  assertEqual { expected: av
              , actual: map fromArray a }
  assertEqual { expected: a
              , actual: map toArray av }


logDebug :: String -> Effect Unit
logDebug = if debug then log else const (pure unit)

inspect :: forall a. Show a => ArrayView a -> String
inspect = genericShow
