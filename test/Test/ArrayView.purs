module Test.ArrayView
       ( checkSlices
       , checkWithIndex
       , checkWithPredicate
       , checkCombinations
       )
where

import Test.ArrayView.Common

import Data.Array as A
import Data.ArrayView as AV
import Data.ArrayView.Internal (ArrayView, fromArray, toArray)
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Prelude (class Eq, class Ord, class Show, Unit, compare, const, discard, eq, flip, map, negate, not, pure, ($), (&&), (+), (<), (<>), (==), (>), (>=), (>>>))
import Test.Assert (assertThrows)


checkSlices :: Int -> Int -> ArrayView Int -> Array Int -> Effect Unit
checkSlices i j avslice aslice = do

  -- Ord
  equal (aslice `compare` aslice)
        (avslice `compare` avslice)
  equal (A.reverse aslice `compare` aslice)
        (AV.reverse avslice `compare` avslice)

  -- fromArray <<< toArray == identity
  equal (avslice)
          (fromArray (toArray avslice))

  equal (aslice) (toArray avslice)

  equal (avslice) (fromArray aslice)

  -- force
  equal (AV.force avslice) aslice

  -- null
  equal (A.null aslice) (AV.null avslice)

  -- length
  equal (A.length aslice) (AV.length avslice)

  -- cons
  equal (A.cons 0 aslice) (AV.cons 0 avslice)

  -- snoc
  equal (A.snoc aslice 0) (AV.snoc avslice 0)

  -- insert
  for_ (-1 A... 1) \e -> do
    equal (AV.insert e avslice) (A.insert e aslice)

  -- insertBy
  for_ (-1 A... 1) \e -> do
    equal
      (AV.insertBy (flip compare) e avslice)
      (A.insertBy  (flip compare) e aslice)

  -- head
  equal (A.head aslice) (AV.head avslice)

  -- last
  equal (A.last aslice) (AV.last avslice)

  -- tail
  equal (A.tail aslice) (AV.tail avslice)

  -- init
  equal (A.init aslice) (AV.init avslice)

  -- uncons
  equal (A.uncons aslice) (map fixTail (AV.uncons avslice))

  -- unsnoc
  equal (A.unsnoc aslice) (map fixInit (AV.unsnoc avslice))

  -- modifyAtIndices
  equal (AV.modifyAtIndices avslice (_ + 1) avslice)
        (A.modifyAtIndices  aslice  (_ + 1) aslice)

  -- reverse
  equal (AV.reverse avslice) (A.reverse aslice)

  -- concat
  equal (A.concat $ map (const aslice) aslice)
        (AV.concat $ map (const avslice) avslice)

  -- concatMap
  equal (AV.concatMap pure avslice) (A.concatMap pure aslice)

  -- mapWithIndex
  equal (AV.mapWithIndex (\n el -> Tuple n el) avslice)
        (A.mapWithIndex (\n el -> Tuple n el) aslice)

  -- sort
  equal (AV.sort avslice) (A.sort aslice)

  -- sortBy
  equal (AV.sortBy (flip compare) avslice) (A.sortBy  (flip compare) aslice)

  -- sortWith
  equal (AV.sortWith negate avslice) (A.sortWith negate aslice)

  -- group
  equal (AV.group avslice) (A.group aslice)

  -- group'
  equal (AV.group' avslice) (A.group' aslice)

  -- groupBy
  equal (AV.groupBy (map not eq) avslice)
        (A.groupBy (map not eq) aslice)

  -- nub
  equal (AV.nub avslice) (A.nub aslice)

  -- nubEq
  equal (AV.nubEq avslice) (A.nubEq aslice)

  -- nubBy
  equal (AV.nubBy (flip compare) avslice) (A.nubBy (flip compare) aslice)

  -- nubByEq
  equal (AV.nubByEq (\x y -> x > y) avslice) (A.nubByEq  (\x y -> x > y) aslice)

  -- union
  equal
    (AV.union (AV.take 2 avslice) (AV.drop 4 avslice))
    (A.union (A.take 2 aslice) (A.drop 4 aslice))

  -- unionBy
  equal
    (AV.unionBy (\x y -> x > y) (AV.take 2 avslice) (AV.drop 4 avslice))
    (A.unionBy  (\x y -> x > y) (A.take 2 aslice)   (A.drop 4 aslice))

  -- delete, deleteBy
  for_ avslice \el -> do
    equal
      (AV.delete el avslice)
      (A.delete el aslice)

    equal
      (AV.deleteBy (\x y -> x > y) el avslice)
      (A.deleteBy (\x y -> x > y) el aslice)

  -- foldM
  let fold_f = (\a b -> [a + b])
  equal (AV.foldM fold_f 1 avslice) (A.foldM fold_f 1 aslice)

  let fold_f' = (\a b -> Just $ a + b)
  -- foldRecM
  equal (AV.foldRecM fold_f' 1 avslice) (A.foldRecM  fold_f' 1 aslice)


checkWithIndex :: Int -> ArrayView Int -> Array Int -> Effect Unit
checkWithIndex ix avslice aslice = do
  for_ (-10 A... 10) \i -> do
    -- slice
    equal (AV.slice i ix avslice) (A.slice  i ix aslice)

  -- take
  equal (AV.take ix avslice) (A.take ix aslice)

  -- takeEnd
  equal (AV.takeEnd ix avslice) (A.takeEnd ix aslice)

  -- drop
  equal (AV.drop ix avslice) (A.drop ix aslice)

  -- dropEnd
  equal (AV.dropEnd ix avslice) (A.dropEnd ix aslice)

  -- index
  equal (A.index aslice ix) (AV.index avslice ix)

  -- unsafeIndex
  if ix >= 0 && ix < A.length aslice
    then
    equal (unsafePartial (A.unsafeIndex aslice ix))
          (unsafePartial (AV.unsafeIndex avslice ix))
    else
    assertThrows \_ ->
    equal (unsafePartial (A.unsafeIndex aslice ix))
          (unsafePartial (AV.unsafeIndex avslice ix))

  -- elemIndex
  equal (A.elemIndex ix (aslice <> aslice))
        (AV.elemIndex ix  (avslice <> avslice))

  -- elemLastIndex
  equal (A.elemLastIndex ix (aslice <> aslice))
        (AV.elemLastIndex ix  (avslice <> avslice))

  -- findIndex
  equal (A.findIndex (_ == ix) (aslice <> aslice))
        (AV.findIndex (_ == ix)  (avslice <> avslice))

  -- findLastIndex
  equal (A.findIndex (_ == ix) (aslice <> aslice))
        (AV.findIndex (_ == ix)  (avslice <> avslice))

  -- insertAt
  equal (AV.insertAt ix ix avslice) (A.insertAt ix ix aslice)

  -- deleteAt
  equal (AV.deleteAt ix avslice) (A.deleteAt ix aslice)

  -- updateAt
  equal (AV.updateAt ix 0 avslice) (A.updateAt ix 0 aslice)

  -- updateAtIndices
  let ixs = [Tuple ix 0, Tuple (ix+1) 1]
  equal (AV.updateAtIndices ixs avslice) (A.updateAtIndices ixs aslice)

  -- modifyAt
  equal (AV.modifyAt ix (_ + 1) avslice) (A.modifyAt ix (_ + 1) aslice)

  -- alterAt
  equal (AV.alterAt ix pure avslice) (A.alterAt ix pure aslice)


checkWithPredicate :: forall a.
                      Eq a => Show a =>
                      (a -> Boolean) -> ArrayView a -> Array a -> Effect Unit
checkWithPredicate pred avslice aslice = do
  -- span
  equal (fixInitRest (A.span pred aslice)) (AV.span pred avslice)

  -- takeWhile
  equal (AV.takeWhile pred avslice) (A.takeWhile pred aslice)

  -- dropWhile
  equal (AV.dropWhile pred avslice) (A.dropWhile pred aslice)

  -- filter
  equal (AV.filter pred avslice) (A.filter pred aslice)

  -- filterA
  equal (A.filterA (pred >>> A.singleton) aslice)
        (AV.filterA (pred >>> A.singleton) avslice)

  -- partition
  equal (fixYesNo (A.partition pred aslice)) (AV.partition pred avslice)

  -- mapMaybe
  let g n = if pred n then Just n else Nothing
  equal (AV.mapMaybe g avslice) (A.mapMaybe g aslice)

  -- catMaybes
  equal (AV.catMaybes (map g avslice)) (A.catMaybes (map g aslice))


checkCombinations :: forall a. Ord a => Show a =>
                     ArrayView a -> ArrayView a ->
                     Array a -> Array a -> Effect Unit
checkCombinations av1 av2 a1 a2 = do
  -- difference
  equal (AV.difference av1 av2) (A.difference a1 a2)

  -- intersect
  equal (AV.intersect av1 av2) (A.intersect a1 a2)

  -- intersectBy
  let f = (\x y -> x > y)
  equal (AV.intersectBy f av1 av2) (A.intersectBy f a1 a2)

  -- zipWith
  equal (AV.zipWith (\x y -> x > y) av1 av2) (A.zipWith (\x y -> x > y) a1 a2)

  -- zipWIthA
  equal (AV.zipWithA (\x y -> [x > y]) av1 av2)
        (A.zipWithA  (\x y -> [x > y]) a1 a2)

  -- zip
  equal (AV.zip av1 av2) (A.zip a1 a2)

  -- unzip
  equal (AV.unzip (AV.zip av1 av2))
        (A.unzip (A.zip a1 a2))
