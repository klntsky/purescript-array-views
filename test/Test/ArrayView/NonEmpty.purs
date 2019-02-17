module Test.ArrayView.NonEmpty
       ( checkNonEmptySlices
       , checkNonEmptyWithIndex
       , checkNonEmptyWithPredicate
       , checkNonEmptyCombinations
       )
where

import Test.ArrayView.Common (equal)

import Data.Array as A
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NE
import Data.ArrayView.Internal
import Data.ArrayView.NonEmpty as NEAV
import Data.Maybe (Maybe(..))
import Data.Traversable (for_)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, compare, const, discard, eq, flip, map, negate, not, ($), (&&), (+), (<), (<<<), (>), (>=))
import Test.Assert (assertThrows)


checkNonEmptySlices :: Int -> Int ->
                       Tuple (NonEmptyArray Int) (NonEmptyArrayView Int) -> Effect Unit
checkNonEmptySlices i j (Tuple ne neav) = do
  equal (NEAV.singleton i)      (NE.singleton i)
  equal (NEAV.range i j)        (NE.range i j)
  equal (NEAV.replicate i neav) (NE.replicate i ne)
  equal (NEAV.length neav)      (NE.length ne)

  equal (NEAV.cons i neav)      (NE.cons i ne)
  equal (NEAV.cons' i (NEAV.toArrayView neav))
        (NE.cons'   i (NE.toArray ne))
  equal (NEAV.snoc neav i)      (NE.snoc ne i)
  equal (NEAV.snoc' (NEAV.toArrayView neav) i)
        (NE.snoc'   (NE.toArray ne) i)

  equal (NEAV.appendArrayView neav (NEAV.toArrayView neav))
        (NE.appendArray       ne   (NE.toArray ne))

  equal (NEAV.head neav)        (NE.head ne)
  equal (NEAV.last neav)        (NE.last ne)
  equal (NEAV.tail neav)        (NE.tail ne)
  equal (NEAV.init neav)        (NE.init ne)
  equal (NEAV.uncons neav).head (NE.uncons ne).head
  equal (NEAV.uncons neav).tail (NE.uncons ne).tail
  equal (NEAV.unsnoc neav).init (NE.unsnoc ne).init
  equal (NEAV.unsnoc neav).last (NE.unsnoc ne).last
  equal (NEAV.reverse neav)     (NE.reverse ne)
  equal (NEAV.concat (map (const neav) neav))
        (NE.concat (map (const ne) ne))
  equal (NEAV.concatMap (const neav) neav)
        (NE.concatMap (const ne) ne)
  equal (NEAV.sort neav)        (NE.sort ne)
  equal (NEAV.sortBy (flip compare) neav)
        (NE.sortBy (flip compare) ne)

  equal (NEAV.nub neav)         (NE.nub ne)
  equal (NEAV.nubBy (flip compare) neav)
        (NE.nubBy (flip compare) ne)

  equal (NEAV.nubEq neav)       (NE.nubEq ne)
  equal (NEAV.nubByEq eq neav)  (NE.nubByEq eq ne)

  let fold_f = (\a b -> [a + b])
  equal (NEAV.foldM fold_f 1 neav)
        (NE.foldM fold_f 1 ne)

  let fold_f' = (\a b -> Just $ a + b)
  equal (NEAV.foldRecM fold_f' 1 neav)
        (NE.foldRecM  fold_f' 1 ne)

  equal (NEAV.force neav)       (neav)


checkNonEmptyWithIndex :: Int -> Tuple (NonEmptyArray Int) (NonEmptyArrayView Int) -> Effect Unit
checkNonEmptyWithIndex ix (Tuple ne neav) = do
  equal (NEAV.index neav ix)         (NE.index ne ix)
  equal (NEAV.elemIndex ix neav)     (NE.elemIndex ix ne)
  equal (NEAV.elemLastIndex ix neav) (NE.elemLastIndex ix ne)
  equal (NEAV.insertAt ix ix neav)   (NE.insertAt ix ix ne)
  equal (NEAV.deleteAt ix neav)      (NE.deleteAt ix ne)
  equal (NEAV.updateAt ix ix neav)   (NE.updateAt ix ix ne)
  equal (NEAV.updateAtIndices [Tuple ix 0, Tuple ix 1] neav)
        (NE.updateAtIndices   [Tuple ix 0, Tuple ix 1] ne)
  equal (NEAV.modifyAt ix (_ + 1) neav)
        (NE.modifyAt ix (_ + 1) ne)
  equal (NEAV.modifyAtIndices [ix] (_ + 1) neav)
        (NE.modifyAtIndices   [ix] (_ + 1) ne)
  equal (NEAV.alterAt ix (\x -> Just x) neav)
        (NE.alterAt   ix (\x -> Just x) ne)
  equal (NEAV.takeEnd ix neav)       (NE.takeEnd ix ne)
  equal (NEAV.insert ix neav)        (NE.insert ix ne)

  equal (NEAV.insertBy (flip compare) ix neav)
        (NE.insertBy (flip compare) ix ne)

  for_ (-5 A... 5) \iy -> do
    equal (NEAV.slice iy ix neav)    (NE.slice iy ix ne)

  equal (NEAV.take ix neav)       (NE.take ix ne)
  equal (NEAV.drop ix neav)       (NE.drop ix ne)
  equal (NEAV.dropEnd ix neav)    (NE.dropEnd ix ne)

  equal (NEAV.delete ix neav)     (NE.delete ix ne)

  for_ [ (\x y -> x > y)
       , (\x y -> x < y)
       ] \pred -> do
    equal (NEAV.deleteBy pred ix neav) (NE.deleteBy pred ix ne)

  if ix >= 0 && ix < NEAV.length neav
    then
    equal (unsafePartial (NEAV.unsafeIndex neav ix))
          (unsafePartial (NE.unsafeIndex ne ix))
    else
    assertThrows \_ ->
    equal (unsafePartial (NEAV.unsafeIndex neav ix))
          (unsafePartial (NE.unsafeIndex ne ix))


checkNonEmptyWithPredicate :: (Int -> Boolean) ->
                              Tuple (NonEmptyArray Int) (NonEmptyArrayView Int) ->
                              Effect Unit
checkNonEmptyWithPredicate f (Tuple ne neav) = do
  equal (NEAV.filter f neav)        (NE.filter f ne)
  equal (NEAV.partition f neav).yes (NE.partition f ne).yes
  equal (NEAV.partition f neav).no  (NE.partition f ne).no

  let fA = Just <<< f
  equal (NEAV.filterA fA neav)      (NE.filterA fA  ne)

  let f' x = if f x then Just (f x) else Nothing
  equal (NEAV.mapMaybe f' neav)     (NE.mapMaybe f' ne)
  equal (NEAV.catMaybes $ map f' neav)
        (NE.catMaybes   $ map f' ne)

  equal (NEAV.dropWhile f neav)     (NE.dropWhile f ne)
  equal (NEAV.span f neav).init     (NE.span f ne).init
  equal (NEAV.span f neav).rest     (NE.span f ne).rest


checkNonEmptyCombinations :: Int ->
                             Tuple (NonEmptyArray Int) (NonEmptyArrayView Int) ->
                             Effect Unit
checkNonEmptyCombinations n (Tuple ne neav) = do
  let ne1 =   NE.cons'   1 (NE.drop n ne)
      neav1 = NEAV.cons' 1 (NEAV.drop n neav)
      ne2 =   NE.cons'   0 (NE.take n ne)
      neav2 = NEAV.cons' 0 (NEAV.take n neav)
      av = NEAV.take n neav1
      a = NE.take n ne1
      neq = not <<< eq
  equal (NEAV.union neav1 neav2) (NE.union ne1 ne2)
  equal (NEAV.union' neav1 av)   (NE.union' ne1 a)

  equal (NEAV.unionBy neq neav1 neav2) (NE.unionBy neq ne1 ne2)
  equal (NEAV.unionBy' neq neav1 av)   (NE.unionBy' neq ne1 a)
  equal (NEAV.difference neav1 neav2)  (NE.difference ne1 ne2)
  equal (NEAV.difference' neav1 av)    (NE.difference' ne1 a)

  equal (NEAV.intersect neav1 neav2)   (NE.intersect ne1 ne2)
  equal (NEAV.intersect' neav1 av)     (NE.intersect' ne1 a)

  equal (NEAV.intersectBy neq neav1 neav2) (NE.intersectBy neq ne1 ne2)
  equal (NEAV.intersectBy' neq neav1 av)   (NE.intersectBy' neq ne1 a)

  let zipF x y = x + y
  equal (NEAV.zipWith zipF neav1 neav2)    (NE.zipWith zipF ne1 ne2)
  let zipFA = (\x -> Just <<< zipF x)
  equal (NEAV.zipWithA zipFA neav1 neav2)  (NE.zipWithA zipFA ne1 ne2)
  equal (NEAV.zip neav1 neav2)             (NE.zip ne1 ne2)
  equal (NEAV.unzip $ NEAV.zip neav1 neav2)
        (NE.unzip $ NE.zip ne1 ne2)
