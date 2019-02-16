module Data.ArrayView.NonEmpty
  ( module Exports
  , fromArrayView
  , fromNonEmpty
  , toArrayView
  , toNonEmpty

--   , fromFoldable
--   , fromFoldable1
--   , toUnfoldable
--   , toUnfoldable1
--   , singleton
--   , (..), range
--   , replicate
--   , some

--   , length

--   , (:), cons
--   , cons'
--   , snoc
--   , snoc'
--   , appendArray
--   , insert
--   , insertBy

--   , head
--   , last
--   , tail
--   , init
--   , uncons
--   , unsnoc

--   , (!!), index
--   , elemIndex
--   , elemLastIndex
--   , findIndex
--   , findLastIndex
--   , insertAt
--   , deleteAt
--   , updateAt
--   , updateAtIndices
--   , modifyAt
--   , modifyAtIndices
--   , alterAt

--   , reverse
--   , concat
--   , concatMap
--   , filter
--   , partition
--   , filterA
--   , mapMaybe
--   , catMaybes

--   , sort
--   , sortBy
--   , sortWith
--   , slice
--   , take
--   , takeEnd
--   , takeWhile
--   , drop
--   , dropEnd
--   , dropWhile
--   , span

--   , nub
--   , nubBy
--   , nubEq
--   , nubByEq
--   , union
--   , union'
--   , unionBy
--   , unionBy'
--   , delete
--   , deleteBy

--   , (\\), difference
--   , difference'
--   , intersect
--   , intersect'
--   , intersectBy
--   , intersectBy'

--   , zipWith
--   , zipWithA
--   , zip
--   , unzip

--   , foldM
--   , foldRecM

--   , unsafeIndex
  ) where

import Prelude
import Data.NonEmpty
import Data.Newtype
import Data.Array as A
import Data.Maybe

import Data.ArrayView.Internal (NonEmptyArrayView) as Exports
import Data.ArrayView.Internal
import Data.ArrayView

-- | *O(1)*
fromArrayView :: forall a. ArrayView a -> Maybe (NonEmptyArrayView a)
fromArrayView av = case uncons av of
  Just { head, tail } -> Just (wrap (head :| tail))
  Nothing -> Nothing

fromNonEmpty :: forall a. NonEmpty ArrayView a -> NonEmptyArrayView a
fromNonEmpty = wrap

toArrayView :: forall a. NonEmptyArrayView a -> ArrayView a
toArrayView = fromNEAV

toNonEmpty :: forall a. NonEmptyArrayView a -> NonEmpty ArrayView a
toNonEmpty = unwrap
