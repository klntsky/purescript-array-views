# purescript-array-views

![](https://img.shields.io/librariesio/github/8084/purescript-array-views.svg)
[![Build status](https://travis-ci.org/8084/purescript-array-views.svg?branch=master)](https://travis-ci.org/8084/purescript-array-views)

The time complexity of `uncons` called on `Array` is *O(n)* because [`.slice()` is used](https://github.com/purescript/purescript-arrays/blob/d218f6f6fa1a41ce3bd6daeef72f9b197c1eb8d2/src/Data/Array.js#L109)  to preserve purity. Thus iterating over `Array` using `uncons` is *O(n<sup>2</sup>)*.

But since purescript `Array`s are persistent at runtime, it is possible to defer multiple `.slice()` calls by introducing another data structure and operating on it instead.

`ArrayView` contains a pointer to some `Array` coupled with two numbers: index where the view starts relative to the beginning of the array and the length of the view.

```purescript
newtype ArrayView a = View { from :: Int, len :: Int, arr :: Array a }
```


So, instead of slicing, it is possible to just shift the indices.

Obviously, this technique does not improve the asymptotics of `cons`/`snoc`/`append` and other array constructing functions, so if the code uses these, there will be no benefit in replacing `Array` with `ArrayView`.

It should be noted once more that iterating over ordinary arrays using `cons` and `uncons`, which is common in Haskell (with lists) is a bad practice. Often such code can be refactored to use some type of fold and/or other standard functionals. However, there are cases where it is not straightforward, or where using `uncons`/`unsnoc` is more syntactically appealing, e.g. when iterating over many structures at once.

## Asymptotics comparison

For every function in `Data.Array` there is a corresponding function in `Data.ArrayView`, though most of them are plain reuses up to conversions between `Array` and `ArrayView`. Those with different time complexities are listed below:

| functions | Array | ArrayView | Note |
|----------|-------|-----------|-------|
| `slice`, `uncons`, `unsnoc`, `tail`, `init`, `take`, `drop`, `takeEnd`, `dropEnd` | *O(n)* | *O(1)* | *n* is the length of the resulting array
|`span` (used by `takeWhile`, `dropWhile`) | *O(n+m)* | *O(n)* | *n* is the length of the `init` array, *m* is the length of the `rest` |
| `ArrayView.toArray` |  | *O(n)* | *O(1)* if the given view corresponds to the whole array |
| `ArrayView.fromArray` | *O(1)* | |

## Impact on GC

Since every `ArrayView` holds a reference to some array, the latter can't be garbage-collected while the former is used. This leads to a memory consumption overhead.

If you need to free unused parts of array, use `Data.ArrayView.force :: forall a. ArrayView a -> ArrayView a` (which performs slicing and therefore is *O(n)*).

## Benchmarks

![](img/withUncons.png)

# Disclaimer

The code is in early stage of development and was not tested yet.