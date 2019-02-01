# purescript-array-views

`uncons` on `Array` is `O(n)` because [`.slice()` is used](https://github.com/purescript/purescript-arrays/blob/d218f6f6fa1a41ce3bd6daeef72f9b197c1eb8d2/src/Data/Array.js#L109)  to preserve purity. Thus iterating over `Array` using `uncons` is `O(n^2)`.

But since purescript `Array`s are persistent at runtime, it is possible to just "manipulate the pointers" instead on actually calling `.slice()`.

`ArrayView` contains a pointer to some `Array` coupled with two numbers: index where the view "starts" relative to the beginning of the array and the length of the view.

```purescript
data ArrayView a = View Int Int (Array a)
```

Every `.slice()`-dependent function in `Data.Array` has the corresponding function in `Data.ArrayView` that just shifts the indices which is `O(1)`.

![](img/withUncons.png)

# Disclaimer

The code is in early stage of development and was not tested yet.