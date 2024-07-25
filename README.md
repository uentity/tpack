# Modern C++ type pack/list library with efficient value-based approach
`tpack` is a header-only C++ library that implements types pack (or types list, terminology difference is minor) and various compile-time algorithms on it.
The main difference from most of available alternatives is that `tpack` is aiming to reduce compile time and compilation memory footprint by *minimizing the number of auxillary templates instantiations* required to manipulate with a types pack.

Typical C++ implementation of type list relies on classic meta-programming approach based on recursive instantiation of helper template class/struct that implements the algorithm step with explicitly provided partial specialization(s) to stop recursion.
In contrast, `tpack` heavily uses C++17 *constexpr functions* and *folding expressions* for the same purposes and will **never** trigger recursive helper templates instantiations.

As consequence, algorithms on types are represented as conventional *constexpr functions* instead of *meta-functions* which are actually classes with `type` and/or `value` members denoting meta-function return type and/or value.
Hence the *value-based approach* meaning that both arguments to algorithms and resulting type pack are *values* instead of types in case of meta-functions.

Some algorithms like `find_if` require a predicate which is a function operating on types, and provide two overloads: first that accepts predicate as constexpr function (corresponding to value-based approach) and second where predicate is specified as meta-function (described as `template<typename...> typename Predicate`). The second form of API is provided for convenience in terms of direct compatibility with STL `type_traits` and other meta-functions that user may already have.

## References
Library is inspired by author's experience and ideas from this talk (in Russian):
https://www.youtube.com/watch?v=ZUmc45Njs9U
