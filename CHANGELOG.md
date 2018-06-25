# Changelog

## [not released] 0.3.0-alpha5

* Added new numeric type `EpsilonOf`

* Fixed `Distinct`, `DistinctBy` predicates, `DistinctListOf`,
  `NonEmptyDistinctListOf` types: all of them didn't actually check
  distinctiveness in previous release due to incorrect apply of `distinct?`
  function to the list of arguments

* Fixed `Exists` predicate: it failed with runtime exception on every check in
  previous release due to the typo

* Removed Potemkin dependency

* Introduced `refined'` macro to deal with namings of types (captures the type name
  as it was specified in the code *before* resolving the var itself)

* Added `deps.edn`

## 0.3.0-alpha4

* Well... I've started here :)
