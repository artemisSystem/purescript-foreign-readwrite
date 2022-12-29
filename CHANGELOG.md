# Changelog

All notable changes to this project will (hopefully) be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased

- Added `MonoidProxy` and `DefaultMonoid` for returning the mempty value when
trying to parse `undefined` (including missing object fields)
- Fixed infinite recursion in the `ReadForeign Default` instance

## [v3.3.0] - 2022-12-08

- Added `NonEmptyArray` instances for `ReadForeign` and `WriteForeign`

## [v3.2.1] - 2022-10-11

### Additions

- Added `default` which resolves to the default value of a given `Default` type.

## [v3.2.0] - 2022-10-10

### Additions

- Added the `Default` type which can specify a default value that read/writes
from/to `undefined`.

## [v3.1.0] - 2022-09-26

### Additions

- Added `Unit` instances for `ReadForeign` and `WriteForeign`
- Exported `undefined` from `Foreign.ReadWrite`

## [v3.0.0] - 2022-05-04

### Breaking Changes

- Update to PureScript 0.15
- The typeclass `ReadWriteForeign` has been split into two typeclasses called
`ReadForeign` and `WriteForeign`, with methods `readForeign` and `writeForeign`
respectively.

### Additions

- Added `IncompleteRecord`, which has a `ReadForeign` instance.

## [v2.0.0] - 2022-04-21

### Breaking Changes

- The `Record` instance for `ReadWriteForeign` now preserves the ordering of its
fields, and no longer writes a field to the `Foreign` result if its value is
`undefined`.

## [v1.0.2] - 2022-04-17

- Remove unused dependencies

## [v1.0.1] - 2022-04-17

- Fix package name in spago.dhall

## [v1.0.0] - 2022-04-15

- Initial Release
