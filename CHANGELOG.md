# Change Log

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog], and this project adheres to
[Semantic Versioning].

# Change Log

## [Unreleased]

## [0.3.2] - 2022-02-22

### Changed

- Drop support for Erlang R15, R16 and 17.0 (because of deprecated time APIs)

### Fixed

- Fixed header formatting inconsistencies
- Stack traces containing io:format control sequences now print correctly

## [0.3.1] - 2018-07-12

### Added

- Print something when no tests are run [\#13](https://github.com/eproxus/unite/issues/13)
- Enable backwards compatibility to R15 [\#12](https://github.com/eproxus/unite/issues/12)

### Fixed

- Crash on OTP 21 [\#19](https://github.com/eproxus/unite/issues/19)
- Crash in `format\_macro\_string` when attempting to pretty-print guard from `?assertMatch` [\#14](https://github.com/eproxus/unite/issues/14)
- Do not attempt to pretty-print expressions that cannot be parsed [\#15](https://github.com/eproxus/unite/pull/15) ([jonathanperret](https://github.com/jonathanperret))

## [0.3.0] - 2017-10-20

### Added

- Print text when no tests are run ([60cd086](https://github.com/eproxus/unite/commit/60cd086f2aac123988f5c1f5aa0a28df57e9c5fa))
- Support old time API for pre-18 versions ([998df2a](https://github.com/eproxus/unite/commit/998df2a2eb169fda13fae7f4739d46e2809ff7a2))

## [0.2.0] - 2017-09-12

### Added

- Show relative paths in stack traces [\#11](https://github.com/eproxus/unite/issues/11)
- Support OTP 18 [\#6](https://github.com/eproxus/unite/pull/6) ([knutin](https://github.com/knutin))

### Fixed

- Fix crash on unhandled case clause [\#10](https://github.com/eproxus/unite/pull/10) ([binarytemple](https://github.com/binarytemple))
- Fix spontaneous errors on strange testcase reports [\#5](https://github.com/eproxus/unite/pull/5) ([keynslug](https://github.com/keynslug))

## [v0.0.1] - 2015-08-25

### Fixed

- io:columns\(\) can return {error, enotsup} when run from an editor [\#1](https://github.com/eproxus/unite/issues/1)
- Fix for older Erlang version [\#2](https://github.com/eproxus/unite/pull/2) ([JonGretar](https://github.com/JonGretar))


[Unreleased]: https://github.com/eproxus/unite/compare/v0.3.2...HEAD
[0.3.1]: https://github.com/eproxus/unite/compare/v0.3.1...v0.3.2
[0.3.1]: https://github.com/eproxus/unite/compare/v0.3.0...v0.3.1
[0.3.0]: https://github.com/eproxus/unite/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/eproxus/unite/compare/v0.0.1...v0.2.0
[0.0.1]: https://github.com/eproxus/unite/compare/048e7e0e84c34d06426a4d952973df7df307391a...v0.0.1

[Keep a Changelog]: https://keepachangelog.com/en/1.0.0/
[Semantic Versioning]: https://semver.org/spec/v2.0.0.html
