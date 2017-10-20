[![Travis][travis badge]][travis]
[![Hex.pm Version][hex version badge]][hex]
[![Hex.pm License][hex license badge]][hex]
[![Erlang Versions][erlang version badge]][travis]
[![Build Tool][build tool]][hex]

# Unite

Pretty EUnit test formatters

<img src="https://raw.github.com/eproxus/unite/master/screenshot.png"
 width="490" alt="Example output" />

## Installation & Usage

Add Unite as a dependency in your top level `rebar.config` and enable the Unite formatter:

```erlang
{deps, [
    {unite, "", {git, "git://github.com/eproxus/unite.git"}}
]}.

{eunit_opts, [no_tty, {report, {unite_compact, []}}]}.
```

Using `no_tty` is important, because it disables the standard EUnit output.

Then just run Rebar 3 as usual: `rebar3 eunit`.

### Profiling

To get timings of long running tests, add `profile` to the option list:

```erlang
{eunit_opts, [no_tty, {report, {unite_compact, [profile]}}]}.
```

This will show the top 10 slowest tests (or the top N slowest tests if
`{profile, N}` is used).


<!-- Badges -->
[travis]: https://travis-ci.org/eproxus/unite
[travis badge]: https://img.shields.io/travis/eproxus/unite/master.svg?style=flat-square
[hex]: https://hex.pm/packages/meck
[hex version badge]: https://img.shields.io/hexpm/v/unite.svg?style=flat-square
[hex license badge]: https://img.shields.io/hexpm/l/unite.svg?style=flat-square
[erlang version badge]: https://img.shields.io/badge/erlang-R15B03%20to%2020.0-blue.svg?style=flat-square
[build tool]: https://img.shields.io/badge/build%20tool-rebar3-orange.svg?style=flat-square
