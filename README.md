<h1 align="center">unite</h1>

<p align="center">
  <a href="https://hex.pm/packages/unite">
    <img alt="hex.pm version" src="https://img.shields.io/hexpm/v/unite?style=flat-square"/>
  </a>
  <a href="LICENSE">
    <img alt="hex.pm license" src="https://img.shields.io/hexpm/l/unite?style=flat-square"/>
  </a>
  <img alt="erlang versions" src="https://img.shields.io/badge/erlang-21+-blue.svg?style=flat-square"/>
  <a href="https://github.com/sponsors/eproxus">
    <img alt="hex.pm license" src="https://img.shields.io/github/sponsors/eproxus?style=flat-square&color=%23ec6cb9"/>
  </a>
</p>

<p align="center">
  Pretty EUnit test formatters
</p>
<p align="center">
  <img src="assets/screenshots/screenshot.png" width="490" alt="Example output" />
</p>

## Installation & Usage

Add Unite as a dependency in your `rebar.config` and enable the Unite formatter:

```erlang
{profiles, [
    {test, [
        {deps, [unite]},
        {eunit_opts, [no_tty, {report, {unite_compact, []}}]}
    ]}
]}.

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

## Donations

If you or your company use Unite and find it useful, [donations](https://github.com/sponsors/eproxus) are greatly appreciated!
