Unite
=====

Pretty EUnit test formatters

![Example Output](https://raw.github.com/eproxus/unite/master/screenshot.png)

Installation & Usage
--------------------

Add Unite as a dependency in your top level `rebar.config` and enable the Unite formatter:

```erlang
{deps, [
    {unite, "", {git, "git://github.com/eproxus/unite.git"}}
]}.

{eunit_opts, [no_tty, {report, {unite_compact, []}}]}.
```

Using `no_tty` is important, because it disables the standard EUnit output.

Then just run Rebar as usual: `rebar eunit`.

### Profiling

To get timings of long running tests, add `profile` to the option list:

```erlang
{eunit_opts, [no_tty, {report, {unite_compact, [profile]}}]}.
```

This will show the top 10 slowest tests (or the top N slowest tests if `{profile, N}` is used).

