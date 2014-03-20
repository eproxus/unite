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
