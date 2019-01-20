-   [`android`](#android)
-   [`ios`](#ios)
-   [`name`](#name)
-   [`overrides`](#overrides)
-   [`packages`](#packages)
-   [`passthru`](#passthru)
-   [`shellToolOverrides`](#shelltooloverrides)
-   [`shells`](#shells)
-   [`tools`](#tools)
-   [`useWarp`](#usewarp)
-   [`withHoogle`](#withhoogle)

------------------------------------------------------------------------

`android`
---------

#### Description

Use this argument to configure android apps. The returned derivations
will be in `android.<app name>`.

#### Example

``` nix
{
  frontend = {
    applicationId = "org.example.frontend";
    displayName = "Example Android App";
    executableName = "frontend";
  };
}
```

#### Default

``` nix
{}
```

------------------------------------------------------------------------

`ios`
-----

#### Description

Use this argument to configure ios apps. The returned derivations will
be in `ios.<app name>`.

#### Example

``` nix
{
  frontend = {
    bundleIdentifier = "org.example.frontend";
    bundleName = "Example iOS App";
    executableName = "frontend";
  };
}
```

#### Default

``` nix
{}
```

------------------------------------------------------------------------

`name`
------

#### Description

An optional name for your project. This is only used for the name of the
untargeted `nix-build`.

#### Default

``` nix
"reflex-project"
```

------------------------------------------------------------------------

`overrides`
-----------

#### Description

A function for overriding Haskell packages. You can use `callHackage`
and `callCabal2nix` to bump package versions or build them from GitHub.
e.g.

#### Example

``` nix
self: super: {
  lens = self.callHackage "lens" "4.15.4" {};
  free = self.callCabal2nix "free" (pkgs.fetchFromGitHub {
    owner = "ekmett";
    repo = "free";
    rev = "a0c5bef18b9609377f20ac6a153a20b7b94578c9";
    sha256 = "0vh3hj5rj98d448l647jc6b6q1km4nd4k01s9rajgkc2igigfp6s";
  }) {};
}

```

#### Default

``` nix
"<function>"
```

------------------------------------------------------------------------

`packages`
----------

#### Description

An attribute set for defining packages easily. Keys are the cabal
package name and values are the path to the source directory, or
derivations returning sources.

#### Example

``` nix
{
  frontend = ./frontend;
  backend = ./backend;
  common = ./common;

  free = pkgs.fetchFromGitHub {
    owner = "ekmett";
    repo = "free";
    rev = "<...>";
    sha256 = "<...>";
  };
}

```

#### Default

``` nix
{}
```

------------------------------------------------------------------------

`passthru`
----------

#### Description

Specify arbitrary nix expressions.

#### Example

``` nix

```

#### Default

``` nix
{}
```

------------------------------------------------------------------------

`shellToolOverrides`
--------------------

#### Description

A function returning a record of tools to provide in the nix-shells.
Some tools, like `ghc-mod`, have to be built with the same GHC as your
project. The argument to the `tools` function is the haskell package set
of the platform we are developing for, allowing you to build tools with
the correct Haskell package set.

Some tools, like `ghc-mod`, have to be built with the same GHC as your
project. The argument to the `tools` function is the haskell package set
of the platform we are developing for, allowing you to build tools with
the correct Haskell package set.

The second argument, `super`, is the record of tools provided by
default. You can override these defaults by returning values with the
same name in your record. They can be disabled by setting them to null.

#### Example

``` nix
ghc: super: {
  inherit (ghc) hpack;
  inherit (pkgs) chromium;
  ghc-mod = null;
  cabal-install = ghc.callHackage "cabal-install" "2.0.0.1" {};
  ghcid = pkgs.haskell.lib.justStaticExecutables super.ghcid;
};

```

#### Default

``` nix
"<function>"
```

------------------------------------------------------------------------

`shells`
--------

#### Description

The `shells` field defines which platforms we'd like to develop for, and
which packages' dependencies we want available in the development
sandbox for that platform. Note in the example that specifying `common`
is important; otherwise it will be treated as a dependency that needs to
be built by Nix for the sandbox. You can use these shells with
`cabal.project` files to build all three packages in a shared
incremental environment, for both GHC and GHCJS.

#### Example

``` nix
{
  ghc = [
    ("frontend")
    ("backend")
    ("common")
  ];
  ghcjs = [
    ("frontend")
    ("common")
  ];
}
```

#### Default

``` nix
{}
```

------------------------------------------------------------------------

`tools`
-------

#### Description

A function returning the list of tools to provide in the nix-shells.
Some tools, like `ghc-mod`, have to be built with the same GHC as your
project. The argument to the `tools` function is the haskell package set
of the platform we are developing for, allowing you to build tools with
the correct Haskell package set.

#### Example

``` nix
ghc: with ghc; [
  hpack
  pkgs.chromium
]

```

#### Default

``` nix
"<function>"
```

------------------------------------------------------------------------

`useWarp`
---------

#### Description

Configure `reflex-dom` to use `jsaddle-warp`.

#### Default

``` nix
false
```

------------------------------------------------------------------------

`withHoogle`
------------

#### Description

Set to false to disable building the hoogle database when entering the
nix-shell.

#### Default

``` nix
true
```
