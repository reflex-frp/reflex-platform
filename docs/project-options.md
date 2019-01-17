-   [`android`](#android)
-   [`android.<name>.activityAttributes`](#androidnameactivityattributes)
-   [`android.<name>.applicationId`](#androidnameapplicationid)
-   [`android.<name>.assets`](#androidnameassets)
-   [`android.<name>.displayName`](#androidnamedisplayname)
-   [`android.<name>.executableName`](#androidnameexecutablename)
-   [`android.<name>.googleServicesJson`](#androidnamegoogleservicesjson)
-   [`android.<name>.iconPath`](#androidnameiconpath)
-   [`android.<name>.intentFilters`](#androidnameintentfilters)
-   [`android.<name>.package`](#androidnamepackage)
-   [`android.<name>.permissions`](#androidnamepermissions)
-   [`android.<name>.releaseKey`](#androidnamereleasekey)
-   [`android.<name>.resources`](#androidnameresources)
-   [`android.<name>.services`](#androidnameservices)
-   [`android.<name>.universalApk`](#androidnameuniversalapk)
-   [`android.<name>.version`](#androidnameversion)
-   [`android.<name>.version.code`](#androidnameversioncode)
-   [`android.<name>.version.name`](#androidnameversionname)
-   [`ios`](#ios)
-   [`ios.<name>.bundleIdentifier`](#iosnamebundleidentifier)
-   [`ios.<name>.bundleName`](#iosnamebundlename)
-   [`ios.<name>.executableName`](#iosnameexecutablename)
-   [`ios.<name>.package`](#iosnamepackage)
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

`android.<name>.activityAttributes`
-----------------------------------

#### Description

Additional activity attributes

#### Example

``` nix
"android:launchMode=\"singleInstance\""
```

------------------------------------------------------------------------

`android.<name>.applicationId`
------------------------------

#### Description

The [Application
ID](https://developer.android.com/studio/build/application-id.html) for
your Android package

#### Example

``` nix
"org.example.frontend"
```

------------------------------------------------------------------------

`android.<name>.assets`
-----------------------

#### Description

#### Default

``` nix
"./assets"
```

------------------------------------------------------------------------

`android.<name>.displayName`
----------------------------

#### Description

The app name displayed to the user.

#### Example

``` nix
"Example Android App"
```

------------------------------------------------------------------------

`android.<name>.executableName`
-------------------------------

#### Description

The name of the executable in the Cabal file that will become the main
activity in the Android package

#### Example

``` nix
"frontend"
```

------------------------------------------------------------------------

`android.<name>.googleServicesJson`
-----------------------------------

#### Description

------------------------------------------------------------------------

`android.<name>.iconPath`
-------------------------

#### Description

#### Default

``` nix
"@drawable/ic_launcher"
```

------------------------------------------------------------------------

`android.<name>.intentFilters`
------------------------------

#### Description

Manifest XML for additional intent filters

#### Example

``` nix
concatStrings (map intentFilterXml deepLinkUris)

```

------------------------------------------------------------------------

`android.<name>.package`
------------------------

#### Description

The Haskell package to get your frontend executable from. Defaults to
`<name>`. The `package` argument can be set to use a different Haskell
package than the one named <app name>.

#### Example

``` nix
p: p.frontend
```

#### Default

``` nix
null
```

------------------------------------------------------------------------

`android.<name>.permissions`
----------------------------

#### Description

Manifest XML for additional permissions

------------------------------------------------------------------------

`android.<name>.releaseKey`
---------------------------

#### Description

To create a release build, set this to a value like:

#### Example

``` nix
{ storeFile = ./path/to/keystore;
  storePassword = "password";
  keyAlias = "myKey";
  keyPassword = "password";
}

```

#### Default

``` nix
null
```

------------------------------------------------------------------------

`android.<name>.resources`
--------------------------

#### Description

#### Default

``` nix
"./res"
```

------------------------------------------------------------------------

`android.<name>.services`
-------------------------

#### Description

------------------------------------------------------------------------

`android.<name>.universalApk`
-----------------------------

#### Description

Set this to false to build one APK per target platform. This will
automatically transform the version code to 1000 \* versionCode + offset
where "offset" is a per-platform constant.

#### Default

``` nix
true
```

------------------------------------------------------------------------

`android.<name>.version`
------------------------

#### Description

Version information.

#### Example

``` nix
{
  version = {
    code = 2;
    name = "1.1";
  };
}
```

------------------------------------------------------------------------

`android.<name>.version.code`
-----------------------------

#### Description

Must be a monotonically increasing number; defines what it means to
"upgrade" the app.

#### Example

``` nix
2
```

------------------------------------------------------------------------

`android.<name>.version.name`
-----------------------------

#### Description

The version that is displayed to the end user

#### Example

``` nix
"1.1"
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

`ios.<name>.bundleIdentifier`
-----------------------------

#### Description

The bundle identifier to make the app with.

#### Example

``` nix
"org.example.frontend"
```

------------------------------------------------------------------------

`ios.<name>.bundleName`
-----------------------

#### Description

The app name displayed to the user.

#### Example

``` nix
"Example iOS App"
```

------------------------------------------------------------------------

`ios.<name>.executableName`
---------------------------

#### Description

The name of the executable component in your packages cabal file.

#### Example

``` nix
"frontend"
```

------------------------------------------------------------------------

`ios.<name>.package`
--------------------

#### Description

The Haskell package to get your frontend executable from. Defaults to
`<name>`. The `package` argument can be set to use a different Haskell
package than the one named <app name>.

#### Example

``` nix
p: p.frontend
```

#### Default

``` nix
null
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
