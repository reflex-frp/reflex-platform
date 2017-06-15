Installation
============

.. todo:: copy from reflex-platform, it has to provide all the possible ways
  user might need to install including stack, nix, nixos, ...

The essential components required for developing reflex based application are

1. GHC or GHCJS

2. Reflex library

OS and System requirements
~~~~~~~~~~~~~~~~~~~~~~~~~~


Using nix
---------

Nix is the preferred method for setting up the development environment for reflex.


Using Reflex-platform
~~~~~~~~~~~~~~~

Please refer to `reflex-platform <https://github.com/reflex-frp/reflex-platform/blob/develop/README.md#setup>`_

The `try-reflex` script can create a development environment with ghcjs. You can use this to have a quick starting setup to compile code-snippets and smaller projects. (Quick means requiring less user input, the first time can take considerable time to complete)

For a non-trivial project it is recommended to use cabal.

Using cabal with nix and reflex-platform
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you dont have a project with cabal file then use `cabal init` to create one.

1. Use the `workon` script from reflex-platform to create a development environment (nix shell) according to the dependencies specified in cabal file.
::

  $ ~/reflex-platform/work-on ghcjs ./your-project

  # or just "cabal configure" if working on ghc
  <nix-shell> $ cabal configure --ghcjs
  <nix-shell> $ cabal build

This will use your package's cabal file to determine dependencies. If you have a default.nix, it will use that instead. Note that your project's path must include at least one slash ('/') so that work-on can detect that it is a path, rather than a package name.

This will give you the exact environment needed to work with the given package and platform, rather than the general-purpose environment provided by the Reflex Platform.

You can replace ghcjs with ghc to hack on the native GHC version of the package (including with GHCi if you want). You can also use a package name instead of a path, which will drop you into the standard build environment of that package; this works even if you don't yet have the source for that package.

Add reflex-platform to project
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Since the build environment is dependent on the reflex-platform, it is important to keep this dependency as a part of the project. Moreover the version of libraries will change with time in the reflex-platform so it is important to keep a reference to the reflex-platform' "version" which has been used to build the project.

The simplest way to do this is to create a submodule in your project

Assuming you are using git for versioning::

  git submodule add https://github.com/reflex-frp/reflex-platform

  # Then use the workon script to get the nix-shell
  ./reflex-platform/workon ghcjs ./.

Nix can also be used directly (instead of the `workon` script) as shown below


Creating multiple sub-projects (Server + Client)
------------------------------------------------

If you writing both the server and client in haskell, it is a good practice to put the code used for communication (like message types) and common data types in a shared package. Then use the shared package in both client and server by adding it to their dependent packages.

The structure of a typical project looks like this::

  .
  ├── reflex-platform
  ├── client
  │   ├── client.cabal
  │   ├── Setup.hs
  │   └── src
  │       └── Main.hs
  ├── common
  │   ├── common.cabal
  │   ├── Setup.hs
  │   └── src
  │       └── DataTypes.hs
  └── server
      ├── server.cabal
      ├── Setup.hs
      └── src
          └── Main.hs


  # client.cabal
  name:                client
  version:             0.1.0.0
  build-type:          Simple
  cabal-version:       >=1.10

  executable client
    main-is:             Main.hs
    build-depends:       base >=4.9 && <4.10
                       , common
                       , reflex-dom
                       , reflex
    hs-source-dirs:      src
    default-language:    Haskell2010

  # common.cabal
  name:                common
  version:             0.1.0.0
  build-type:          Simple
  cabal-version:       >=1.10

  library
    exposed-modules:     DataTypes
    build-depends:       base >=4.9 && <4.10
                       , aeson
    hs-source-dirs:      src
    default-language:    Haskell2010

  # server.cabal
  name:                server
  version:             0.1.0.0
  build-type:          Simple
  cabal-version:       >=1.10

  executable server
    main-is:             Main.hs
    build-depends:       base >=4.9 && <4.10
                       , common
                       , wai
                       , warp
    hs-source-dirs:      src
    default-language:    Haskell2010

To specify the local dependency of `common` we can use the nix

There are various ways of doing this, depending upon the level of your understading of nix.

For a simple setup see
https://github.com/srhb/reflex-servant-scaffold


Local Haddock documentation
------------------------------------

In a nix shell created using `try-reflex` or `workon` you can use this command to get the path to haddock documentation.::

  # Or use ghcjs-pkg
  ghc-pkg field <package> haddock-html
