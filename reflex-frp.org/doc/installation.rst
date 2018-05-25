Installation
============

.. todo:: Add ways to use reflex without nix / reflex-plarform

Overview
--------

The essential components required for developing ``reflex`` based application are

#. GHC or GHCJS

   If you are building a web application with ``reflex-dom`` then you need ``ghcjs`` to create JavaScript output.
   With ``ghc`` you can use the ``reflex-dom`` to create a ``webkit`` based desktop or mobile app.

#. Reflex library

   The current supported ``reflex`` and ``reflex-dom`` packages (version 0.5 and 0.4 respectively) are available only through Github, as they are not yet published on Hackage.

   To obtain the current ``reflex`` packages, along with all its dependecies, ``reflex-platform`` is the recommended method.

``reflex-platform`` - https://github.com/reflex-frp/reflex-platform

  Is a collection of ``nix`` scripts to provide ``ghc``, ``ghcjs`` and a curated set of packages for use with ``reflex-dom``.

  This includes a specially modified ``text`` package which internally uses the JS string.
  The performance of this ``text`` package is significantly better on the browser.

You might also need

* Haddock Documentation with local hoogle

* GHCi / ghcid with jsaddle-warp for faster dev cycles

* IDE/Editor support

.. note::
  GHCJS uses a lot of memory during compilation. 16GB of memory is recommended, with 8GB being pretty close to bare minimum.

.. _reflex_project_skeleton:

``reflex-project-skeleton``
---------------------------

See - https://github.com/ElvishJerricco/reflex-project-skeleton

This is a bare repository which uses ``reflex-platform`` to provide a nice development enviroment, with both the power of ``nix`` (for binary cache of dependencies) and ``cabal new-*`` commands (for incremental builds).

For a project with both a ``backend`` and ``frontend`` components, this is the recommended setup.

See README and ``reflex-project-skeleton/reflex-platform/project/default.nix`` for usage details.

This also supports cross-compiling the ``frontend`` part to android and iOS platforms!

The following contains information of creating a project-skeleton from scratch and also more details about its working.

https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md

Minimal dev-env using ``reflex-platform``
-----------------------------------------

Please refer to `reflex-platform' README <https://github.com/reflex-frp/reflex-platform/blob/develop/README.md#setup>`_

The ``try-reflex`` script can create a development environment with ``ghc`` and ``ghcjs``. You can use this to have a quick starting setup to compile code-snippets and smaller projects.

When using this for the first time, setup can take considerable time to download all the dependencies from the binary cache.


But for a non-trivial project it is recommended to use ``cabal``.


Using cabal with ``reflex-platform``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you dont have a project with cabal file then use ``cabal init`` to create one.

1. Use the ``workon`` script from reflex-platform to create a development environment (nix shell) according to the dependencies specified in cabal file.
::

  $ ~/reflex-platform/work-on ghcjs ./your-project

  # or just "cabal configure" if working on ghc
  <nix-shell> $ cabal configure --ghcjs
  <nix-shell> $ cabal build

This will use your package's cabal file to determine dependencies. If you have a ``default.nix``, it will use that instead. Note that your project's path must include at least one slash (``/``) so that work-on can detect that it is a path, rather than a package name.

This will give you the exact environment needed to work with the given package and platform, rather than the general-purpose environment provided by the Reflex Platform.

You can replace ghcjs with ghc to hack on the native GHC version of the package (including with GHCi if you want). You can also use a package name instead of a path, which will drop you into the standard build environment of that package; this works even if you don't yet have the source for that package.

Add reflex-platform to project
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. note
  The ``reflex-project-skeleton`` does this, and has many additional benefits

Since the build environment is dependent on the reflex-platform, it is important to keep this dependency as a part of the project. Moreover the version of libraries will change with time in the reflex-platform so it is important to keep a reference to the reflex-platform' "version" which has been used to build the project.

The simplest way to do this is to create a submodule in your project, and use the ``workon`` script from it to create a shell with proper build dependencies.

Assuming you are using git for versioning::

  git submodule add https://github.com/reflex-frp/reflex-platform

  # Then use the workon script to get the nix-shell
  ./reflex-platform/workon ghcjs ./.

A better way is to use the ``nix`` commands, see :ref:`reflex_project_skeleton` or `project-development.md <https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md>`_
 

.. _haddock_and_hoogle:

Local Haddock and Hoogle
------------------------

Local hoogle server can be run from the shell created for development environment by ::

  $ hoogle server --local

To obtain a shell; if you are using

* ``reflex-project-skeleton`` then do::

  $ nix-shell -A shells.ghc

* ``reflex-platform``: Create a shell from either ``try-reflex`` or ``workon``::

From this shell the path of local haddock documentation can also be obtained using::

  # or use ghcjs-pkg
  $ ghc-pkg field <package> haddock-html


GHCi / ghcid with ``jsaddle-warp``
----------------------------------

* ``reflex-project-skeleton``:

  For a simple ghci repl do::

    $ ./cabal new-repl frontend

  or create a shell using nix-build::

    $ nix-shell -A shells.ghc
    $ cabal new-repl frontend

  See the README of the project for more details

  For ``ghcid`` you might have to run the ghcid from the frontend directory so that it detects the ``src`` folder correctly ::

    $ cd frontend; ghcid -c "cd ..; ./cabal new-repl frontend"

* ``reflex-platform``:

  Create a shell from either ``try-reflex`` or ``workon``
  and use the regular ``cabal repl`` or ``ghcid`` commands from your project root.

With ``jsaddle-warp`` package you can run your app in browser without using ``ghcjs``.
You need to modify the ``main`` like the code below. Then you can run it via ``ghci`` or ``ghcid``, and open your application from browser via http://127.0.0.1:3911/::

  module Main where
  
  import Reflex.Dom.Core
  import Language.Javascript.JSaddle.Warp
  
  main = run 3911 $ mainWidget $ text "hello"

This should works fine on Chrome/Chromium, but might not work with firefox.

IDE tools support
-----------------

Instructions for setting emacs/spacemacs are here : https://github.com/reflex-frp/reflex-platform/pull/237
