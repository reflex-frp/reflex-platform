Project Development
-------------------

This document describes how to build real-world applications written in
Reflex.

.. contents:: Table of Contents

Creating the Project
--------------------

First, create a directory for your project. This will contain all of the
files needed for the build process and a checkout of
``reflex-platform``, which will provide all of the Haskell libraries and
compilers the project will depend on. To get ``reflex-platform``, it is
easiest to use `git <https://git-scm.com/>`__ and add it as a submodule,
so that the version being used is consistent amongst your team and
updating it is easy.

.. code:: bash

   $ mkdir my-project
   $ cd my-project
   $ git init
   $ git submodule add https://github.com/reflex-frp/reflex-platform

If you’ve never built a project with ``reflex-platform`` before, you may
need to install `Nix <https://nixos.org/nix/>`__ and configure Reflex’s
binary cache. ``reflex-platform`` provides the ``try-reflex`` script,
which will do this for you and download some of the basic tools and
libraries we’ll need ahead of time.

.. code:: bash

   $ reflex-platform/try-reflex

After running this command, you’ll find yourself in a different shell.
This is the ``try-reflex`` sandbox, which provides GHC and GHCJS with
``reflex-dom`` preinstalled. You can use this environment to quickly
test things out, but this document only uses it to install Nix, so go
ahead and ``exit`` out of this shell.

In Reflex-DOM projects, it’s common to have three separate Haskell
components: the frontend, the backend, and the common code shared
between them. It’s easiest to have a separate cabal package for each of
these. We’re going to teach Nix how to build them and how to give us an
environment where they can be built by hand.

Create a directory for each package, then run ``cabal init`` inside them
to create the ``*.cabal`` file and directory structure. If you don’t
have ``cabal`` installed on your system, you can enter the
``try-reflex`` sandbox to use the version that comes with that. We will
see a better way to get the ``cabal`` command later.

.. code:: bash

   $ mkdir common backend frontend
   $ (cd common && cabal init)
   $ (cd backend && cabal init)
   $ (cd frontend && cabal init)

This will prompt for various bits of metadata. ``common`` should be a
library, and ``frontend`` and ``backend`` should be executables. These
cabal files are where the dependencies and build targets of each Haskell
component can be described.

In ``frontend/frontend.cabal`` and ``backend/backend.cabal``, add
``common`` and ``reflex-dom`` as Haskell dependencies.

.. code:: yaml

   ...
     build-depends: base
                  , common
                  , reflex-dom
   ...

Finally, Nix will fail to build ``common`` if it exports no modules.

.. code:: haskell

   -- common/src/Common.hs
   module Common where

.. code:: yaml

   ...
     exposed-modules: Common
   ...

Building with Nix
-----------------

Nix will be used to manage installing dependencies and building the
project. In the root directory of your project, create this
``default.nix`` file:

.. code:: nix

   # default.nix
   { system ? builtins.currentSystem }:
   (import ./reflex-platform { inherit system; }).project ({ pkgs, ... }: {
     packages = {
       common = ./common;
       backend = ./backend;
       frontend = ./frontend;
     };

     shells = {
       ghc = ["common" "backend" "frontend"];
       ghcjs = ["common" "frontend"];
     };
   })

See `project/default.nix <../project/default.nix>`__ for more details on
available options.

You can build individual components of your project using ``-A``.

.. code:: bash

   $ nix-build -o backend-result -A ghc.backend
   $ nix-build -o frontend-result -A ghcjs.frontend

These commands will create two symlinks (``backend-result`` and
``frontend-result``) that point at the build products in the Nix store.

Building with Cabal
-------------------

``nix-build`` is great for release builds since it’s deterministic and
sandboxed, but it is not an incremental build system. Changing one file
will require ``nix-build`` to recompile the entire package. In order to
get a dev environment where changing a module only rebuilds the affected
modules, even across packages, a more incremental tool is required.

``cabal`` is the only tool that simultaneously supports Nix and GHCJS.
The Nix expression in ``default.nix`` uses ``shells`` to setup
``nix-shell`` sandboxes that ``cabal`` can use to build your project.
The ``shells`` field in ``default.nix`` defines which platforms we’d
like to develop for, and which packages’ dependencies we want available
in the development sandbox for that platform. Note that specifying
``common`` is important; otherwise it will be treated as a dependency
that needs to be built by Nix for the sandbox.

You can use these shells with ``cabal.project`` files to build all three
packages in a shared incremental environment, for both GHC and GHCJS.
``cabal.project`` files are how you configure ``cabal new-build`` to
build your local project. It’s easiest to have a separate file for GHC
and GHCJS.

.. code:: yaml

   -- cabal.project
   packages:
     common/
     backend/
     frontend/

.. code:: yaml

   -- cabal-ghcjs.project
   compiler: ghcjs
   packages:
     common/
     frontend/

To build with GHC, use the ``nix-shell`` command to enter the sandbox
shell and use ``cabal`` (which is supplied by the sandbox):

.. code:: bash

   $ nix-shell -A shells.ghc
   [nix-shell:~/path]$ cabal new-build all

To build with GHCJS:

.. code:: bash

   $ nix-shell -A shells.ghcjs
   [nix-shell:~/path]$ cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all

You can also run commands in the nix-shell without entering it
interactively using the ``--run`` mode. This is useful for scripting.

.. code:: bash

   $ nix-shell -A shells.ghc --run "cabal new-build all"
   $ nix-shell -A shells.ghcjs --run "cabal --project-file=cabal-ghcjs.project --builddir=dist-ghcjs new-build all"

``nix-shell`` will put you in an environment with all the dependencies
needed by your project, including the ``cabal`` tool. It reads your
``*.cabal`` files to determine what Haskell dependencies to have
installed when you enter the sandbox, so you do not need to manually run
``cabal install`` to get Haskell dependencies. Just like Stack, all you
have to do is add them to the ``build-depends`` field in you cabal file.

**Note:** Cabal may complain with
``Warning: The package list for 'hackage.haskell.org' does not exist. Run 'cabal update' to download it.``
This can be ignored since we are using Nix instead of Cabal’s own
package manager. Nix uses a package snapshot similar to a Stackage LTS.

Building frontends with GHC
---------------------------

GHCJS can be quite slow, especially if you are using Template Haskell.
Building the frontend with GHC can drastically speed up build times, and
enables you to test from GHCi for even faster reloads.

JSaddle is a set of libraries that allows Reflex-DOM to swap out its
JavaScript backend easily. By default, Reflex-DOM’s ``mainWidget`` will
work on GHC out of the box, using the ``jsaddle-webkit2gtk`` backend. So
simply building your ``frontend`` package using GHC will produce a
working native program that renders DOM using WebKit. This is
recommended for native desktop releases.

To build this with ``nix-build``:

.. code:: bash

   $ nix-build -o ghc-frontend-result -A ghc.frontend

To build it with ``cabal``:

.. code:: bash

   $ nix-shell -A shells.ghc --run "cabal new-build frontend"

``jsaddle-warp`` is an alternative JSaddle backend that uses a local
``warp`` server and WebSockets to control a browser from a native
Haskell process. This is recommended to allow testing different
browsers, and to make use of a browser’s significantly better developer
tools.

To use it, enable the ``useWarp`` option in ``default.nix``.

.. code:: nix

   # default.nix
   (import ./reflex-platform {}).project ({ pkgs, ... }: {
     useWarp = true;

     packages = {
       common = ./common;
       backend = ./backend;
       frontend = ./frontend;
     };

     shells = {
       ghc = ["common" "backend" "frontend"];
       ghcjs = ["common" "frontend"];
     };
   })

Running the GHC-built frontend with this option will spawn the Warp
server on port 3003, which you can connect your browser to to run the
app. It will also compile under GHCJS as is, automatically defaulting
back to the GHCJS backend. Both ``jsaddle-warp`` and
``jsaddle-webkit2gtk`` are safe to use from GHCi, so you can test
changes even more quickly with ``:r``.

**Note:** The native backends for JSaddle have much much better runtime
performance than the GHCJS backend. To put it in perspective, the native
backends running on most mobile phones will outperform most desktops
running the GHCJS backend. GHCJS is quite fast, especially considering
all it has to do; but native Haskell is simply much faster than a JS VM
for what Reflex is doing.

Building mobile apps
--------------------

The project Nix expression also supports defining mobile apps.

.. code:: nix

   (import ./reflex-platform {
     config.android_sdk.accept_license = true;
   }).project ({ pkgs, ... }: {
     packages = {
       common = ./common;
       backend = ./backend;
       frontend = ./frontend;
     };

     shells = {
       ghc = ["common" "backend" "frontend"];
       ghcjs = ["common" "frontend"];
     };

     android.frontend = {
       executableName = "frontend";
       applicationId = "org.example.frontend";
       displayName = "Example Android App";
     };

     ios.frontend = {
       executableName = "frontend";
       bundleIdentifier = "org.example.frontend";
       bundleName = "Example iOS App";
     };
   })

Note that you must accept the `Android Software Development Kit License
Agreement <https://developer.android.com/studio/terms>`__ and indicate
so by setting ``config.android_sdk.accept_license`` when instantiating
Reflex Platform in order to build Android apps.

Build them with ``nix-build``:

.. code:: bash

   $ # On Linux
   $ nix-build -o android-result -A android.frontend
   $ # On macOS
   $ nix-build -o ios-result -A ios.frontend

Currently, Android apps can only be built on Linux, and iOS apps can
only be built on macOS. If you would like to launch builds from an
unsupported platform, you can use Nix `distributed
builds <https://nixos.org/nixos/manual/options.html#opt-nix.buildMachines>`__.

For example, to build the Android app from a Mac configured with a Linux
remote builder:

.. code:: bash

   $ nix-build -o android-result -A android.frontend --arg config '{system="x86_64-linux";}'

Note that only ``android.frontend`` was built in this case. Currently,
Android apps can only be built on Linux, and iOS apps can only be built
on macOS. If you would like to get both in the result directory, use
``-A all`` and make sure to have Nix `distributed
builds <https://nixos.org/nixos/manual/options.html#opt-nix.buildMachines>`__
set up. Nix will delegate builds to remote machines automatically to
build the apps on their required systems.

Building via remote Nix Builder
-------------------------------

For some use-cases it can be required to build derivations to be
deployed on a different system than the one used for building. For
example, a derivation needs to be deployed to ``x86_64-linux`` but the
system used for building is ``x86_64-darwin``.

Nix supports delegating builds to other machines using `remote
builders <https://nixos.org/nix/manual/#chap-distributed-builds>`__. For
the above example, the
`nix-docker <https://github.com/LnL7/nix-docker>`__ project might be
useful, as it provides a Docker-based Linux build environment usable on
Darwin machines. After having set up remote builders, the Reflex
application can be built for x86_64-linux by passing the appropriate
``system`` argument:

.. code:: bash

   $ nix-build --argstr system x86_64-linux
