Advanced Topics
---------------

Backend integration
~~~~~~~~~~~~~~~~~~~

Since in most cases you will be writing your backend also in Haskell. It is
important to be able to share code between backend and frontend.

#. Share server-client messages and their serialisation/deserialisation code.

#. Share routes

   This will be backend specific. How to do this??


Assuming you have two projects (either simple cabal projects or with stack, nix),
in order to share code between the two; you have these options:

#. Create a common source directory and share the link of this directory in both
   projects, and add this common directory to ``hs-source-dirs`` of cabal file.

#. Create a separate project (ie with separate cabal file) and add dependency of
   this project to both frontend and backend cabal files.

   This is a better way to handle common code. But you need to provide the
   dependency of local package either through stack.yaml or in your nix config.

   .. todo:: More details on this
             Is it good to provide a scaffolding for this

.. Integration with Yesod, Servant, Snap, etc.
   Any other thing to mention here

Deploying
~~~~~~~~~

You need to serve index.html, rts.js, lib.js, out.js and runmain.js from the
cabal generated folder ``dist/build/<pkg-name>/<pkg-name>.jsexe/``

Simplest way is to copy these files to the *static* directory of your backend
project. This can be automated using simple shell script.

Client Side Routing
~~~~~~~~~~~~~~~~~~~

``reflex-dom-contrib`` has a ``route`` API to provide routing capabilities.

* Change route via Event (like <a> click)
* Get route changes from browser Forward/Back button clicks.
* JS forward/backward calls

https://github.com/reflex-frp/reflex-dom-contrib/blob/master/src/Reflex/Dom/Contrib/Router.hs

.. Here is a post which shows how client side routing can be used. Though it would
   be easier if an example with the route API is provided
..       https://ublubu.tumblr.com/post/144208331227/client-side-routing-in-reflex-dom-notes-1
       servant-router

Designing library/ reusable web snippets
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Libraries - diagrams-reflex, reflex-gloss, etc.
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

FFI
~~~

.. todo:: What is the recommended way to do FFI in reflex app.

Use JQuery, BootStrap, etc?
~~~~~~~~~~~~~~~~~~~~~~~~~~~


Design of project for both ghc and ghcjs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Create both desktop app and web app from same project

