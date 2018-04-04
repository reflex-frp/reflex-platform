Resources
=========

.. _tutorials:

Tutorials
---------

* Queensland FP Lab: Functional Reactive Programming with `reflex`

  https://blog.qfpl.io/projects/reflex/

  These are very well written tutorials for beginners. It also has a number of exercises.


* https://github.com/hansroland/reflex-dom-inbits/blob/master/tutorial.md

  This is a single page `long-form` introduction, which covers a lot of material for ``reflex-dom`` applications.

Examples
--------

* https://github.com/gspia/reflex-examples

  A fork of the https://github.com/reflex-frp/reflex-examples, updated to use a recent reflex-platform together with an example on the new project setup (as of early 2018).

  Examples include Basic ToDo, Drag-and-Drop, file input and many more.

* https://github.com/reflex-frp/reflex-dom-contrib

  A collection is useful APIs and DOM widgets.

* https://github.com/gspia/7guis-reflex-nix

  Example of 7 types of GUI tasks from basic counter to a spreadsheet.

..
  This contains some code for poissonLossy
  https://github.com/imalsogreg/my-reflex-recipes - take snippets from this

Applications
------------

Full-Stack Haskell Apps
~~~~~~~~~~~~~~~~~~~~~~~

* http://hsnippet.com/

  A web application to try out reflex in browser.

  The code is somewhat out of date, so latest features in reflex may not be available.

  Code: https://github.com/mightybyte/hsnippet

* https://tenjinreader.com

  An application to read Japanese books.
  Uses :ref:`reflex_project_skeleton`.

  Code: https://github.com/blueimpact/tenjinreader

* https://app.gonimo.com/

  The free baby monitor for smartphone, tablet or PC.

  It has a web + android version of the reflex app

  Code: https://github.com/gonimo/gonimo

Games
~~~~~

* https://mightybyte.github.io/reflex-2048/

  Code: https://github.com/mightybyte/reflex-2048

* https://rvl.github.io/flatris/

  Code: https://github.com/rvl/flatris

  A simple FE only game.
  This also contains an example of auto-reloading development environment

Other
~~~~~

* https://github.com/CBMM/cochleagram

  Tools for psychoacoustics.

  This captures WebAudio, and does the processing to create an audio spectogram.

Reflex Libraries
----------------

.. _dom_ui_libs:

DOM-UI Libraries
~~~~~~~~~~~~~~~~

* Semantic UI components

  https://github.com/reflex-frp/reflex-dom-semui

* Bootstrap Material Design

  https://github.com/hexresearch/reflex-material-bootstrap

  See README for instructions on integrating external js and also for using closure-compiler.

* Material Components

  https://github.com/alasconnect/reflex-material
 
* https://github.com/TaktInc/reflex-dhtmlx

  A wrapper around `date-picker` widget from DHTMLX

* https://github.com/gspia/reflex-dom-htmlea

  This library provides short-hand names for the most common HTML elements and attributes.

  A longer term aim is to provide self contained customisable components providing reasonable default settings with examples, allowing to build demos quickly.
  For example, a table component gives a functionality in which it is possible to select columns, cells, rows and have other ready made functionality.

  Also see https://github.com/gspia/reflex-dom-themes and https://github.com/gspia/reflex-dom-htmlea-vs

Other Libraries
~~~~~~~~~~~~~~~

* https://github.com/diagrams/diagrams-reflex

  Port of the ``diagrams`` library with svg output.
  See the README for supported constructs.

  Examples
  http://bergey.github.io/gooey/

  https://github.com/bergey/gooey

* https://github.com/qfpl/reflex-dom-svg

  This is a work-in-progress helper library for creating svg

* https://github.com/qfpl/reflex-dom-canvas

  An experimental support for canvas element

* https://github.com/reflex-frp/reflex-dom-ace

  This package provides a Reflex wrapper around the ACE editor.

  This is also intended to serve as an example of how to structure FFI packages that rely on external JS packages.

* https://github.com/dfordivam/audiocapture

  Demo for capturing audio via WebAudio APIs

Posts / Blogs
-------------


* https://github.com/mightybyte/real-world-reflex/blob/master/index.md

* https://emmanueltouzery.github.io/reflex-presentation


.. _monad_fix:

MonadFix / RecursiveDo
~~~~~~~~~~~~~~~~~~~~~~

* 24 Days of GHC Extensions: Recursive Do

  https://ocharles.org.uk/blog/posts/2014-12-09-recursive-do.html

* Grokking Fix

  http://www.parsonsmatt.org/2016/10/26/grokking_fix.html

* MonadFix is Time Travel

  https://elvishjerricco.github.io/2017/08/22/monadfix-is-time-travel.html

* Haskell Wiki

  https://wiki.haskell.org/MonadFix

* Typeclassopedia on MonadFix

  https://wiki.haskell.org/Typeclassopedia#MonadFix
