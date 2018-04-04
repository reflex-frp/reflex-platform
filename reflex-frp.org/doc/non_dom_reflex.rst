
Non-DOM related usage of ``reflex``
===================================

The ``reflex`` FRP architecture (and package) can be used to create non-DOM based UI application and even some non-UI stuff like server.

``reflex-host``
---------------

Source : https://github.com/bennofs/reflex-host

This provides a set of higher-level abstractions on top of the ``reflex`` FRP primitives.

Using this library, you don't need to build your own event loop. You can just start registering external events and performing actions in response to FRP events.

* https://github.com/dalaing/reflex-host-examples

  This has a set of examples using this package

* https://github.com/dalaing/reflex-basic-host

  Contains an even simplified API interface

UI
--

* https://github.com/reflex-frp/reflex-sdl2

  Experimental SDL 2 based reflex app using sdl2 haskell bindings.

* https://github.com/deech/fltkhs-reflex-host

  An experimental code for `FLTK GUI toolkit` based applications using reflex.

* https://github.com/lspitzner/bricki-reflex

  Experimental ``brick`` based terminal UI.

  http://hexagoxel.de/postsforpublish/posts/2017-10-30-brick-plus-reflex.html

Other
-----

* https://github.com/dalaing/reflex-server-websocket
