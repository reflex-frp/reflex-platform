Overview
========

``reflex``

  provides the Functional Reactive Programming (FRP) implementation. 

  This is the base for ``reflex-dom`` but is independent of the DOM / web interface design code, and can be used in many other applications.

  See `Quick Ref <https://github.com/reflex-frp/reflex/blob/develop/Quickref.md>`_

``reflex-dom-core`` and ``reflex-dom``

  provides a APIs for constructing DOM widgets, do websocket / XHR requests, etc.

  Most of the functionality is part of the ``reflex-dom-core`` package.

  See `Quick Ref <https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md>`_

.. _reflex_basics:

Reflex Basics
-------------


The ``reflex`` package provides the foundation for the FRP architecture.
It consists of many type class definitions and their implementations, and the most important type class in this package is ``Reflex``.


The three main types to understand in ``Reflex`` are Behavior, Event, and Dynamic.

#. Behavior

   A container for a value that can change over time.  'Behavior's can be
   sampled at will, but it is not possible to be notified when they change

   ``Behavior t a`` abstracts the idea of a value ``a`` at all points in time. It must be
   defined for all points in time and at any point you can look at the behavior and
   sample its value. If you need to represent something that does not have a value
   at all points in time, you should probably use ``Behavior t (Maybe a)``.

#. Event

   ``Event t a`` abstracts the idea of something that occurs or is updated at discrete
   points in time. An example might be button clicks which would be ``Event t ()``, or
   key presses which might be ``Event t Char``. Events are push oriented, i.e. they
   tell you when the value changes.

#. Dynamic

   ``Dynamic t a`` is an abstraction that has a value at all points in time AND can
   notify you when its value is updated. They are essentially a tuple of an Event
   and a Behavior boxed up in a way that keeps everything consistent. They can be
   viewed as a step function over time, with the value changing at every
   occurrence.

   We use ``Dynamic`` in ``reflex-dom`` in a lot of places where you might expect to use ``Behavior`` in various other FRP settings because the DOM API is fundamentally push-based: you pretty much have to explicitly tell things to update, the browser isn't asking our program which DOM tree should be displayed, so we have to know when the values change.

The ``t`` type parameter indicates which *timeline* is in use.
Timelines are fully-independent FRP contexts, and the type of the timeline determines the FRP engine to be used. This is passed to every FRP-enabled datatypes
and it ensures that wires don't get crossed if a single
program uses Reflex in multiple different contexts.

In reactive programming you have various sources of events
which have to be utilised for providing responses. For example when user clicks a
button, this event can have various different reponses depending
upon the context or more specifically the state of the application.

The response to an event in most cases will do some changes like modify DOM, communicate with server or change the internal state of application.

In Reflex this response can be expressed or implemented by

1. Firing another ``Event``.
2. Modification of a ``Dynamic`` Value.

Note that there is no explicit callbacks or function calls in response to the
incoming events. Instead there is generation of new Events and modification of
Dynamic values. These Event and Dynamic values are then propagated to widgets
which provide the appropriate response to the event.

Since this propagation of ``Event``/``Dynamic`` values can be cyclic, it can be thought
as an Event propagation graph.

For more details see :ref:`reflex_event`

Architecture of a Reflex-DOM Application
----------------------------------------

A typical Reflex-DOM application consists of widgets, and some glue code to *connect* the widgets together.

Widget can be thought as a DOM Structure which has the capability to modify its
contents in response to events or based on some dynamic values. It can also contain
structures like input fields which can generate events. Moreover user
interaction events like mouse clicks can also be captured from the widgets.

Additionally there are some pieces of code (equivalent to a controller) which
does not have a Dom view, but can process input events, maintain a state and
generate output events or dynamic values.

These controller can encapsulate the logic behind handling of incoming events,
they can transform (using Functor) or filter (using Applicative) these events
and dynamic values as per the need. This way user has the power to create custom
event flows which can be either restricted/local to some widgets or span the
entire app.

Reflex does not enforce a strict separation between these two, and user has the
complete flexibility to choose a suitable design.

Sometimes it is a good practice to partition the code in these sub-categories,
like implementing the main business logic in a pure function or a state machine, and the view in a separate module.

But many times it is better to have independent self-contained widgets, thereby
reducing the complexity of propagating trivial events from view to the
controller.

Also see the reddit thread `how to structure a reflex application. <https://www.reddit.com/r/reflexfrp/comments/6l5ddn/how_to_structure_a_reflex_application/>`_

Monadic DOM
-----------

The HTML DOM is constructed as a tree of "Objects" in which both the "sequence" of objects in the tree and their "heirarchy" has to be specified.

In ``reflex-dom``, DOM creation works in a Monad ``MonadWidget``. Since it is monadic, the sequence of function calls directly correspond to the sequence of DOM elements.
To create heirarchy a lot of basic widgets take an addition argument of type (m a) which will be nested inside it.

For example::

  let myText = do -- Specifies sequence
     el "h1" (text "Header") -- Nesting
     text "Content"

  el "div" myText -- Nesting


The top widget is created in the beginning when the browser opens the app.
But its contents can be modified with "time" ie the nested widgets can be a Dynamically varying



View-Controller Architecture
----------------------------

Separate APIs to manage events and to render view ::

  -- button_and_textvisibility.hs
  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE LambdaCase #-}

  -- This code demonstrates use of an event to create dynamic values
  -- Simple flow of an event from one widget to another.
  main = mainWidget $ do

    -- View Widget to Generate Events
    -- button widget is defined in library, it creates a simple button
    evClick <- button "Click Me!"

    -- Controller
    -- Handle events and create a 'Dynamic t Bool' value
    -- This toggles the visibility when the button is pressed
    isVisible <- foldDyn (\_ b -> not b) False evClick

    -- View
    -- This is a simple widget that takes a 'Dynamic t Bool' as input
    textWithDynamicVisibility isVisible

    return ()

  -- This widget takes the input value of visibility
  -- and creates a view based on that
  textWithDynamicVisibility isVisible = do
    let dynAttr = ffor isVisible
                   (\case
                     True -> ("style" =: "")
                     False -> ("style" =: "display: none;"))

    elDynAttr "div" dynAttr $
      text "Click the button again to make me disappear!"


Widgets Interacting Together
----------------------------

By using the recursive-do notation we can connect the widgets together.
This is a simple example of creating a cicular Event-Dynamic propagation.::

  -- button_and_textvisibility_2.hs
  {-# LANGUAGE OverloadedStrings #-}
  {-# LANGUAGE LambdaCase #-}
  {-# LANGUAGE RecursiveDo #-} -- This is important!

  -- This code demonstrates use of an event to create dynamic values
  -- Circular flow of Event/Dynamic using Recursive-do syntax
  main = mainWidget $ do

    rec
      -- Controller
      -- Handle events and create a 'Dynamic t Bool' value
      -- This toggles the visibility when the button is pressed
      isVisible <- foldDyn (\_ b -> not b) False evClick

      -- View
      -- This widget creates the button and its click event,
      -- The click event is propagated to the controller
      evClick <- textWithDynamicVisibility isVisible

    return ()

  -- This widget takes the input value of visibility
  -- and creates a view based on that
  textWithDynamicVisibility isVisible = do
    -- View Widget to Generate Events
    -- button widget is defined in library, it creates a simple button
    evClick <- button "Click Me!"

    let dynAttr = ffor isVisible
                   (\case
                     True -> ("style" =: "")
                     False -> ("style" =: "display: none;"))

    elDynAttr "div" dynAttr $
      text "Click the button again to make me disappear!"

    return evClick

As you can see this helps to completely separate the View widget and controller code.

But the real power of recursive-do notation can be utilised in creating more
complex *Integrated* widgets as desribed in the next section.


Integrated Widget Architecture
------------------------------

In Reflex it is possible to combine the view and controller part of the code to
create integrated widgets which can be plugged in easily in your app.

Example of a widget which is self-contained. This widget creates a simple text field, which can be edited by clicking on it.
`Source <https://github.com/reflex-frp/reflex-dom-contrib/blob/4825ff4abdff35a2719bd1dc7ba58b164ec02229/src/Reflex/Dom/Contrib/Widgets/EditInPlace.hs>`_::

  editInPlace
      :: MonadWidget t m
      => Behavior t Bool
      -- ^ Whether or not click-to-edit is enabled
      -> Dynamic t String
      -- ^ The definitive value of the thing being edited
      -> m (Event t String)
      -- ^ Event that fires when the text is edited

Quoting `mightybyte <https://github.com/mightybyte>`_

  This defines the entire interface to this widget. What makes this example particularly
  interesting is that the widget has to maintain some internal state in order to implement
  its functionality. Namely, it has to keep track of the Viewing/Editing state.
  Reflex allows widgets to handle this kind of state internally without needing to
  add it to some top-level application-wide state object.
  This hugely improves composability and ultimately allows you to build GUI apps
  just like you would any other Haskell app--main is your overarching top-level function
  and then you split out whatever widgets it makes sense to split out.
  Your guide for splitting things will probably be that you want to find pieces that are
  loosely connected to everything else in terms of inputs and ouputs and make them their own function.


Overview of ``ghcjs`` and ``jsaddle`` Packages
-----------------------------------------


``ghcjs``

  Is the compiler, like ``ghc``.

``ghcjs-dom``

  Provides the interface APIs to work with DOM and Web APIs, either on a browser (by compiling with ``ghcjs``) or natively using webkitgtk (when compiled with ``ghc``)

  Applications should use the ``ghcjs-dom`` package and the ``GHCJS.DOM.*`` modules it contains; to get the best mix of protability and performance (rather than using the ``jsaddle-dom``, ``ghcjs-dom-jsaddle`` and ``ghcjs-dom-jsffi`` directly).


.. note:: The below package descriptions are provided for information only. For using reflex-dom in applications ghcjs-dom should be sufficient.

``ghcjs-base``

  Is the base library for ``ghcjs`` for JavaScript interaction and marshalling

  This package should be included in cabal only if using ``ghcjs`` by adding this ::

    if impl(ghcjs)
      build-depends: ghcjs-base

``jsaddle``

  JavaScript interface that works with ``ghcjs`` or ``ghc``.

  It provides a set of APIs to do arbitrary JS execution in a type-safe manner.

  * If compiled with ``ghc`` on native platforms like WebKitGtk, WKWebView on iOS / macOS or Android using JNI.

    It uses a `JavaScript command interpreter` for each of the different targets.

  * If compiled with ``ghc`` using ``jsaddle-warp`` and running on browser.

    The JS commands are encoded in the executable running on native platform, and sent to the browser for execution using a websocket connection.

  * If compiled with ``ghcjs``, it uses some JSFFI calls to execute the functions indirectly.

    Note: this has poor performance compared to calling the DOM APIs directly through ``ghcjs-dom-ffi`` as the DOM API calls are wrapped in an execution script.

  See `README <https://github.com/ghcjs/jsaddle/blob/master/README.md>`_ for more details.

``ghcjs-base`` and ``jsaddle`` form the base for these packages

``ghcjs-dom-ffi``

  This package implements the entire DOM/Web API interface as direct JSFFI calls.

  On browser this is the most optimal way to execute DOM related actions.

``ghcjs-dom-jsaddle`` and ``jsaddle-dom``

  This provides the DOM/Web API interface using ``jsaddle``
