
Architecture of a Reflex Application
------------------------------------

A typical reflex application consists of widgets, and some glue code to *connect* the widgets together.

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
complete flexibility to chose a suitable design.

Sometimes it is a good practice to partition the code in these sub-categories,
like implementing the main business logic in a pure function or a state machine, and the view in a separate module.

But many times it is better to have independent self-contained widgets, thereby
reducing the complexity of propagating trivial events from view to the
controller.


Overview of Reflex Basics
~~~~~~~~~~~~~~~~~~~~~~~~~

The reflex package provides the foundation for the FRP architecture through the
type class definitions, and the most important type class in this package is ``Reflex``.


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

The ``t`` type parameter indicates which *timeline* is in use.
Timelines are fully-independent FRP contexts, and the type of the timeline determines the FRP engine to be used. This is passed to every FRP-enabled datatypes
and it ensures that wires don't get crossed if a single
program uses Reflex in multiple different contexts.

View-Controller Architecture
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In Reflex it is possible to combine the view and controller part of the code to
create integrated widgets which can be plugged in easily in your app.

Example of a widget which is self-contained ::

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

Single Page App vs Other designs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Reflex is suitable primarily for single-page apps.

.. todo:: Add ways to build non-single-page apps.

.. See :ref:`guide_to_event_management` for more info on how to construct the event graph using the APIs.

.. See :ref:`guide_to_dom_creation` for more info on how to create DOM using APIs from Reflex-DOM.





