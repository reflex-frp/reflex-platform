Documentation
=============

Contents
--------

#. Installation
#. Architecture of a reflex application
#. Reflex basics
#. Reflex-dom basics
#. Reflex API reference
#. Reflex-dom API reference

Installation
------------

Architecture of a reflex application
------------------------------------

A typical reflex application consists of widgets which have a Dom view, and
these widgets can create events, and also respond to events.

Widget can be thought as a DOM Structure which has the capability to modify its
contents in response to events or based on a Dynamic value. It can also contain
structures like input fields which can generate events. Moreover user
interaction events can also be generated from the widgets. 

Additionally there can be portions of code (equivalent to a controller) which
does not have a Dom view, but can process input events, maintain a state and
generate output events or dynamic values.

Reflex does not enforce a strict separation between these two.
Sometimes it is a good practice to partition the code in these sub-categories,
like implementing the main business logic in a pure function with a well defined
state machine.

But many times it is better to have independent self-contained widgets, thereby
reducing the complexity of propagating trivial events from view to the
controller.

Example of a simple widget which creates a Click event, and another which
responds to it. (may be button_and_textvisibility.hs)

Another example of a widget which is self-contained ::

  editInPlace
      :: MonadWidget t m
      => Behavior t Bool
      -- ^ Whether or not click-to-edit is enabled
      -> Dynamic t String
      -- ^ The definitive value of the thing being edited
      -> m (Event t String)
      -- ^ Event that fires when the text is edited

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

Finally an example of multiple widgets with circular dependency.

Events can be broadly categorized into:-

  1. Input fields
  2. Mouse / keyboard events
  3. Browser resize, etc.


Reflex basics
-------------

The three main types to understand in Reflex are Behavior, Event, and Dynamic.

#. Behavior
  ``Behavior t a`` abstracts the idea of a value a at all points in time. It must be
  defined for all points in time and at any point you can look at the behavior and
  sample its value. If you need to represent something that does not have a value
  at all points in time, you should probably use Behavior t (Maybe a).

#. Event
  ``Event t a`` abstracts the idea of something that occurs or is updated at discrete
  points in time. An example might be button clicks which would be Event t (), or
  key presses which might be Event t Char. Events are push oriented, i.e. they
  tell you when the value changes.

#. Dynamic
  ``Dynamic t a`` is an abstraction that has a value at all points in time AND can
  notify you when its value is updated. They are essentially a tuple of an Event
  and a Behavior boxed up in a way that keeps everything consistent. They can be
  viewed as a step function over time, with the value changing at every
  occurrence.


Reflex-Dom basics
-----------------

Type classes information (Reflex, MonadWidget, MonadSample, MonadHold, etc)


