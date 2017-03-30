.. _guide_to_event_management:

A Guide to Event Management
===========================

The reflex package provides many APIs to create the control logic of reflex app
which is independent of the DOM.

See `Quick Ref <https://github.com/reflex-frp/reflex/blob/develop/Quickref.md>`_

In order to leverage the full power of reflex, one has to effectively use
ability to create Event propagation graphs. This guide gives an overview of
various useful techniques.

Overview
--------

In reactive programming you have various sources of events
which have to be utilised for providing responses. For example when user clicks a
button, this event can have various different reponses depending
upon the context or more specifically the state of the application.

The response to an event is most cases do some change in DOM, AJAX request or
change the internal state of application.

In an FRP architecture (atleast in Reflex) this response can be expressed or implemented by

1. Firing another Event.
2. Modification of a Dynamic Value.

Note that there is no explicit callbacks or function calls in response to the
incoming events. Instead there is generation of new Events and modification of
Dynamic values. These Event and Dynamic values are then propagated to widgets
which provide the appropriate response to the event.

Since this propagation of Event/Dynamic values can be cyclic, it can be thought
as an Event propagation graph.

The following sections covers details of constructing this graph.

Event Creation
--------------

The following are the main source of events

#. DOM - See ... for DOM event creation widgets
#. Dynamic - By calling ``updated`` on a Dynamic value one can obtain the event
   when its value changes.

Events can be manipulated using fmap,

Behavior
--------

The sink of a behavior is ``sample``, so it is only useful when a widget is created
dynamically by sampling some behavior, but remain static after its creation.


Dynamic
-------

Event to Dynamic
~~~~~~~~~~~~~~~~

holdDyn -

Tagging Event
~~~~~~~~~~~~~

tagPromptlyDyn -

tag -

Sampling Dynamic: Promptly vs delayed?

attachDyn, tagPromptlyDyn

Creating Event propagation graph
--------------------------------

Simple
~~~~~~

In pure code
Simply pass the Event/Dynamic to input of function

In monadic code
Create simple event propagation tree

Recursive Do
~~~~~~~~~~~~

In Monadic code - create a cyclic graph of event propagation


Problems in cyclic dependency

#. Deadlock - Runtime deadlock due to block on an MVar operation
   This can occur if a widget depends on an Event which is created
   in a ``let`` clause after the widget creation.
   To fix this simply move the ``let`` clause before the widget creation

#. Loop - Output of holdDyn feeds back can cause this??

Collections
~~~~~~~~~~~

Use of Dynamic t [], Dynamic t (Map k v), etc

User data model design : separate guide?

Fanning
~~~~~~~

Split or distribute the event

fan? EventSelector?

Merging/Switching
~~~~~~~~~~~~~~~~~

Dynamic values can be merged simply by ``zipDyn``, mconcat, etc.

Events

Given some events you can choose either to keep them all by using ``align``
align - If two events can possibly happen together (because of a common driver
perhaps), then use this to capture them in a single event.

or select just one from the list using ``leftmost``

or use one of these to merge
mergewith, mergeList - returns a NonEmpty list


mconcatDyn for monoid Dynamic

Higher order FRP
~~~~~~~~~~~~~~~~

Nested structure and flattening


Maintaining State via fold
--------------------------

In order to store a state/data for your app (ie create a state machine) simply
use foldDyn (or foldDynMaybe, foldDynM, foldDynMaybeM).

  stateDynVal <- foldDyn eventhandler initState event

The eventHandler can be a pure API.

Even nested state machines can be designed if your have a nested Dynamic value
by using foldDynM

see nested_dynamic.hs



..
  Simple Reflex stuff guide, no DOM related stuff here.

  https://www.reddit.com/r/reflexfrp/comments/3bocn9/how_to_extract_the_current_value_from_a_text_box/

  Event is probably as you understand it, discrete events. Behavior's are values which change over time (but you don't know when they changed)
  and a Dynamic is Event + Behavior, values which change over time, and you're notified when they change, too.
  The problem with your example, is that omg is not an Event, Behavior or Dynamic but just a String (so it will never change).
  What you might want to do is tag the event with the value from the text box like this:
  omg <- mapDyn (\t -> "myUrl/" ++ t ++ "/me") value questionBox
  dyn <- mkAsyncDyn "default" $ tag (current omg) insertEvent
  This way omg is a Dynamic, so it can change over time. Then we tag the event with the value of the behavior current omg.
  (Note that if we used directly tagDyn omg insertEvent the event would fire both when omg changed as well as when the button was clicked, which is not what we want)
  mkAsyncDyn :: MonadWidget t m => T.Text -> Event t String -> m (Dynamic t (Maybe T.Text))
  mkAsyncDyn defaultValue event = do
    ev <- performRequestAsync $ fmap (\url -> xhrRequest "GET" url def) event
    holdDyn (Just defaultValue) $ fmap _xhrResponse_body ev
  So the takeaway here is that for values to update they need to be reactive type (Event, Behavior, Dynamic), sample is almost never what you want to do.


  https://www.reddit.com/r/reflexfrp/comments/4nyteu/joindyn_and_eboth/
  http://anderspapitto.com/posts/2016-11-09-efficient-updates-of-sum-types-in-reflex.html

