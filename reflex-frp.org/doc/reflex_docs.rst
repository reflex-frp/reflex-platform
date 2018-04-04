Reflex
======

The reflex package provides APIs to create the FRP control logic which is independent of the DOM.

The `Quick Ref <https://github.com/reflex-frp/reflex/blob/develop/Quickref.md>`_ provides a really nice overview.


FRP Basics
----------

In order to leverage the full power of reflex, one has to effectively use the
ability to create an Event propagation graphs, and use it to model the business logic.
This guide gives an overview of basics and various useful techniques.

Also see :ref:`reflex_basics`

.. _reflex_event:

``Event``
~~~~~~~~~

Creation
^^^^^^^^

.. _new_trigger_event:

newTriggerEvent
***************

  Is used to inject value in the ``reflex`` event-propagation-graph from outside using IO action::
  
       newTriggerEvent :: TriggerEvent t m
         => m (Event t a    -- Event triggered by fun
              , a -> IO ()) -- fun
  
  ``newTriggerEvent`` can also be used to break a big ``rec`` block.::
  
       rec
         ev1 <- widget1 evN
  
         ..
         ..
  
         evN <- widgetN evN_1
  
  In this the ``widgetN`` and many other widgets in-between can be pulled outside the ``rec`` block::
  
       (evN, evNIOAction) <- newTriggerEvent
  
       ev1 <- widget1 evN
  
       ..
       ..
  
       evN' <- widgetN evN_1
  
       performEvent $ ((\v -> liftIO $ evNIOAction v) <$> evN')

From ``Dynamic``
****************

  By calling ``updated`` on a ``Dynamic`` value one can obtain the event when its value changes.::
  
    updated :: (Reflex t) => Dynamic t a -> Event t a
   
Repeating Events
****************

  Using APIs from ``Reflex.Time`` one can create repeating events.::
  
    tickLossy :: (_)
      => NominalDiffTime -- in seconds
      -> UTCTime
      -> m (Event t TickInfo) 
  
  ``tickLossy`` will create an ``Event`` every ``n`` seconds. Though it is not guaranteed to always fire an ``Event`` after the elapsed time, especially if the value ``n`` is very small.
  
  There are many more APIs in this module to generate repeating events based on more complex algorithms.

From DOM widgets
****************

  When doing DOM based programming using ``reflex-dom-core``, a number of widgets provide ``Event`` in response to the external events.
  
  * Input fields like button, text-box, drop down, etc.
  
    See :ref:`dom_input_elements`
  
  * User interaction events like mouse click, mouse over, etc.
  
    See :ref:`dom_events`
  
  * Response from XHR / AJAX / websocket requests
  
    See :ref:`xhr_websocket`
  
  * Arbitrary `on` events from the browser
  
    See :ref:`ffi`

Manipulation
^^^^^^^^^^^^

Using these primary ``Event``\s you can create secondary / derived events by

#. Manipulated the value using fmap::

    -- inputValueEv :: Event t Int

    doubledInputValueEv = ffor inputValue (* 2)

#. Filter the value::

    -- inputValueEv :: Event t Int

    -- This Event will fire only if input value is even
    evenOnlyEv = ffilter even inputValueEv

   Use ``fmapMaybe fforMaybe`` for similar filtering

#. Multiple events can be combined using

   Merges the value `a` ::

       <>         :: Semigroup a => Event a -> Event a -> Event a


   This fires the `a` event only when `b` is not firing at the same time::

      difference :: Event a -> Event b -> Event a

   Combine two separate events::

      align      ::                     Event a -> Event b -> Event (These a b)
      alignWith  :: (These a b -> c) -> Event a -> Event b -> Event c

   Combine a list of events::

      mergeWith  :: (a -> a -> a) -> [Event a] -> Event a
      mergeList  :: [Event a] -> Event (NonEmpty a)

   Drop all except the `leftmost` event::

      leftmost   :: [Event a] -> Event a
    
   Other APIs::

      mergeMap   :: Ord k => Map k (Event a) -> Event (Map k a)
      merge      :: GCompare k => DMap (WrapArg Event k) -> Event (DMap k)

#. Tagging value of ``Dynamic`` or ``Behavior``.

   Using these APIs, see
   `Quick Ref <https://github.com/reflex-frp/reflex/blob/develop/Quickref.md#functions-producing-event>`_
   ::

      gate                       ::                     Behavior Bool -> Event a -> Event a
      tag                        ::                        Behavior a -> Event b -> Event a
      tagPromptlyDyn             ::                         Dynamic a -> Event b -> Event a
      attach                     ::                        Behavior a -> Event b -> Event (a, b)
      attachPromptlyDyn          ::                         Dynamic a -> Event b -> Event (a, b)
      attachWith                 :: (a -> b ->       c) -> Behavior a -> Event b -> Event c
      attachPromptlyDynWith      :: (a -> b ->       c) ->  Dynamic a -> Event b -> Event c
      attachWithMaybe            :: (a -> b -> Maybe c) -> Behavior a -> Event b -> Event c
      attachPromptlyDynWithMaybe :: (a -> b -> Maybe c) ->  Dynamic a -> Event b -> Event c
      <@>                        ::                 Behavior (a -> b) -> Event a -> Event b
      <@                         ::                        Behavior a -> Event b -> Event a

   The below will create an event which will fire whenever the Dynamic changes and give the *old* value of the Dynamic.
   ::
    tag (current dyn) $ updated dyn


``Behavior``
~~~~~~~~~~~~

``Behavior`` value can be tagged with an ``Event`` using ``tag`` or ``attach``, or it can be sampled in a widget, when it is first created using ``sample``.

``Dynamic``
~~~~~~~~~~~

Creation
^^^^^^^^

  Create a ``Dynamic`` which changes value when ``Event`` occurs::
  
    holdDyn :: (MonadHold t m) => a -> Event t a -> m (Dynamic t a)
  
  There are also a number of input APIs in ``reflex-dom-core`` which provide ``Dynamic`` values in the context of DOM. See :ref:`dom_input_elements`

Manipulation
^^^^^^^^^^^^

  Using some primary ``Dynamic`` values you can create secondary / derived values by
  
  * ``fmap`` - Simply use functor instance when only one ``Dynamic`` value is being manipulated.
  
  * Combine multiple ``Dynamic`` values using::
    
      zipDyn :: Reflex t => Dynamic t a -> Dynamic t b -> Dynamic t (a, b)
  
      zipDynWith :: Reflex t => (a -> b -> c) -> Dynamic t a -> Dynamic t b -> Dynamic t c
  
    Zipping is useful when multiple ``Dynamic`` values have a common point of influence
    in the application.
  
    For example if you have two variable parameters like color and font of text.
    Then you can construct the dynamic attributes from these parameters by simply
    zipping them together.::
  
      -- textFont :: Dynamic t Text
      -- textColor :: Dynamic t Text
  
      getAttr (f,c) = ("style" =: ("font-family: " <> f "; color: " <> c))
  
      elDynAttr "div" (getAttr <$> (zipDyn textFont textColor)) $ text "Text"
  
  * Using ``Applicative``::
  
       -- dInt1, dInt2, dInt3 :: Dynamic t Int
       let
         eInt :: Dynamic t (Int, Int, Int)
         eInt = (,,) <$> dInt1 <*> dInt2 <*> dInt3
     
    Much more complicated things can be done using ``traverse``/ ``sequenceA``::
  
      -- mDyn :: Map k (Dynamic t Int)
      let
        dMap :: Dynamic t (Map k Int)
        dMap = sequenceA mDyn
      
     
  .. note:: ``zipDynWith`` is more efficient than ``f <$> d1 <*> d2``

``Reflex``
~~~~~~~~~~

The ``Reflex`` class provides the basic functionality for FRP. It provides the basic functions to efficiently handle the ``Event``, ``Behavior`` and ``Dynamic`` values.
All the `pure` APIs like ``tagDyn``, ``zipDyn``, etc are created using the functionality provided through ``Reflex`` class.

The other two most important features required for FRP are maintaining some state, and doing modifications based on events. This is provided from the two classes ``MonadHold`` and ``Adjustable``.

Also see `QuickRef <https://github.com/reflex-frp/reflex/blob/develop/Quickref.md#typeclasses-to-introspect-and-modify-an-frp-network>`_

``MonadHold``
~~~~~~~~~~~~~

This is required to create any stateful computations with Reflex.
It designates monads that can create new ``Behavior`` s based on ``Event`` s.::

  hold :: a -> Event t a -> m (Behavior t a)


``Adjustable``
~~~~~~~~~~~~~~

A Monad that supports adjustment over time. After an action has been run, if the given events fire, it will adjust itself so that its net effect is as though it had originally been run with the new value.::

  runWithReplace :: m a -> Event t (m b) -> m (a, Event t b) 


Event Propagation Graph
-----------------------

.. Its probably better to just give some example here?

Simple Tree
~~~~~~~~~~~

Simply pass the ``Event``/``Dynamic`` values to input of functions. This will create kind of an event propagation flow from top to bottom. But no feedback-loops can be created, for that use ``RecursiveDo``. 

RecursiveDo
~~~~~~~~~~~

Is used to create a cyclic event propagation graph. Because the underlying mechanism of graph creation is monadic (using ``MonadHold``, etc). To create feedback-loops we need to use ``MonadFix``.

The actual usage is quite simple::

  -- Required extension for rec style blocks
  -- {-# LANGUAGE RecursiveDo #-}

  rec
    let
      ev1 = f2 <$> ev2
    d1 <- widgetHold (w1Init) (w1 <$> ev1)
    ev2 <- viewD1Widget d1

in this example the ``ev1`` is used to create a ``Dynamic`` value ``d1``, which is then shown to the user using ``viewD1Widget``.
This widget can in turn modify the value using the ``Event`` ``ev2``.

But there are some pitfalls too, see debugging :ref:`hang_stack_overflow`

For more details checkout the articles on :ref:`monad_fix`

.. _maintain_state:

Maintaining State via fold
~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to store a state/data for your app (ie create a state machine) simply
use ``foldDyn``

::

  -- State can be any arbitrary haskell data
  stateDynVal :: Dynamic t MyState

  -- ev can a collection of all events on which the state depends
  -- For example all input events
  ev :: Event t Inputs

  -- This is a pure API which can process the input events and current state
  -- to generate a new state.
  eventHandler :: (Inputs -> MyState -> MyState)

  -- foldDyn :: (a -> b -> b) -> b -> Event t a -> Dynamic t b
  stateDynVal <- foldDyn eventHandler initState ev

Even nested state machines can be designed if your have a state with nested ``Dynamic`` value by using ``foldDynM``

See `nested_dynamic.hs <https://github.com/reflex-frp/reflex-frp.org/blob/master/code-snippets/nested_dynamics.hs>`_

Use ``foldDynMaybe``, ``foldDynMaybeM`` in cases where you want to filter input
events, such that they don't modify the state of application.

For example in a shopping cart if the user has not selected any items, the "add
to cart" button should do nothing. This kind of behavior can be implemented by
returning ``Nothing`` from the eventHandler.


``getPostBuild``
~~~~~~~~~~~~~~~~
::

  getPostBuild :: PostBuild t m => m (Event t ())

This ``Event`` will fire once at the start of an action / DOM widget is created. Also each time that part of the DOM gets re-created (like if it is created from scratch via ``widgetHold``). This can be used to do communication with server or do some FFI.

Note that the ``Event`` fires when the build action completes, but the fragment may not yet be in the browser DOM. So you might have to add some delay to this before accessing the DOM via some FFI.

Doing IO via ``performEvent``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Example::

  doneEv <- performEvent (ffor triggerEv $ \val -> liftIO $ do
    putStrLn "Doing some action"
    someIOAction val)
 
  widgetHold (text "Waiting for action to complete")
    (showResultOfAction <$> doneEv)

.. todo:: Does the doneEv always occur in the frame after triggerEv?

.. _debounce:

Debounce, Delay, BatchOccurence
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``Reflex.Time`` provides a set of useful APIs which come handy when you need to do real life event handling.::

  debounce :: (_) => NominalDiffTime -> Event t a -> m (Event t a)

  -- Wait for user to stop typing for 0.5 sec, and then send a search request to server

  searchTextEv <- debounce 0.5 (_textInput_input someTextInput)

When doing FFI ``delay`` is required::

  delay :: (_) => NominalDiffTime -> Event t a -> m (Event t a) 


  performEvent (abort <$ stopAndRestartEv)
  delayedEv <- delay 0.2 stopAndRestartEv
  performEvent (start <$ delayedEv)
  
When handling a set of events from external sources many times the sequence of events is not deterministic,
or perhaps we want a ``debounce`` kind of functionality but dont want to miss any ``Event``.
In such cases we need to use ``batchOccurrences`` to properly model the logic.::

  batchOccurrences :: (_) => NominalDiffTime -> Event t a -> m (Event t (Seq a)) 

  

Higher order FRP
----------------

Nested Values and flattening
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

When you model real world ``Dynamic`` values many times you end up with nested
structures.

For example, if the value of items in a shopping cart depends on the shipping
method chosen, then you can end up with a value ``total' :: Dynamic t [Dynamic t Int]``::

  selectedItems :: Dynamic t [Item]
  isExpeditedShipping :: Dynamic t Bool

  total' = Dynamic t [Dynamic t Int]
  total' = ffor selectedItems
            (map getItemPrice)

  getItemPrice :: Item -> Dynamic t Int
  getItemPrice itm = ffor isExpeditedShipping
                      (\case
                        True -> (itemPrice itm) + (shippingCharges itm)
                        False -> itemPrice itm)

In such cases in order to get a total value ``Dynamic t Int``, you need to use
flattening APIs. In case of ``Dynamic`` it is simply ``join`` from
``Control.Monad`` (since ``Dynamic`` has an instance of ``Monad``)::

  total'' :: Dynamic t (Dynamic t Int)
  total'' = foldr1 (\a b -> (+) <$> a <*> b) <$> total'

  total :: Dynamic t Int
  total = join total''

See `QuickRef <https://github.com/reflex-frp/reflex/blob/develop/Quickref.md#flattening-functions>`_
for details on other flattening APIs.

Dynamic widgets on Dynamic Collections
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In order to model complex flows of events or dynamically changing data
collection, we need to use higher order containers like lists (``[]``) or Maps
(``Data.Map``).

To effectively work with such ``Dynamic`` collections, ``Reflex.Collection`` provides a bunch of APIs.

See Quickref for a summary of these APIs 
https://github.com/reflex-frp/reflex/blob/develop/Quickref.md#collection-management-functions

..
  A tutorial on this is in pipeline by dalaing


``Reflex.Network``
~~~~~~~~~~~~~~~~~~

Provides these APIs.
If you look closely they are the equivalent of ``dyn`` and ``widgetHold``, but work in non-DOM applications.::

  networkView :: (Reflex t, NotReady t m, Adjustable t m, PostBuild t m)
    => Dynamic t (m a) -> m (Event t a)
  
  networkHold :: (Reflex t, Adjustable t m, MonadHold t m)
    => m a -> Event t (m a) -> m (Dynamic t a)


``EventWriter`` and ``DynamicWriter``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

``EventWriter`` allows you to send events "upwards" in your widget hierarchy, much like Elm's update propagation.::

  -- Main APIs
  runEventWriterT :: (Reflex t, Monad m, Semigroup w) => EventWriterT t w m a -> m (a, Event t w)
  tellEvent :: EventWriter t w m => Event t w -> m ()

  -- Example usage
  body :: MonadWidget t m => m ()
  body = do
    rec
      (_, ev) <- runEventWriterT ewbs
      dy <- foldDyn (:) ["bar"] ev
      simpleList dy dynText
    return ()

  ewbs :: MonadWidget t m => EventWriterT t Text m ()
  ewbs = do
    evClick <- button "Click Me"
    tellEvent ("foo" <$ evClick)
    return ()

..
  A tutorial on this is in pipeline by dalaing

.. _requester:

``Requester``
~~~~~~~~~~~~~

``Requester`` lets you make requests and receive responses anywhere within your widgets, and automatically collect/distribute them as necessary.

The primary API which will be used to initiate a request and get a response is::

  requesting :: Event t (Request m a) -> m (Event t (Response m a)) 

This requires defining two type constructors ``Request m`` and ``Response m``.

The API to actually collect all the requests and provide response to each request is::

  runRequesterT :: (Reflex t, Monad m)
    => RequesterT t request response m a
    -> Event t (RequesterData response)
    -> m (a, Event t (RequesterData request))

As you can see all the requests are bundled up in the ``RequesterData request``, and the responses are also provided in a similar event of type ``RequesterData response``.

The ``RequesterData`` is like a ``Map`` structure where the keys are some arbitrary values corresponding to the origin of request, and the values are the actual request data.


to provide a response one can use these APIs::

  traverseRequesterData :: forall m request response. Applicative m
    => (forall a. request a -> m (response a))
    -> RequesterData request
    -> m (RequesterData response) 

can be used to provide response to all the request by specifying a `request handler`.

But if you want access to each request separately and provide the responses in independent manner (in case you are doing XHR/ websocket requests for each request separately).

Then you can convert this into a list of key value pairs (``DSum``), provide the response to each request by using the same key with ``singletonRequesterData`` to recreate the ``RequesterData``::


  requesterDataToList :: RequesterData f -> [DSum RequesterDataKey f]

  singletonRequesterData :: RequesterDataKey a -> f a -> RequesterData f

``Workflow``
~~~~~~~~~~~~


``Reflex.Workflow`` provides a specialised API::

  newtype Workflow t m a = Workflow { unWorkflow :: m (a, Event t (Workflow t m a))}

  workflow :: forall t m a. (Reflex t, Adjustable t m, MonadFix m, MonadHold t m)
    => Workflow t m a -> m (Dynamic t a)

The working of this API can be easily explained using a DOM based widget example::
  
  -- A DOM based example of Workflow
  page1, page2, page3 :: (MonadWidget t m) => Workflow t m Text
  page1 = Workflow . el "div" $ do
    el "div" $ text "This is page 1"
    pg2 <- button "Switch to page 2"
    return ("Page 1", page2 <$ pg2)
  
  page2 = Workflow . el "div" $ do
    el "div" $ text "This is page 2"
    pg3 <- button "Switch to page 3"
    pg1 <- button "No wait, I want to go back to page 1"
    return ("Page 2", leftmost [page3 <$ pg3, page1 <$ pg1])
  
  page3 = Workflow . el "div" $ do
    el "div" $ text "You have arrived on page 3"
    pg1 <- button "Start over"
    return ("Page 3", page1 <$ pg1)
  
  main = mainWidget $ do
    r <- workflow page1
    el "div" $ do
      text "Current page is: "
      dynText r

Performance
-----------

``UniqDynamic``
~~~~~~~~~~~~~~~

``UniqDynamic`` is useful to eliminate redundant update events from a Dynamic.::

  uniqDynamic :: Reflex t => Dynamic t a -> UniqDynamic t a

  fromUniqDynamic :: (Reflex t, Eq a) => UniqDynamic t a -> Dynamic t a 

Internally, ``UniqDynamic`` uses pointer equality as a heuristic to avoid unnecessary update propagation; this is much more efficient than performing full comparisons.
However, when the UniqDynamic is converted back into a regular Dynamic, a full comparison is performed.

In order to maintain this constraint, the value inside a UniqDynamic is always evaluated to weak head normal form.

Also see the documentation of ``Reflex.Dynamic.Uniq``

Patch and Incremental
~~~~~~~~~~~~~~~~~~~~~

An ``Incremental`` is a more general form of a ``Dynamic``.
Instead of always fully replacing the value, only parts of it can be patched.
This is only needed for performance critical code via ``mergeIncremental`` to make small changes to large values.

``Reflex.Patch.*`` provides a number of data structures which have the ability to do incremental updates.

Cheap / Fast variants of APIs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Internals
---------


Frames
~~~~~~

A frame is the atomic time unit

* Frame begins with, say, a mouse click
* Mouse click event fires
* Events fmapped from that event fire
* All other events depending on those events fire
* Repeat until there are no more event firings
* Frame ends

Spider Timeline
~~~~~~~~~~~~~~~


