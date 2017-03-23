Reflex-Dom Quick(ish) Reference
===============================

Typeclasses
-----------

Function signatures have been simplified. The 't' type parameter and
many typeclass constraints are removed, and annotations are added
regarding the monad of the result type (See the Reflex Quickstart for
more discussion). Annotations:

.. code:: haskell

    [ ]   -- Pure function
    [W]   -- Function runs in any monad supporting MonadWidget
    [I]   -- Function runs in IO

Functions from Reflex that run in MonadSample or MonadHold context ([S]
or [H]) will also run in [W].

Creating widgets (DOM elements hooked into the FRP system)
----------------------------------------------------------

Basic widgets
~~~~~~~~~~~~~

These functions generally take an element tag (such as "div", "h1", etc)
and a child widget, and produce a widget of the given type containing
the child. Some of these functions return data of type ``El``, which is
a handle to the created DOM element along with a set of potential Events
that might occur on that element.

Widgets may return any type (this is 'a' in many of the functions
below). Often this will be an Event carrying user interactions with the
widget.

.. code:: haskell

    -- Simplest form.  Create a widget of given type containing the given child.
    -- Return whatever the child returns.
    [W]   el         :: Text ->                                  m a -> m a

    -- This version returns the 'El' as well.
    [W]   el'        :: Text ->                                  m a -> m (El, a)

    -- These two additionally apply attributes to the element, such as ("class" =: "blah")
    [W]   elAttr     :: Text ->            Map Text Text ->      m a -> m a
    [W]   elAttr'    :: Text ->            Map Text Text ->      m a -> m (El, a)

    -- As above, but now the attribute map is Dynamic
    [W]   elDynAttr  :: Text ->   Dynamic (Map Text Text) ->     m a -> m a
    [W]   elDynAttr' :: Text ->   Dynamic (Map Text Text) ->     m a -> m (El, a)

    -- Shortcut for elAttr when you only want to set the "class" attribute.
    [W]   elClass    :: Text ->                       Text ->    m a -> m a

    -- Even shorter-cut for above when element type is "div".  Create a div of given class.
    [W]   divClass   ::                               Text ->    m a -> m a

    -- Create a widget of given type with arbitrary, Dymamic HTML inside.
    [W]   elDynHtml'     :: Text ->                        Dynamic Text ->   m El
    [W]   elDynHtmlAttr' :: Text ->   Map Text Text ->     Dynamic Text ->   m El

    -- Create a static text element
    [W]   text    ::              Text ->   m ()

    -- Create a dynamic text element
    [W]   dynText ::      Dynamic Text ->   m ()
    [W]   display :: Show a => Dynamic a -> m ()

    -- Create a "button" element with given label, return onClick Event
    [W]   button :: Text -> m (Event ())

    -- Empty widget
    [W]   blank :: m ()

Dynamic widgets
~~~~~~~~~~~~~~~

In the Dynamic cases so far, the *content* of a widget is dynamic but
the *definition* of the widget is static. The functions below enable the
definition and/or structure of the widget itself to change over time.

Note the "list" functions do not imply particular HTML tags (ul, li,
etc), though the widgets they create can have those tags if you
construct them appropriately.

.. code:: haskell

    -- Create a dynamically-redefined widget from a Dynamic of widget actions.
    [W]   dyn        ::        Dynamic (m a) -> m (Event a)

    -- Same as dyn, but takes initial value and an update Event instead of a Dynamic.
    [W]   widgetHold :: m a ->   Event (m a) -> m (Dynamic a)

    -- Turn a Dynamic key/value map into a set of dynamically-changing widgets.
    [W]   listWithKey :: Ord k =>
              Dynamic (Map k v) -> (k -> Dynamic v -> m        a ) -> m (Dynamic (Map k a))

    -- Same as above where the widget constructor doesn't care about the key.
    [W]   list        :: Ord k =>
              Dynamic (Map k v) -> (     Dynamic v -> m        a ) -> m (Dynamic (Map k a))

    -- Even simpler version where there are no keys and we just use a list.
    [W]   simpleList  ::
              Dynamic       [v] -> (     Dynamic v -> m        a ) -> m (Dynamic       [a])

    -- Like listWithKey specialized for widgets returning (Event a).
    [W]   listViewWithKey :: Ord k =>
              Dynamic (Map k v) -> (k -> Dynamic v -> m (Event a)) -> m (Event   (Map k a))

    -- Create a dynamically-changing set of widgets, one of which is selected at any time.
    [W]   selectViewListWithKey_ :: Ord k => Dynamic k ->
              Dynamic (Map k v) -> (k -> Dynamic v -> Dynamic Bool -> m (Event a)) -> m (Event k)

    -- Same as listWithKey, but takes initial values and an updates Event instead of a Dynamic.
    [W]   listWithKey' :: Ord k =>
              Map k v -> Event (Map k (Maybe v)) -> (k -> v -> Event v -> m a) -> m (Dynamic (Map k a))

Utility widgets
~~~~~~~~~~~~~~~

These are useful widgets that are implemented (or could be implemented)
in terms of the low-level widgets above.

Some of these widget builders take a configuration record and return a
record containing Events or other useful data associated with the
created widget (similar to 'El'). The configuration records have default
values, so you can just supply 'def'. See Reflex/Dom/Widget/Input.hs for
record fields (Lenses are provided).

.. code:: haskell

    -- Text input.
    [W]   textInput :: TextInputConfig -> m TextInput
    [ ]   textInputGetEnter :: TextInput -> Event ()
    [W]   textArea :: TextAreaConfig -> m TextArea

    -- Range input (slider with float values).
    [W]   rangeInput :: RangeInputConfig -> m RangeInput

    -- Checkbox.  The Bool supplies the initial state.
    [W]   checkbox :: Bool -> CheckboxConfig -> m Checkbox

    -- Dropdown with Dynamic options.  First argument is initial state.
    [W]   dropdown :: (Ord k, Show k, Read k) =>
              k -> Dynamic (Map k Text) -> DropdownConfig k -> m (Dropdown k)

    -- Table with static columns and dynamic rows.
    [W]   tableDynAttr :: ...        -- See Reflex.Dom.Widget.Basic

    -- Tabbed view that shows only one of its child widgets at a time.
    [W]   tabDisplay :: (Show k, Ord k) => Text -> Text -> Map k (Text, m ()) -> m ()

    -- Widget to efficiently display long scrolling lists.
    [W]   virtualListWithSelection :: ...        -- See Reflex.Dom.Widget.Lazy

Connecting to the real world (I/O)
----------------------------------

Connecting to DOM events
~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: haskell

    -- Extract the specified Event from an 'El'.  See Reflex.Dom.Widget.Basic
    [ ]   domEvent :: EventName en -> El -> Event (EventResultType en)

Performing arbitrary I/O in response to Events
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code:: haskell

    -- Run side-effecting actions in Event when it occurs; returned Event contains
    -- results. Side effects run in (WidgetHost m) monad, which includes [S] and [H]
    -- and can also do I/O via liftIO
    [W]   performEvent      :: Event (                WidgetHost m  a) -> m (Event a)

    -- Just run side-effects; no return Event
    [W]   performEvent_     :: Event (                WidgetHost m ()) -> m ()

    -- Actions run asynchronously; actions are given a callback to send return values
    [W]   performEventAsync :: Event ((a -> IO ()) -> WidgetHost m ()) -> m (Event a)

XMLHttpRequest
~~~~~~~~~~~~~~

Convenience functions for XMLHttpRequest. see Reflex.Dom.Xhr

.. code:: haskell

    -- Given method, URL, and config record (with default instance), construct a request.
    [ ]   xhrRequest :: Text -> Text -> XhrRequestConfig a -> XhrRequest a

    -- Given Event of requests, issue them and produce Event of responses.
    [W]   performRequestAsync :: Event (XhrRequest a) -> m (Event XhrResponse)

    -- Issue a collection of requests, wait for them ALL to complete, return collected results.
    [W]   performRequestsAsync :: Traversable f => Event (f (XhrRequest a)) -> m (Event (f XhrResponse))

    -- Convenience function to decode JSON-encoded responses.
    [ ]   decodeXhrResponse :: FromJSON a => XhrResponse -> Maybe a

    -- Simplified interface to "GET" URLs and return decoded results.
    [W]   getAndDecode :: FromJSON a => Event Text -> m (Event (Maybe a))

Time
~~~~

.. code:: haskell

    -- Create Event at given interval with given basis time.
    [W]   tickLossy :: NominalDiffTime -> UTCTime -> m (Event t TickInfo)

    -- Delay an Event's occurrences by a given amount in seconds.
    [W]   delay :: NominalDiffTime -> Event t a -> m (Event t a)

Startup
-------

.. code:: haskell

    -- Reflex-Dom entry point.  Takes a monadic widget-building action of lengthy
    -- type and turns it into an IO action.
    [I]   mainWidget ::
              Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> IO ()
    [I]   mainWidgetWithHead ::
              Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () ->
              Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> IO ()
    [I]   mainWidgetWithCss ::
              ByteString ->
              Widget Spider (Gui Spider (WithWebView SpiderHost) (HostFrame Spider)) () -> IO ()

    -- One-shot Event that is triggered once all initial widgets are built
    [W]   getPostBuild :: m (Event ())
