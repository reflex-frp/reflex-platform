Reflex Dom
==========

See `Quick Ref <https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md>`_

.. Type Classes
.. ------------

.. A reference for what the type class is for

.. DomBuilder
.. ~~~~~~~~~~

Basic Widgets
-------------


Static DOM
~~~~~~~~~~

Here is a simple example of using some of the static-dom widgets::

  -- simple_dom.hs
  {-# LANGUAGE OverloadedStrings #-}

  import Reflex.Dom

  -- Code to showcase Reflex.Dom's APIs to create simple static DOM
  main = mainWidget $ do
    simple

  simple :: (MonadWidget t m) => m ()
  simple = do
    el "div" $
      -- Specify attributes in a (Map Text Text)
      elAttr "span" ("style" =: "color:blue") $
        text "Text inside span"

    -- Use CSS style center-align and red-text
    -- using these specialised APIs
    divClass "center-align" $
      elClass "span" "red-text" $
        text "Div with class center-align and red text"

    el "dl" $ do
      dtdd "dt dd tags" $
        text "Here goes the description"

      dtdd "Reflex" $ do
        text "Haskell + awesome FRP!"
        -- Should we have a 'textbr' API with line break at the end?
        el "br" $ blank -- Add line break, blank == return ()
        -- A simple URL link
        elAttr "a" ("href" =: "http://reflexfrp.org") (text "Reflex-FRP")

Dynamic DOM
~~~~~~~~~~~

To create interactive widgets you need to do changes in DOM in response to
``Event``\s or ``Dynamic`` values.

The simplest way to create a dynamic DOM is to use library APIs which take
Dynamic values as input. The following section covers these APIs.
Using these APIs you can create bigger widgets which can have multiple Dynamic
values as input.::

  -- Show simple text
  dynText $ someDynTextValue -- :: Dynamic t Text

  display $ someDynValueWithShowInstance

  -- The value of input element can be modified from an external Dynamic t text
  txtInpEl <- textInput $ def
    & textInputConfig_initialValue .~ "Button Text"
    & textInputConfig_setValue .~ someOtherDynText


Also you can create dynamic widgets by using static widgets, ie the widget
which don't take dynamic values as inputs (eg. ``button :: Text -> m (Event t a)``).
This can be done simply by mapping the Dynamic values over these widgets and using ``dyn``.::

  -- Use the library API button which accepts static Text
  -- and modify its value by using a (Dynamic t Text)
  dyn (button <$> (value txtInpEl))

The library provides a number of standard widgets which accept ``Dynamic`` values as input


``elDynAttr elDynClass``

  Change the attributes of a DOM element via Dynamic values.

``tableDynAttr``

  A widget to display a table with static columns and dynamic rows.::

``tabDisplay``

  A widget to construct a tabbed view that shows only one of its child
  widgets at a time.
  Creates a header bar containing a ``<ul>`` with one ``<li>`` per child; clicking
  a ``<li>`` displays the corresponding child and hides all others.

.. _dom_input_elements:

DOM Input elements
~~~~~~~~~~~~~~~~~~

To create input form elements and use them to create ``Event`` and ``Dynamic``
values use the widgets provided by ``Reflex.Dom.Widget.Input``

See `input_widgets.hs <https://github.com/reflex-frp/reflex-frp.org/blob/master/code-snippets/input_widgets.hs>`_ for usage of these widgets

The various input elements usually contain these two values::

  *_input :: Event t a
  *_value :: Dynamic t a

The ``_input`` event will only fires when *user* modifies contents of the input field.
But if you are modifying the value of the input field using reflex ``Event`` and you want to capture even these changes, then use ``updated value``.

.. tip:: When using the ``*_input`` Events you might have to use ``debounce``. See :ref:`debounce`

.. _dom_events:

DOM Events
~~~~~~~~~~

  ``domEvent`` API can be used to create ``Event`` on DOM elements::

    (e,_) <- el' "span" $ text "Click Here"

    clickEv :: Event t ()
    clickEv <- domEvent Click e

  For a complete list of events accepted by ``domEvent`` see ``EventName`` in
  `Reflex.Dom.Builder.Class.Events <https://github.com/reflex-frp/reflex-dom/blob/develop/reflex-dom-core/src/Reflex/Dom/Builder/Class/Events.hs>`_

Dynamic widgets based on Events
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Create a widget which updates whenever ``Event`` occurs.

If you have a widget which depends on some event (like server response), but you
need to display something else instead of a blank. ::

  -- ajaxResponseEv :: Event t SomeData
  -- displaySomeData :: SomeData -> m ()

  -- widgetHold :: m a -> Event t (m a) -> m (Dynamic t a)
  widgetHold (text "Loading...") (displaySomeData <$> ajaxResponseEv)

Every time the ``widgetHold`` event fires, it removes the old DOM fragment and builds a new one in-place


Miscellaneous
-------------

Resize Detector
~~~~~~~~~~~~~~~
::

  -- Reflex.Dom.Widget.Resize
  resizeDetector :: (_) => m a -> m (Event t (), a)

This is useful to respond to changes in size of a widget.

.. Does this respond to viewport size changes?

Host / URL / Location
~~~~~~~~~~~~~~~~~~~~~

``Reflex.Dom.Location`` contains utility functions for obtaining the host, URL, protocol, etc.

Client side routes
~~~~~~~~~~~~~~~~~~

`Reflex.Dom.Contrib.Router <https://github.com/reflex-frp/reflex-dom-contrib/blob/master/src/Reflex/Dom/Contrib/Router.hs>`_ provides APIs to manipulate and track the URL.

Also checkout https://github.com/3noch/reflex-dom-nested-routing

SVG
~~~

To embed an SVG element use ``elDynAttrNS'`` along with SVG namespace::

  elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")

Using `canvas` element with reflex is generally not a good idea, as it is based on an imperative style of coding (vs the declarative style of svg).

Also checkout https://github.com/qfpl/reflex-dom-svg

.. _xhr_websocket:

XHR/ websocket
--------------

For usage on XHR / AJAX requests please see the haddock documentation of module ``Reflex.Dom.Xhr``, it contains example usage of the APIs.

Websocket
~~~~~~~~~

Use ``webSocket`` API from the ``Reflex.Dom.WebSocket`` module.::

 webSocket
   :: Text -- url, like "ws://localhost:3000/myWebSocketHandler"
           -- use wss for SSL connections
   -> WebSocketConfig t a -> m (WebSocket t)

 data WebSocketConfig t a
   = WebSocketConfig {_webSocketConfig_send :: Event t [a],
                      _webSocketConfig_close :: Event t (Word, Text),
                      _webSocketConfig_reconnect :: Bool}

  type WebSocket t =
    RawWebSocket t ByteString

  data RawWebSocket t a
    = RawWebSocket {_webSocket_recv :: Event t a,
                    _webSocket_open :: Event t (),
                    _webSocket_error :: Event t (),
                    _webSocket_close :: Event t (Bool, Text)}

To send data over WebSocket pass an event to ``_webSocketConfig_send`` of type
``Event t [a]`` where ``a`` is either ``Text`` or ``ByteString``.

The return value from WebSocket is available from ``_webSocket_recv :: Event t ByteString``

Here ``_webSocketConfig_close`` is an ``Event`` which can close the WebSocket connection
from client side. And ``_webSocket_close`` is the response from server when the
connection closes.

Manually closing a websocket that is configured to reconnect will cause it to reconnect.
If you want to be able to close it permanently you need to set ``_webSocketConfig_reconnect = False``.

See `reflex-examples <https://github.com/reflex-frp/reflex-examples/blob/master/websocket-echo/src/Main.hs>`_ for an echo example.


Integration with Backend
~~~~~~~~~~~~~~~~~~~~~~~~

One of the big strength of ``reflex-dom`` is that a common code base can be shared between backend and frontend.

Quoting `mightybyte <https://github.com/mightybyte>`_ again.
See `hsnippet.com source code here <https://github.com/mightybyte/hsnippet/blob/master/shared/src/HSnippet/Shared/WsApi.hs>`_

  I used a very similar architecture with Reflex with HSnippet, and it's
  delightful to work with. Server communication was done over websockets with the
  wire format being a serialized version of these data types. Adding a new
  client/server or server/client message couldn't be more simple.

The simplest form of integration with backend is to define the message data in the ``common`` package, along with its serialisation functions (eg ``deriving instance`` of ``ToJSON`` and ``FromJSON``).

.. _reflex_websocket_interface:

`reflex-websocket-interface`
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Going a few steps further in this integration is the library `reflex-websocket-interface <https://github.com/dfordivam/reflex-websocket-interface>`_

* It provides a reflex side API like this::

    getResponse :: (_) => Event t request -> m (Event t response)

  This takes care of encoding and decoding of the messages (using ``aeson``), do all the routing of Event behind the scenes, and provide the response at the point where request was initiated.

  This architecture of handling the request and its response at the same place in widget code is essential for self-contained widgets.
  It also helps greatly simplify the coding, especially when there are more than one instance of a widget, and they all use single websocket to communicate.

  Internally this uses :ref:`requester`.

* It ensures the server has code to handle all the request types.

* It further ensures that the type of response for a request is consistent between frontend and backend.

`servent-reflex`
^^^^^^^^^^^^^^^^

https://github.com/imalsogreg/servant-reflex

  `servant-reflex` lets you share your `servant` APIs with the frontend. See the readme for more details.



Performance
-----------

Static  / Server side rendering
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ``renderStatic`` API can be used to render the DOM parts of the application to plain HTML.
This way the server can serve the generated HTML, so that the page `opens` instantly for the user.::

  renderStatic :: StaticWidget x a -> IO (a, ByteString)

To create widget which support static rendering, the ``prerender`` API will be required internally to separate the static code from the Immediate DomBuilder one.

See `StaticBuilder.hs <https://github.com/reflex-frp/reflex-frp.org/blob/master/code-snippets/StaticBuilder.hs>`_ for an example usage::

  prerender :: forall js m a. Prerender js m =>
    m a -> (PrerenderClientConstraint js m => m a) -> m a

Here the first widget supports Static rendering, and the second one has the actual JSM functionality.

  
lazy
~~~~

``Reflex.Dom.Widget.Lazy`` contains widgets for creating long lists. These are scrollable element and only renders child row elements near the current scroll position.
