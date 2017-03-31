.. _guide_to_ajax:

A Guide to AJAX
---------------


XHR
~~~

WebSockets
~~~~~~~~~~

Use ``webSocket :: Text -> WebSocketConfig t a -> m (WebSocket t)`` API from the
``Reflex.Dom.WebSocket`` module.::

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

See `reflex-examples <https://github.com/reflex-frp/reflex-examples/blob/master/websocket-echo/src/Main.hs>`_ for an echo example.


..
  I used a very similar architecture with Reflex with HSnippet, and it's
  delightful to work wth. Server communication was done over websockets with the
  wire format being a serialized version of these data types. Adding a new
  client/server or server/client message couldn't be more simple.

  https://github.com/mightybyte/hsnippet/blob/master/shared/src/HSnippet/Shared/WsApi.hs

