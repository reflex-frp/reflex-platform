

I used a very similar architecture with Reflex with HSnippet, and it's
delightful to work wth. Server communication was done over websockets with the
wire format being a serialized version of these data types. Adding a new
client/server or server/client message couldn't be more simple.

https://github.com/mightybyte/hsnippet/blob/master/shared/src/HSnippet/Shared/WsApi.hs

