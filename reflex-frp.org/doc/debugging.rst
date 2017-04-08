Debugging Reflex Application
----------------------------

If your application compiles but does not behave as expected, or you just want
to get a better understanding of the actual flow of ``Event``\s then use the
trace APIs.

Trace APIs
~~~~~~~~~~

Note that these APIs will only print if their output ``Event`` or ``Dynamic`` is actually being used. So you need to insert these trace APIs in the event propagation graph::

  traceEvent     :: Show a => String -> Event a -> Event a
  traceEventWith ::    (a -> String) -> Event a -> Event a

  traceDyn       :: (Show a) => String -> Dynamic t a -> Dynamic t a
  traceDynWith   :: (a -> String) -> Dynamic t a -> Dynamic t a

The output of these APIs will be printed on the browser console window if you
have a ghcjs app. If the app is compiled with ghc then the trace will be printed
directly on the terminal window.

