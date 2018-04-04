Application Development with Reflex-DOM
=======================================

..
   Dev Environment
   ---------------

   * reflex-plaform

   * GHCi,

   * hoogle
   See :ref:`haddock_and_hoogle`

   *

Debugging
---------

Functionality
~~~~~~~~~~~~~

In addition to the normal ``Debug.Trace`` APIs, the following can be used for debugging.

The output of these APIs will be in the browser console when compiled with ``ghcjs``.
For ``jsaddle-warp`` and ``webkit`` based apps the output will be on the terminal.::


  traceEvent :: (Reflex t, Show a) => String -> Event t a -> Event t a
  traceEventWith :: Reflex t => (a -> String) -> Event t a -> Event t a 

Moreover the `reflex-dom-contrib <https://github.com/reflex-frp/reflex-dom-contrib/blob/master/src/Reflex/Dom/Contrib/Utils.hs>`_ package contains a bunch of utility functions.
One can just copy-paste these functions, ie use them without dependency on the package.::

  -- Reflex.Dom.Contrib.Utils
  -- pops up a javascript alert dialog box
  alertEvent :: (_) => (a -> String) -> Event t a -> m ()

  -- pops up a javascript confirmation dialog box
  confirmEvent :: (_) => (a -> String) -> Event t a -> m (Event t a)

  -- | Prints a string when an event fires.  This differs slightly from
  -- traceEvent because it will print even if the event is otherwise unused.
  putDebugLnE :: MonadWidget t m => Event t a -> (a -> String) -> m ()

.. _hang_stack_overflow:

Hang / Stack Overflow
~~~~~~~~~~~~~~~~~~~~~

In general its possible to create a loop by mistake with this kind of code in a "pure" haskell.::

  let
    f v = ... (f v)

But thanks to ``MonadFix`` (``RecursiveDo``) this is a very common problem, even in a "monadic" code.

Basically for doing anything useful one has to introduce cycle in the event propagation graph.
And often this can lead to either a loop or a deadlock.

To fix this

* Breaking down a big ``rec`` block into nested ``rec`` blocks or a series of ``rec`` blocks.
  Moving the code in a separate functions can also help simplify the ``rec`` block.

  Also see: using :ref:`new_trigger_event` to break down a big ``rec`` block.

* Introduce ``delay``.

  For example in this kind of loop in event propagation there is a need of ``delay``::

    rec
      let someEv = updated d
          ev = someFun <$> someEv

      d <- holdDyn 0 ev

* Avoid using ``switchPromptlyDyn`` / ``tagPromptlyDyn``, instead use ``switch . current`` / ``tag . current``

  Many times what one really need is the previous value of a ``Dynamic`` to create a cyclic event propagation.

* Use ``widgetHold`` against ``dyn``

  Separating an initial value from an update event means that the function using them doesn't have to call ``sample`` on a ``Dynamic``,
  which can be unsafe when you don't know whether the MonadFix knot has been tied.

  Using ``widgetHold`` ensures that the user doesn't accidentally give an untied Dynamic.

For more details checkout the articles on :ref:`monad_fix`


Compilation Errors
~~~~~~~~~~~~~~~~~~

These are a few common compile time errors which can occue while using the
widgets

* If you define a widget but don't use it any where ::

    -- 't' is not used anywhere
    let t = textInput $ def

    Compile error

    • Couldn't match type ‘DomBuilderSpace m0’ with ‘GhcjsDomSpace’
        arising from a use of ‘textInput’
      The type variable ‘m0’ is ambiguous
    • In the expression: textInput $ def
      In an equation for ‘t’: t = textInput $ def


  Solution: Simply comment this code or use it.


* In a ``rec`` block if use a "pure" API in a "monadic" context, then you can get weird type errors::

    -- This will lead to type-checker assume the monad to be Dynamic
    ev <- switchPromptlyDyn dynEv

  The biggest problem with such errors is that the line numbers are not correct, so it can take a while to figure out the source of error

  One possible solution is to explicitly specify the type of functions and expression in the ``let`` and ``do`` block inside of ``rec``::

    --  This is required to specify the types
    --  {-# LANGUAGE ScopedTypeVariables #-}

    -- This can be useful to specify types partially, just to help figure out source of error
    --  {-# LANGUAGE PartialTypeSignatures #-}

    -- Specify an explicit forall
    myWidget :: forall t m k . (MonadWidget t m, Ord k)
      => Map k Text -> m ()
    myWidget mapInput = do
      ..

      rec
        let
          eTabClicks :: Event t k = leftmost tabClicksList

        d :: Dynamic t k <- do
          someCodeThatIsSupposedToReturnDynamicK

.. _ffi:

Web APIs and FFI
----------------

* For working with DOM and using Web APIs the ``ghcjs-dom`` package should suffice.

  It provides APIs like ``getElementById``, ``getBoundingRect`` to work with DOM, and many other Web APIs related to geolocation, media management, web audio, etc.

  To use the DOM related APIs for ``reflex-dom`` created elements, extract the `raw` element from the `reflex element` ::

    import qualified GHCJS.DOM.Types as DOM
    import qualified GHCJS.DOM.DOMRectReadOnly as DOM
    import qualified GHCJS.DOM.Element as DOM

      (_,e) <- el' "div" $ text "Hello"
      
      let getCoords e = DOM.liftJSM $ do
            rect <- DOM.getBoundingClientRect (_raw_element e)
            y <- DOM.getY rect
            h <- DOM.getHeight rect
            return (y,h)

      performEvent (getCoords e <$ ev)

* But when using external .js files, one has to do arbitrary JS code execution.

  For doing this ``jsaddle`` package is preferred as it provides a type-safe way to execute the JS code.

  See `documentation <https://hackage.haskell.org/package/jsaddle-0.9.4.0/docs/Language-Javascript-JSaddle-Object.html>`_ of ``Language.Javascript.JSaddle.Object`` for examples

  See :ref:`dom_ui_libs` for example usage.

* It is also possible to do arbitrary JS code block execution using ``eval`` API from ``Language.Javascript.JSaddle.Evaluate``. ::

    eval :: (ToJSString script) => script -> JSM JSVal

    liftJSM $ eval "console.log('Hello World')"

* JSFFI functions

  This will only work with ``ghcjs``::

    import GHCJS.Types (JSVal)
    
    foreign import javascript unsafe
      "try { $r = $1 / $2; } catch (e) { $r = "error"; }"
      divide :: Double -> Double -> JSVal

  See https://github.com/ghcjs/ghcjs/blob/master/doc/foreign-function-interface.md

If you access a DOM element via FFI which has just been created, then it might result in an error. This is because the ``getPostBuild`` is fired before the DOM has been put in the Window. To solve this add ``delay``::

  evPB <- delay 0.2 =<< getPostBuild
  elAttr ("id" =: "some-uniq-id") $ text "example"
  performEvent_ (someFFI <$ evPB)

Capturing DOM events with FFI
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Many of the Web APIs work on a `callback` mechanism, where a user supplied function will be called. Many of these APIs in JS code start with `on` prefix.

Example JS code for creating an AudioNode to handle audio data,
`Source <https://developer.mozilla.org/en-US/docs/Web/API/ScriptProcessorNode>`_ ::

  // Give the node a function to process audio events
  scriptNode.onaudioprocess = function(audioProcessingEvent) {
    // The input buffer is the song we loaded earlier
    var inputBuffer = audioProcessingEvent.inputBuffer;
    ..
    }

Similar callback can be created by using the ``on`` API from ``GHCJS.DOM.EventM`` ::

  -- here audioProcess is the equivalent "tag" for JS onaudioprocess

    myNode :: ScriptProcessorNode
    
    liftJSM $ on myNode audioProcess myAudioProcessHandler

  myAudioProcessHandler :: EventM ScriptProcessorNode AudioProcessingEvent ()
  myAudioProcessHandler = do
    -- aEv :: AudioProcessingEvent
    aEv <- ask
    buf <- getInputBuffer aEv
    ..

Exception Handling
~~~~~~~~~~~~~~~~~~

..   Add proper exception handling

     jsFn <- eval "(function (cb) { try{cb();alert(1)}catch(e){console.warn(e);alert(2)} })"
     
         (funJsv) <- function $ \ _ _ args -> do
           io $ print 13333
     call jsFn jsFn [funJsv]


  
Integrating CSS and embed in HTML
------------------------------------

``reflex-dom`` has the following entry points for embedding CSS and a head widget::

  mainWidget :: (forall x. Widget x ()) -> IO ()

  mainWidgetWithHead :: (forall x. Widget x ()) -> (forall x. Widget x ()) -> IO ()

  -- Share data between head and body widgets
  mainWidgetWithHead' :: (a -> Widget () b, b -> Widget () a) -> IO ()
  
  -- import Data.FileEmbed -- from file-embed package
  -- This required TemplateHaskell
  -- customCss :: ByteString
  -- customCss = $(embedFile "src/custom.css")
  mainWidgetWithCss :: ByteString -> (forall x. Widget x ()) -> IO ()
  
  mainWidgetInElementById :: Text -> (forall x. Widget x ()) -> IO ()

``reflex-dom-core`` provides equivalent functions in ``Reflex.Dom.Main`` for use with ``jsaddle-warp``



Deploying
---------

Nix based server
~~~~~~~~~~~~~~~~

If your server has ``nix`` installed then the steps to deploy are quite simple.

If you are using :ref:`reflex_project_skeleton` or following `project-development.md <https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md>`_
follow the instructions and create the ``nix-build`` outputs of your backend and frontend projects.

* Frontend

  For ``ghcjs`` based projects the ``frontend-result`` will contain the *.js files which you can simply copy to the desired location on server.

  For information on the use of closure compiler to reduce the size of ``all.js`` see https://github.com/ghcjs/ghcjs/wiki/Deployment

* Backend

  For ``backend-result`` once you have the build products ready, copy them to server using::

    # or nix copy, if using nix 2.0
    $ nix-copy-closure --to someuser@server.org backend-result

  You will have to configure the server's nix configuration and add `someuser` to trusted users::
  
  For NixOS add this to ``/etc/nixos/configuration.nix``::
  
    nix.trustedUsers = [ "someuser" ];
  
  For non NixOS, add this to ``/etc/nix/nix.conf``::
  
    trusted-users = someuser
  
  On the server then use the same nix-path

Miscellaneous
-------------

Rendering image from ``ByteString``
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have the encoded image data as ``ByteString`` then you can render the image in browser using the `img` tag in combination with `createObjectURL`.

This API will create a URL which can be specified in the ``img`` tag's ``src`` attribute::

  foreign import javascript unsafe "window['URL']['createObjectURL']($1)" createObjectURL_ :: Blob.Blob -> IO JS.JSVal

  createObjectURL :: ByteString -> IO Text
  createObjectURL bs = do
    let opt :: Maybe JS.BlobPropertyBag
        opt = Nothing
    -- bsToArrayBuffer :: MonadJSM m => ByteString -> m ArrayBuffer
    ba <- bsToArrayBuffer bs
    b <- Blob.newBlob [ba] opt
    url <- createObjectURL_ b
    return $ T.pack $ JS.fromJSString $ JS.pFromJSVal url

Android / iOS Apps
------------------

On a mobile device the speed of a ``ghcjs`` based browser app can be extremely bad. But the good news is that with little effort the ``reflex-dom`` apps can be compiled to run as a native mobile app. The performance of these apps can be considerably faster (of the order of 10x) as the haskell runtime runs on the actual processor.

See the README of :ref:`reflex_project_skeleton` or `project-development.md <https://github.com/reflex-frp/reflex-platform/blob/develop/docs/project-development.md#building-mobile-apps>`_
for instructions of creating an android or iOS app from your frontend project.

Also see: https://github.com/gonimo/gonimo

.. note:: Cross-compiling currently doesn't support Template Haskell, so replace all the ``makeLenses``, etc code with generated splices

.. todo:: Expand this section
