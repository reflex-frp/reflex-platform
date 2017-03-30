.. _guide_to_dom_creation:

A Guide to DOM Creation
=======================

The ``reflex-dom`` package provides a lot of helpful APIs to construct DOM widgets, do AJAX or any other arbitrary IO.

See `Quick Ref <https://github.com/reflex-frp/reflex-dom/blob/develop/Quickref.md>`_

.. todo:: Add links to latest haddock

..
  briefly explain these clases here?
  Reflex.Dom.WidgetHost, Reflex.Dom.Widget

DOM creation works in ``MonadWidget``. Since it is monadic, the sequence of widget APIs directly correspond to the sequence of DOM elements.

..
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


Static DOM
----------

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
-----------

To create interactive widgets you need to do changes in DOM in response to
``Event``\s or ``Dynamic`` values.

The simplest way to create a dynamic DOM is to use library APIs which take
Dynamic values as input. The following section covers these APIs.
Using these APIs you can create bigger widgets which can have multiple Dynamic
values as input.

Also you can create dynamic widgets by using static widgets, ie the widget
which don't take dynamic values as inputs (like Text -> m (Event t a)).
This can be done simply by mapping the Dynamic values over these widgets (with
mapDyn fmap??) and using ``dyn``.::


  txtInpEl <- textInput $ def {_textInputConfig_initialValue = "Button Text"}

  -- Use the library API button which accepts static Text
  -- and modify its value by using a (Dynamic t Text)
  dyn (button <$> (value txtInpEl))

Library Widgets with Dynamic input
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Change the attributes of a DOM element via Dynamic values. Use

``dynText elDynAttr elDynClass display``

``tableDynAttr``

  A widget to display a table with static columns and dynamic rows.

``tabDisplay``

  A widget to construct a tabbed view that shows only one of its child
  widgets at a time.
  Creates a header bar containing a <ul> with one <li> per child; clicking
  a <li> displays the corresponding child and hides all others.


DOM Input elements
~~~~~~~~~~~~~~~~~~

To create input form elements and use them to create ``Event`` and ``Dynamic``
values use the widgets provided by ``Reflex.Dom.Widget.Input``

See input_widgets.hs for usage of these widgets

.. todo:: Add a link to page with demo of widgets
          or may be Haddock documentation?


Dynamic widgets based on Events
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Create a widget which updates whenever ``Event`` occurs.

If you have a widget which depends on some event (like AJAX response), but you
need to display something else instead of a blank. ::

  -- ajaxResponseEv :: Event t SomeData
  -- displaySomeData :: SomeData -> m ()

  -- widgetHold :: m a -> Event t (m a) -> m (Dynamic t a)
  widgetHold (text "Loading...") (displaySomeData <$> ajaxResponseEv)


Dynamic widgets on Dynamic Collections
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

If you have a collection of Dynamic values, then it is straighforward to use
them to create a dynamic DOM. But if your collection is itself a Dynamic then
use these APIs::

  simpleList :: Dynamic t [v] -> (Dynamic t v -> m a) -> m (Dynamic t [a])
  list :: Dynamic t (Map k v) -> (Dynamic t v -> m a) -> m (Dynamic t (Map k a))


  -- * Widgets on Collections
  listWithKey
  listWithKey'
  listWithKeyShallowDiff
  listViewWithKey

  listHoldWithKey

  partitionMapBySetLT??

.. What is Workflow??

SVG
---

Use ``elDynAttrNS'`` along with SVG namespace::

  elSvgns = elDynAttrNS' (Just "http://www.w3.org/2000/svg")


Troubleshooting type-class errors
---------------------------------

There are a few common compile time errors which can occue while using the
widgets

#. If you define a widget but don't use it any where ::

    -- 't' is not used anywhere
    let t = textInput $ def

    Compile error

    • Couldn't match type ‘DomBuilderSpace m0’ with ‘GhcjsDomSpace’
        arising from a use of ‘textInput’
      The type variable ‘m0’ is ambiguous
    • In the expression: textInput $ def
      In an equation for ‘t’: t = textInput $ def


  Solution: Simply comment this code or use it.

.. http://stackoverflow.com/questions/41367144/haskell-how-to-fix-the-type-variable-ambigous-compiler-error


..
  https://www.reddit.com/r/reflexfrp/comments/3h3s72/rendering_dynamic_html_table/

  I finally figured out how to render a dynamic table. Here's a sample code:
  h1_ $ text "Fetch table"
  clickEvent <- button "Fetch records"

  let req = xhrRequest "GET" "/users/list" def
  asyncReq <- performRequestAsync (tag (constant req) clickEvent)

  resp <- holdDyn (Just []) $ fmap  decodeXhrResponse asyncReq
  h1_ $ text "The table"
  x2 <- mapDyn fromJust resp
  renderUserTable x2

  renderUserTable xsd = do
     xsTabled <- mapDyn makeTable xsd
     dyn xsTabled

  makeTable xs = do
     el "table" $ do
        el "tr" $ do
             el "th" $ text "User Name"
             el "th" $ text "Age"
             el "th" $ text "Department"
             el "th" $ text "On Hold Status"
        forM xs $ \u -> do
           el "tr" $ do
               el "td" $ text (show (userName u))
               el "td" $ text (show (userAge u))
               el "td" $ text (show (userDept u))
               el "td" $ text (userStatus u)

  As you can see i used the function dyn to create a dynamic html table. Unfortunately i could not figure out how to use other functions like
  tableDynAttr, listWithKey etc.
  Complete lack of documentation makes it hard for me to comprehend how those functions work.
  It would be great if someone posted simple examples of how to use some of the functions from Reflex.Dom.Widget modules.

