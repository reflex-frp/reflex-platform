A Guide to DOM Creation
=======================

DOM creation works in () monand. Since it is monadic, the sequence of widget APIs directly correspond to the sequence of DOM elements.

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

simple_dom.hs ``text button el link divClass dtdd blank``

Dynamic DOM
-----------

The simplest way to create a dynamic DOM is to use library APIs which take
Dynamic values as input. The following section covers these APIs.
Using these APIs you can create bigger widgets which can have multiple Dynamic
values as input.

Also you can create dynamic widgets by using static widgets, ie the widget 
which don't take dynamic values as inputs (like String -> m (Event t a)).
This can be done simply by mapping the Dynamic values over these widgets (with
mapDyn fmap??) and using ``dyn``.

When you map the Dynamic values over the widgets which take just the 

Library Widgets with Dynamic input
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


Change the attributes of a DOM element via Dynamic values. Use 
``dynText elDynAttr elDynClass``
``tableDynAttr``
  A widget to display a table with static columns and dynamic
rows.

``display``
``tabDisplay``
  A widget to construct a tabbed view that shows only one of its child
  widgets at a time.
  Creates a header bar containing a <ul> with one <li> per child; clicking
  a <li> displays
   the corresponding child and hides all others.


DOM Input elements
~~~~~~~~~~~~~~~~~~

See input_widgets.hs



Dynamic widgets based on Events
~~~~~~~~~~~~~~~

Create a widget which updates whenever en Event occurs.

dyn

If you have a widget which depends on some event (like AJAX response), but you
need to display something else instead of a blank. ::

  -- widgetHold :: m a -> Event t (m a) -> m (Dynamic t a)
  
  widgetHold (text "Loading...") widgetDependentOnSomeEvent


Dynamic widgets on Dynamic Collections
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

Troubleshooting type-class errors
---------------------------------

.. http://stackoverflow.com/questions/41367144/haskell-how-to-fix-the-type-variable-ambigous-compiler-error



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

