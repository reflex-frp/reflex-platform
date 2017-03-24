

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
