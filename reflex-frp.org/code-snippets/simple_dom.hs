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

