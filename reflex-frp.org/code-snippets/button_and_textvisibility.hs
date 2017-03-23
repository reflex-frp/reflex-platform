{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Reflex.Dom

-- This code demonstrates use of an event to create dynamic values
-- Simple flow of an event from one widget to another.
main = mainWidget $ do

  -- This widget is defined in library, it creates a simple button
  evClick <- button "Click Me!"

  -- Toggle visibility, creates a dynamic bool isVisible
  isVisible <- foldDyn (\_ b -> not b) False evClick

  textWithDynamicVisibility isVisible

  return ()

-- This widget takes the input value of visibility
-- and creates a view based on that
textWithDynamicVisibility isVisible = do
  let dynAttr = ffor isVisible
                 (\case
                   True -> ("style" =: "")
                   False -> ("style" =: "display: none;"))

  elDynAttr "div" dynAttr $
    text "Click the button again to make me disappear!"
