{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

import Reflex.Dom

-- This code demonstrates use of an event to create dynamic values
-- Simple flow of an event from one widget to another.
main = mainWidget $ do

  -- View Widget to Generate Events
  -- button widget is defined in library, it creates a simple button
  evClick <- button "Click Me!"

  -- Controller
  -- Handle events and create a 'Dynamic t Bool' value
  -- This toggles the visibility when the button is pressed
  isVisible <- foldDyn (\_ b -> not b) False evClick

  -- View
  -- This is a simple widget that takes a 'Dynamic t Bool' as input
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
