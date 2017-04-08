{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}

import Reflex.Dom

-- This code demonstrates use of an event to create dynamic values
-- Circular flow of Event/Dynamic using Recursive-do syntax
main = mainWidget $ do

  rec
    -- Controller
    -- Handle events and create a 'Dynamic t Bool' value
    -- This toggles the visibility when the button is pressed
    isVisible <- foldDyn (\_ b -> not b) False evClick

    -- View
    -- This widget creates the button and its click event,
    -- The click event is propagated to the controller
    evClick <- textWithDynamicVisibility isVisible

  return ()

-- This widget takes the input value of visibility
-- and creates a view based on that
textWithDynamicVisibility isVisible = do
  -- View Widget to Generate Events
  -- button widget is defined in library, it creates a simple button
  evClick <- button "Click Me!"

  let dynAttr = ffor isVisible
                 (\case
                   True -> ("style" =: "")
                   False -> ("style" =: "display: none;"))

  elDynAttr "div" dynAttr $
    text "Click the button again to make me disappear!"

  return evClick
