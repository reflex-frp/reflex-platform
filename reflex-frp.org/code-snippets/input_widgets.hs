{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom
import Data.Text as T
import Data.Monoid

-- Code to showcase Reflex.Dom's APIs to create simple dynamic DOM
-- Using Input Widgets
main = mainWidget $ do
  demoWidget
  
demoWidget :: (MonadWidget t m) => m ()
demoWidget = do

  let 
    -- Useful if a library widget requires Dynamic attributes
    -- but you need to provide a constant
    constAttrs = constDyn ("style" =: "font-size: 160%")

  elDynAttr "div" constAttrs $ do

    -- Text Input
    txtInpEl <- textInput $ def

    -- value retrieves the input fields Dynamic value
    dynText (value txtInpEl)

  el "div" $ do
    -- Range Input
    resetEv <- button "Reset Range Input"
    -- Range Input with Initial value of 5
    rangeInpEl <- rangeInput $ def {_rangeInputConfig_initialValue = 5.0
      , _rangeInputConfig_setValue = const 5.0 <$> resetEv}
    
    display (_rangeInput_value rangeInpEl)

  el "div" $ do
    -- Text Area Input
    clearTxtAreaLink <- link "Clear Text Area"

    txtAreaInpEl <- textArea $ def
      {_textAreaConfig_setValue = const ""
          <$> (_link_clicked clearTxtAreaLink)}

    text "Text Length: "
    display (T.length <$> value txtAreaInpEl)

  el "div" $ do
    -- Check Box Input
    checkBoxInpEl <- checkbox False def

    display (value checkBoxInpEl)

  el "div" $ do
    -- File Input
    fileInput def

  el "div" $ do
    -- Drop Down Selection Input
    let dropDownElements = constDyn $ (1 =: "First") <> (2 =: "Second")
                            <> ((3 :: Int) =: "Third")

    dropDownInpEl <- dropdown 2 dropDownElements def

    text "Value selected:"
    display (value dropDownInpEl)

  -- Radio Button Input ?? Not present
