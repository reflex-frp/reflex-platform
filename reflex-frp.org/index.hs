{-# LANGUAGE RecursiveDo, TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Reflex
import Reflex.Dom
import qualified Data.Map as Map
import Safe (readMay)
import Control.Applicative ((<*>), (<$>))

import Data.FileEmbed
import Data.Monoid
import Control.Monad
import Data.Map (Map)
import Debug.Trace

main = mainWidgetWithCss $(embedFile "style.css") $ do


  let links = [ ("hackage", "https://hackage.haskell.org/package/reflex")
              , ("twitter", "http://twitter.com")
              , ("github", "http://github.com/reflex-frp")
              , ("reddit", "http://reddit.com/r/reflexfrp")
              , ("irc.freenode.net #reflex-frp", "http://webchat.freenode.net/?channels=%23reflex-frp&uio=d4")
              ]

  elClass "div" "header" $ do
    elClass "h1" "logo" $ do
      elAttr "img" ("src" =: "reflex-frp-logo.jpg") (text "")
    elClass "ul" "sections" $ navMenu

  elClass "div" "main" $ do
    elClass "h3" "title" $ text "Practical Functional Reactive Programming"
    elClass "p" "class" $ text "Reflex is an fully-deterministic, higher-order Functional Reactive Programming (FRP) interface and an engine that efficiently implements that interface."


    -- Create a list of links from a list of tuples
    forM_ links $ \pair -> do
      elAttr "a" ("href" =: (snd pair)) $ text (fst pair)
      el "br" $ return ()

  return ()


navMenu :: (MonadWidget t m) => m ()
navMenu = do
  forM_ sections $ \pair -> do
    el "li" $
      elAttr "a" ("href" =: (snd pair)) $ text (fst pair)
  where sections = [ ("Home", "/")
                 , ("Tutorial", "/")
                 , ("Examples", "/")
                 , ("Documentation", "/doc/")
                 , ("FAQ", "/")]


navMenu1 :: (MonadWidget t m) => m ()
navMenu1 = do
  forM_ sections $ \link -> do
    el "li" $
      elAttr "a" ("href" =: "#") $
        text link
  where sections = [ "Home", "Tutorials", "Examples", "Documentation", "FAQ"]
