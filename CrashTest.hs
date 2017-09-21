{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom

import Control.Exception (evaluate)
import Control.Monad.Trans
import Data.Time.Clock

import GHCJS.DOM.HTMLTextAreaElement (HTMLTextAreaElement)
import qualified GHCJS.DOM.HTMLTextAreaElement as TA
import qualified GHCJS.DOM.HTMLElement as El
import GHCJS.DOM.Types (MonadJSM)

import Emoji

say :: MonadIO m => String -> m ()
say = liftIO . putStrLn

attachSelectionStart :: (HasJS x (WidgetHost m), PerformEvent t m, MonadJSM (Performable m)) => HTMLTextAreaElement -> Event t a -> m (Event t (Int, a))
attachSelectionStart t e = performEvent . ffor e $ \v -> do
  say "a start"
  n <- TA.getSelectionStart t
  say "a done"
  return (n, v)

setSelectionPos :: (HasJS x (WidgetHost m), PerformEvent t m, MonadJSM (Performable m)) => HTMLTextAreaElement -> Event t Int -> m ()
setSelectionPos t e = performEvent_ . ffor e $ \n -> do 
  say "s force t"
  liftIO (evaluate t)
  say "s force n"
  liftIO (evaluate n)
  say "s start"
  TA.setSelectionStart t n
  say "s end"
  TA.setSelectionEnd t n
  say "s focus"
  El.focus t
  say "s done"

main = do
  now <- getCurrentTime
  mainWidget $ do
    rec tick <- tickLossy 0.05 now
        let emojiPickE = "blush" <$ tick
        t <- textAreaElement $ def { _textAreaElementConfig_setValue = Just (fmap snd newEmoji) }
        setSelectionPos (_textAreaElement_raw t) resetPosE 
        posPickE <- attachSelectionStart (_textAreaElement_raw t) emojiPickE
        let newEmoji = attachWith (\s (n, e) -> insertEmojiAt EMPlusOne e n s) (current $ value t) posPickE
            resetPosE = fmap fst newEmoji
    return ()