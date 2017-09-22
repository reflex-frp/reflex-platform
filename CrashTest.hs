{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom

import Control.Monad.Trans
import Data.Time.Clock

import GHCJS.DOM.HTMLTextAreaElement (HTMLTextAreaElement)
import qualified GHCJS.DOM.HTMLTextAreaElement as TA
import qualified GHCJS.DOM.HTMLElement as El
import GHCJS.DOM.Types (MonadJSM)

import qualified Data.Text as T

import Emoji

say :: MonadIO m => String -> m ()
say = liftIO . putStrLn

setSelectionPos :: (HasJS x (WidgetHost m), PerformEvent t m, MonadJSM (Performable m)) => HTMLTextAreaElement -> Event t Int -> m ()
setSelectionPos t e = performEvent_ . ffor e $ \n -> do 
  TA.setSelectionStart t n
  TA.setSelectionEnd t n
  El.focus t

main :: IO ()
main = do
  now <- getCurrentTime
  mainWidget $ do
    rec tick <- tickLossy 0.000001 now
        let emojiPickE = () <$ tick
        t <- textAreaElement $ def { _textAreaElementConfig_setValue = Just (fmap snd newEmoji) }
        setSelectionPos (_textAreaElement_raw t) resetPosE 
        let newEmoji = attachWith (\s _ -> let (n',s') = insertEmojiAt (T.unpack s) in (n', T.pack s')) 
                                  (current $ value t) emojiPickE
            resetPosE = fmap fst newEmoji
    return ()