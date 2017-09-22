{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

import Reflex.Dom

import Control.Monad.Trans

import GHCJS.DOM.HTMLTextAreaElement (HTMLTextAreaElement)
import qualified GHCJS.DOM.HTMLTextAreaElement as TA
import qualified GHCJS.DOM.HTMLElement as El
import GHCJS.DOM.Types (MonadJSM)

import qualified Data.Text as T

import Emoji

say :: MonadIO m => String -> m ()
say = liftIO . putStrLn

setSelectionPos :: (HasJS x (WidgetHost m), PerformEvent t m, MonadJSM (Performable m)) => HTMLTextAreaElement -> Event t Int -> m (Event t ())
setSelectionPos t e = performEvent . ffor e $ \n -> do 
  TA.setSelectionStart t n
  TA.setSelectionEnd t n
  El.focus t
  return ()

main :: IO ()
main = do
  mainWidget $ do
    rec begin <- button "Begin"
        t <- textAreaElement $ def { _textAreaElementConfig_setValue = Just (fmap snd newEmoji) }
        again <- setSelectionPos (_textAreaElement_raw t) resetPosE
        --again' <- delay 0 again
        let newEmoji = attachWith (\s _ -> let (n',s') = insertEmojiAt (T.unpack s) in (n', T.pack s')) 
                                  (current $ value t) (leftmost [begin,again])
            resetPosE = fmap fst newEmoji
    return ()
