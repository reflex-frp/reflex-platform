{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex.Dom
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Either
import Control.Monad.Except

type CategoryName = Text
type FolderId = Text
type AssetId = Text
type AssetPath = Text
type ErrorMessage = Text

newtype W t m a = W { unW :: Either a (m (Event t (W t m a))) } --TODO: We probably want to clear the widget when it's done and only let the "Done" event fire once

runW :: (MonadAdjust t m, PostBuild t m, MonadHold t m, MonadFix m) => W t m a -> m (Event t a)
runW w0 = do
  let go (W w) = case w of
        Left l -> (return l <$) <$> getPostBuild
        Right r -> r
  rec (next0, built) <- runWithReplace (go w0) $ either (const $ return never) id . unW <$> next
      next <- switch <$> hold next0 built
  return $ fmapMaybe (either Just (const Nothing) . unW) next

instance (Reflex t, Functor m) => Functor (W t m) where
  fmap f x = x >>= return . f

instance (Reflex t, Functor m) => Applicative (W t m) where
  (<*>) = ap
  pure = return

instance (Reflex t, Functor m) => Monad (W t m) where
  return x = W $ Left x
  W x >>= f = W $ case x of
    Left l -> unW $ f l
    Right r -> Right $ fmap (fmap (>>= f)) r --TODO: This is too much fmapping

prompt :: (Reflex t, Functor m) => m (Event t a) -> W t m a
prompt p = W $ Right $ fmap return <$> p

getCategories :: MonadWidget t m => Event t () -> m (Event t [CategoryName])
getCategories = return . (["MyCategory", "YourCategory"] <$)

getFolders :: MonadWidget t m => Event t () -> Dynamic t CategoryName -> m (Event t [FolderId])
getFolders doIt cn = return $ ["F1", "F2"] <$ doIt

getAsset :: MonadWidget t m => Event t () -> Dynamic t FolderId -> m (Event t [AssetId])
getAsset doIt fid = return $ ["A1", "A2"] <$ doIt

main :: IO ()
main = mainWidget $ do
  done <- runW $ runExceptT $ do
    v <- lift $ prompt $ do
      i <- inputElement def
      tag (current (value i)) <$> button "Submit"
    when (v == "") $ throwError "Can't be empty!"
    lift $ prompt $ do
      text "Looks good!"
      button "Finish"
  el "div" $ do
    text "Results: "
    display =<< foldDyn (:) [] done

{-
checkCategoryName :: MonadWidget t m => CategoryName -> W ErrorMessage t m ()
checkCategoryName n = W $ do
  cats <- getCategories =<< getPostBuild
  return $ ffor cats $ \c -> if n `notElem` c
    then Left "Category not in list"
    else Right ()

checkFolderName :: MonadWidget t m => CategoryName -> FolderId -> W ErrorMessage t m ()
checkFolderName cn fid = W $ do
  postBuild <- getPostBuild
  folders <- getFolders postBuild $ constDyn cn
  return $ ffor folders $ \fs -> if fid `notElem` fs
    then Left "Folder not in list"
    else Right ()

main :: IO ()
main = mainWidget $ do
  testResultRetrieved <- test
  latestResult <- holdDyn "" $ show <$> testResultRetrieved
  display latestResult

test :: MonadWidget t m => m (Event t (Either ErrorMessage AssetPath))
test = do
  runW $ do
    let cn = "MyCategory"
    let fn = "F1"
    checkCategoryName cn
    checkFolderName cn fn
    return "TheAssetPath"
-}
