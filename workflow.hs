{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex.Dom
import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Either

type CategoryName = Text
type FolderId = Text
type AssetId = Text
type AssetPath = Text
type ErrorMessage = Text

newtype W r t m a = W { runW :: m (Event t (Either r a)) }

instance MonadWidget t m => Functor (W r t m) where
  fmap f x = x >>= return . f

instance MonadWidget t m => Applicative (W r t m) where
  (<*>) = ap
  pure = return

instance MonadWidget t m => Monad (W r t m) where
  return x = W $ do
    postBuild <- getPostBuild
    return $ Right x <$ postBuild
  (W x :: W r t m a) >>= (f :: a -> W r t m b) = W $ do
    rec (xDone :: Event t (Either r a), fInstalled :: Event t (Event t (Either r b))) <- runWithReplace x $ either (\_ -> return never) (runW . f) <$> xDone
    fDone :: Event t (Either r b) <- switchPromptlyDyn <$> holdDyn never fInstalled
    return $ leftmost [ fmapMaybe (\case { Left x -> Just (Left x) ; Right _ -> Nothing }) xDone
                      , fDone
                      ]

getCategories :: MonadWidget t m => Event t () -> m (Event t [CategoryName])
getCategories = return . (["MyCategory", "YourCategory"] <$)

getFolders :: MonadWidget t m => Event t () -> Dynamic t CategoryName -> m (Event t [FolderId])
getFolders doIt cn = return $ ["F1", "F2"] <$ doIt

getAsset :: MonadWidget t m => Event t () -> Dynamic t FolderId -> m (Event t [AssetId])
getAsset doIt fid = return $ ["A1", "A2"] <$ doIt

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
