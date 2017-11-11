{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Data.Aeson
import Data.Aeson.TH
import Data.Align
import qualified Data.ByteString.Lazy as LBS
import Data.Either
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)
import Data.These
import System.Directory
import System.Environment
import System.FilePath

data BenchmarkResult = BenchmarkResult
  { _benchmarkResult_framework :: Text
  , _benchmarkResult_benchmark :: Text
  , _benchmarkResult_type :: Text
  , _benchmarkResult_min :: Double
  , _benchmarkResult_max :: Double
  , _benchmarkResult_mean :: Double
  , _benchmarkResult_median :: Double
  , _benchmarkResult_geometricMean :: Double
  , _benchmarkResult_standardDeviation :: Double
  , _benchmarkResult_values :: [Double]
  }
  deriving (Show, Read, Eq, Ord)

deriveJSON (defaultOptions { fieldLabelModifier = drop $ length ("_benchmarkResult_" :: String) }) ''BenchmarkResult

loadResult :: FilePath -> IO BenchmarkResult
loadResult p = either error id . eitherDecode <$> LBS.readFile p

main :: IO ()
main = do
  [dir1, dir2] <- getArgs
  let loadDir d = mapM (loadResult . (d </>)) =<< listDirectory d
      relevant br = _benchmarkResult_framework br == "reflex-dom-v0.4-keyed"
      resultMap = Map.fromList . fmap (\br -> (_benchmarkResult_benchmark br, _benchmarkResult_geometricMean br))
      getResults = fmap (resultMap . filter relevant) . loadDir
  results1 <- getResults dir1
  results2 <- getResults dir2
  let showMNum :: Maybe Double -> Text
      showMNum = \case
        Just d -> T.pack $ show d
        Nothing -> "?"
      header = "| Benchmark | Before | After | Ratio |"
      separator = "| --- | --- | --- | --- |"
      formatLine (b, rs) =
        let r1 = preview here rs
            r2 = preview there rs
        in "| " <> b <> " | " <> showMNum r1 <> " | " <> showMNum r2 <> " | " <> showMNum ((/) <$> r2 <*> r1) <> " |"

  T.putStrLn $ T.unlines $ header : separator : fmap formatLine (Map.toList $ align results1 results2)
