{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
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

loadResult :: FilePath -> IO [BenchmarkResult]
loadResult p = either error id . eitherDecode <$> LBS.readFile p

main :: IO ()
main = do
  [file1, file2] <- getArgs
  let resultMap = Map.fromList . fmap (_benchmarkResult_benchmark &&& _benchmarkResult_geometricMean)
      getResults = fmap resultMap . loadResult
  results1 <- getResults file1
  results2 <- getResults file2
  let showMNum = maybe "?" (T.pack . show)
      header = "| Benchmark | Before | After | Ratio |"
      separator = "| --- | --- | --- | --- |"
      formatLine (b, rs) =
        let r1 = preview here rs
            r2 = preview there rs
        in "| " <> b <> " | " <> showMNum r1 <> " | " <> showMNum r2 <> " | " <> showMNum ((/) <$> r2 <*> r1) <> " |"

  T.putStrLn $ T.unlines $ header : separator : fmap formatLine (Map.toList $ align results1 results2)
