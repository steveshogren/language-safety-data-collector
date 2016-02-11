{-# LANGUAGE DeriveGeneric #-}
module Database (
              blankRecord
              --  load
              --  , addData
              --  , clearFile
              --  , generateJson
              --  , Metric
              --  , MetricHistory
               ) where

import Data.Time ( showGregorian,  localDay, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Data.Time.Format (defaultTimeLocale)
import Control.Monad(liftM2, liftM)
import Records
import qualified Data.Map as M
import qualified Data.List as L
import Control.Lens

blankRecord = SeveralResults { languages = [] }

-- today :: IO String
-- today = do
--   time <- liftM2 utcToLocalTime getCurrentTimeZone getCurrentTime
--   return $ showGregorian $ localDay time

-- type Metric = M.Map String Int
-- type MetricHistory = M.Map String Metric

load :: (Read a) => FilePath -> IO a
load f = do
   s <- readFile f
   return (read s)

save :: (Show a) => a -> FilePath -> IO ()
save x f = writeFile f (show x)

clearFile :: String -> IO ()
clearFile f = save blankRecord f

-- queryData :: MetricHistory -> String -> Metric
-- queryData db metricName = do
--   case M.lookup metricName db of
--     Nothing -> M.empty
--     Just metric -> metric

-- addData :: String -> String -> Int -> IO MetricHistory
-- addData file metricName count = do
--   db <- load file
--   let innerMetric = queryData db metricName
--     in length updatedMap `seq` (save updatedMap file >> load file)
