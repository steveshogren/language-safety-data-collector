{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Database (
              blankRecord
                , load
                , clearFile
                , updateRecord
               ) where

import Data.Time ( showGregorian,  localDay, getCurrentTime, getCurrentTimeZone, utcToLocalTime)
import Data.Time.Format (defaultTimeLocale)
import Control.Monad(liftM2, liftM)
import Records
import qualified Data.Map as M
import qualified Data.List as L
import Control.Lens

makeLenses ''Results
makeLenses ''SeveralResults

blankRecord = SeveralResults { _languages = [] }

load :: (Read a) => FilePath -> IO a
load f = do
   s <- readFile f
   return (read s)

save :: (Show a) => a -> FilePath -> IO ()
save x f = writeFile f (show x)

clearFile :: String -> IO ()
clearFile f = save blankRecord f

updateRecord f aRecord = do
  db <- load f
  let updated = db & languages <>~ [aRecord]
  save updated f

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
