{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Database (blankRecord
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
import qualified Data.ByteString as Str
import qualified Data.ByteString.Lazy as Lz
import qualified Data.ByteString.Char8 as BS
import Control.Lens

makeLenses ''Results
makeLenses ''SeveralResults

blankRecord = SeveralResults { _languages = [] }

load :: (Read a) => FilePath -> IO a
load f = read <$> BS.unpack <$> Str.readFile f

save :: (Show a) => a -> FilePath -> IO ()
save x f = Str.writeFile f (BS.pack . show $ x)

clearFile :: String -> IO ()
clearFile f = save blankRecord f

updateRecord f aRecord = do
  db <- load f
  save (db & languages <>~ [aRecord]) f
