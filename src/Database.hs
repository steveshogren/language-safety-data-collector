{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Database
       (blankRecord, load, clearFile, updateRecord, clearRepoStats,
        updateRepoBugCount, updateRepoCommitCount, lensIt, bug_count,
        commit_count)
       where

import Records
import qualified Data.Map as M
import qualified Data.ByteString as Str
import qualified Data.ByteString.Char8 as BS
import Control.Lens
import Data.Map.Lens
import qualified Data.Map as M

makeLenses ''Results
makeLenses ''SeveralResults
makeLenses ''RepoStat

blankRecord =
    SeveralResults
    { _languages = []
    }

load :: (Read a) => FilePath -> IO a
load f = read <$> BS.unpack <$> Str.readFile f

save :: (Show a) => a -> FilePath -> IO ()
save x f = Str.writeFile f (BS.pack . show $ x)

clearFile :: String -> IO ()
clearFile f = save blankRecord f

updateRecord f aRecord = do
    db <- load f
    save (db & languages <>~ [aRecord]) f

blankRepoStats = (M.fromList [("haskell", M.empty)]) :: RepoStats

clearRepoStats :: String -> IO ()
clearRepoStats f = save blankRepoStats f

makeNewRepo repoName =
    RepoStat
    { _full_name = repoName
    , _bug_count = Nothing
    , _commit_count = Nothing
    }

gt :: String -> String -> RepoStats -> RepoStat
gt lang repoName stats =
    (stats ^. at lang . non M.empty . at repoName . non (makeNewRepo repoName))

updateRepoFieldCount
    :: ((a -> Identity (Maybe b)) -> RepoStat -> Identity RepoStat)
    -> FilePath
    -> String
    -> String
    -> b
    -> IO ()
updateRepoFieldCount field f lang repoName count = do
    db <- load f :: IO (RepoStats)
    let updated =
            (at lang . _Just . at repoName . non (makeNewRepo repoName) . field ?~
             count) $
            db
    let updated2 = ((gt lang repoName db) ^. bug_count)
    save updated f
updateRepoBugCount = updateRepoFieldCount bug_count
updateRepoCommitCount = updateRepoFieldCount commit_count

lensIt f lang = do
    db <- load f :: IO (RepoStats)
    return $ db ^. at lang . non M.empty
