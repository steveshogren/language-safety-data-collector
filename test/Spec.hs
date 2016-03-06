{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module SomeTests
       (main)
       where

import Test.HUnit
import qualified Lib as L
import qualified Database as D
import Control.Lens
import Control.Monad (join)
import qualified Data.Map as M
import Records
import Data.Functor ((<$>))
import Control.Monad.Reader (runReaderT,ReaderT)

makeLenses ''Results
makeLenses ''SeveralResults

main :: IO ()
main = do
    x <- runTestTT tests
    return ()

dateRange :: Assertion
dateRange = (L.dateRange @?= " created:\"2011-01-01..2016-01-30\"")

testDb = "testing"

blankRecord :: Assertion
blankRecord = (D.blankRecord ^. languages @?= [])

clearFileTest :: Assertion
clearFileTest = do
  actual <- runReaderT (D.clearFileT >> D.loadT) testDb
  (actual @?= D.blankRecord)

aRecord =
    Results
    { _total_count = 5
    , _language = "Haskell"
    }

doStuff :: ReaderT FilePath IO ()
doStuff = do
  D.clearFileT >> D.updateRecordT aRecord >> D.loadT

updateFileTest :: Assertion
updateFileTest = do
    actual <- runReaderT (D.clearFileT >> D.updateRecordT aRecord >> D.loadT) testDb
    ((actual ^. languages) @?= [aRecord])

stat name bug commit =
    RepoStat
    { _full_name = name
    , _bug_count = bug
    , _commit_count = commit
    }

-- when the repo name doesn't already exist
saveFieldCount :: Assertion
saveFieldCount = do
    actual <- runReaderT (D.clearRepoStatsT
                          >> D.updateRepoBugCountT "haskell" "somerepo" 2
                          >> D.updateRepoCommitCountT "haskell" "somerepo" 4
                          >> D.lookupLanguageT "haskell") testDb
    let repo = join $ (view (at "somerepo")) <$> actual
    (repo @?= Just (stat "somerepo" (Just 2) (Just 4)))

initRepoTest :: Assertion
initRepoTest = do
    actual <-
        runReaderT (D.clearRepoStatsT >>
                    D.updateRepoNameT "haskell" "somerepo" >>
                    D.lookupLanguageT "haskell") testDb
    let repo = join $ (view (at "somerepo")) <$> actual
    (repo @?= Just (stat "somerepo" Nothing Nothing))

initReposTest :: Assertion
initReposTest = do
    actual <-
        runReaderT(D.clearRepoStatsT  >>
                   D.updateRepoNamesT  "haskell" ["otherrepo", "somerepo"] >>
                   D.lookupLanguageT "haskell") testDb
    ((M.size <$> actual) @?= Just 2)
    let repo = join $ (view (at "somerepo")) <$> actual
    (repo @?= Just (stat "somerepo" Nothing Nothing))

mockApi :: String -> String -> String -> String -> IO [String]
mockApi a b c name = return [a, b, c]

peristAllNamesTest :: Assertion
peristAllNamesTest = do
    haskell <-
        runReaderT (D.clearRepoStatsT) testDb >>
        L.ipersistAllNames (mockApi "a" "b" "c") testDb "haskell" >>
        L.ipersistAllNames (mockApi "e" "f" "g") testDb "rust" >>
        runReaderT (D.lookupLanguageT "haskell") testDb
    rust <- runReaderT(D.lookupLanguageT "rust") testDb
    ((join $ (view (at "a")) <$> haskell) @?= Just (stat "a" Nothing Nothing))
    ((join $ (view (at "e")) <$> haskell) @?= Nothing)
    ((join $ (view (at "e")) <$> rust) @?= Just (stat "e" Nothing Nothing))
    ((join $ (view (at "a")) <$> rust) @?= Nothing)

tests =
    TestList
        [ "blank record" ~: blankRecord
        , "date range" ~: dateRange
        , "update record" ~: updateFileTest
        , "saving repo names with mocked api call" ~: peristAllNamesTest
        , "saving repo stats" ~: saveFieldCount
        , "init repo with name" ~: initRepoTest
        , "init several repos with names" ~: initReposTest
        , "clearing and reading from a file" ~: clearFileTest]

-- makeStat name bug commit =
--     RepoStat
--     { _full_name = name
--     , _bug_count = Just bug
--     , _commit_count = Just commit
--     }

-- getRepoStat :: String -> String -> Lens' RepoStats RepoStat
-- getRepoStat k1 k2 = at k1 . non (M.empty) . at k2 . non (makeStat k2 0 0)

-- fakeDb = M.empty :: RepoStats
-- setBug = fakeDb & (getRepoStat "a" "b") . D.bug_count ?~ 4
-- setName = fakeDb & (getRepoStat "a" "b") . D.full_name .~ "horse"
-- getBug = fakeDb ^. ((getRepoStat "a" "b"))
