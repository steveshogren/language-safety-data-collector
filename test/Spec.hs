{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import qualified Lib as L
import qualified Database as D
import Control.Lens
import qualified Data.Map as M
import Records
import Data.Functor ((<$>))

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
    actual <- D.clearFile testDb >> D.load testDb
    (actual @?= D.blankRecord)

aRecord =
    Results
    { _total_count = 5
    , _language = "Haskell"
    }

updateFileTest :: Assertion
updateFileTest = do
    actual <-
        D.clearFile testDb >> D.updateRecord testDb aRecord >> D.load testDb
    ((actual ^. languages) @?= [aRecord])

stat name bug commit =
    RepoStat
    { _full_name = name
    , _bug_count = Just bug
    , _commit_count = Just commit
    }

saveFieldCount :: Assertion
saveFieldCount = do
    actual <-
        D.clearRepoStats testDb >>
        D.updateRepoBugCount testDb "haskell" "somerepo" 2 >>
        D.updateRepoCommitCount testDb "haskell" "somerepo" 4 >>
        D.lookupLanguage testDb "haskell"
    ((actual ^. at "somerepo") @?= Just (stat "somerepo" 2 4))

tests =
    TestList
        [ "blank record" ~: blankRecord
        , "date range" ~: dateRange
        , "update record" ~: updateFileTest
        , "saving repo stats" ~: saveFieldCount
        , "clearing and reading from a file" ~: clearFileTest]

makeStat name bug commit =
    RepoStat
    { _full_name = name
    , _bug_count = Just bug
    , _commit_count = Just commit
    }

getRepoStat :: String -> String -> (RepoStat -> Identity RepoStat) -> RepoStats -> Identity RepoStats
getRepoStat k1 k2 = at k1 . non (M.empty) . at k2 . non (makeStat k2 0 0)

fakeDb = M.empty :: RepoStats
setBug = fakeDb & (getRepoStat "a" "b") . D.bug_count ?~ 4
setName = fakeDb & (getRepoStat "a" "b") . D.full_name .~ "horse"
--getBug = fakeDb ^. ((getRepoStat "a" "b"))
