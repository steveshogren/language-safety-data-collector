{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import qualified Lib as L
import qualified Database as D
import Control.Lens
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

d a b c =
  a + b + b

saveFieldCount :: Assertion
saveFieldCount = do
    actual <-
        D.clearRepoStats testDb >>
        D.updateRepoBugCount testDb "haskell" "somerepo" 2 >>
        D.updateRepoCommitCount testDb "haskell" "somerepo" 4 >>
        D.lensIt testDb "haskell"
    ((actual ^. at "somerepo") @?= Just (stat "somerepo" 2 4))

tests =
    TestList
        [ "blank record" ~: blankRecord
        , "date range" ~: dateRange
        , "update record" ~: updateFileTest
        , "saving repo stats" ~: saveFieldCount
        , "clearing and reading from a file" ~: clearFileTest]
