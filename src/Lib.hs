{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
     , dateRange
       , ipersistAllNames
    ) where

import Network.Wreq
import Control.Lens
import Control.Monad (join, mapM_)
import Data.Aeson
import Data.Aeson.Lens
import Records
import qualified Data.Text as T
import qualified Database as D
import qualified Data.ByteString.Char8 as BS

-- lens stuff
-- ^? - traverse for a single Maybe thing
-- ^.. - traverse for a list of things
-- values - start operating on a list of values
-- key string - pull out an element by key name
-- . - regular compose, but somehow backwards now..?

-- https://api.github.com/search/issues?q=label:bug

addAuthHeader token opts = opts & header "Authorization" .~ [BS.pack $ "token " ++ token]
                                & header "User-Agent" .~ ["steveshogren"]

dateRange =  " created:\"2011-01-01..2016-01-30\""
bugLabel = " label:bug"

bugSearchOpts :: String -> String -> Options
bugSearchOpts repo token = addAuthHeader token $ defaults
                    & param "q" .~ [T.pack $ "repo:" ++ repo ++ dateRange ++ bugLabel]

repoSearchOpts :: String -> String -> Int -> Options
repoSearchOpts lang token page = addAuthHeader token $ defaults
                    & param "q" .~ [T.pack $ "language:" ++ lang
                                      ++ " forks:\">15\"" ++ dateRange]
                    & param "per_page" .~ ["200"] & param "page" .~ [T.pack . show $ page]

bugSearch repo = do
    token <- readFile "token"
    getWith (bugSearchOpts repo token) "https://api.github.com/search/issues"

repoSearch lang page = do
    token <- readFile "token"
    getWith (repoSearchOpts lang token page) "https://api.github.com/search/repositories"

extractTotalCount r = r ^? responseBody . key "total_count" . _Number

collectTotalCount lang = extractTotalCount <$> repoSearch lang 1

extractFullNames r =
    let names =
            r ^.. responseBody . key "items" . values . key "full_name" .
            _String
    in map T.unpack names

collectPageOfNames :: String -> Int -> IO [String]
collectPageOfNames lang page = extractFullNames <$> repoSearch lang page

collectAllNames :: String -> IO [String]
collectAllNames lang =
    concat <$> (sequence $ map (collectPageOfNames lang) [1 .. 10])

collectBugCountsForRepo repo =
    extractTotalCount <$> bugSearch repo

-- looks like search now limits to 30/hr, or 1 every 2min
-- i want to search 1000 repos per lang... or, 33 hours a lang...
collectBugCountsForLang lang =
     sequence . (map collectBugCountsForRepo) <$> collectPageOfNames lang 1

someFunc :: IO ()
someFunc = (repoSearch "ruby" 1) >>= print

ipersistAllNames :: (String -> IO [String]) -> FilePath -> String -> IO ()
ipersistAllNames getNames f lang = do
  names <- getNames lang
  D.clearRepoStats f
  mapM_ (D.updateRepoName f lang) names
persistAllNames = ipersistAllNames collectAllNames

persistAllReposForAllLangs :: FilePath -> IO ()
persistAllReposForAllLangs f = mapM_ (\l -> persistAllNames (f++"_"++l) l) allLangs

allLangs = ["haskell", "rust", "clojure", "js", "java", "csharp", "python", "ruby", "php", "perl", "scala", "go"]
--allLangs = ["haskell", "rust"]
