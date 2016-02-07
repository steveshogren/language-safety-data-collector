{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Network.Wreq
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

-- lens stuff
-- ^? - traverse for a single Maybe thing
-- ^.. - traverse for a list of things
-- values - start operating on a list of values
-- key string - pull out an element by key name
-- . - regular compose, but somehow backwards now..?

-- https://api.github.com/search/issues?q=label:bug

authHeader token = header "Authorization" .~ [BS.pack $ "token " ++ token]
dateRange =  " created:\"2011-01-01..2016-01-30\""
bugLabel = " label:bug"

bugSearchOpts repo token = defaults
                    & param "q" .~ [T.pack $ "repo:" ++ repo ++ dateRange ++ bugLabel]
                    & authHeader token
                    & header "User-Agent" .~ ["steveshogren"]


repoSearchOpts :: String -> String -> Int -> Options
repoSearchOpts lang token page = defaults
                    & param "q" .~ [T.pack $ "language:" ++ lang
                                      ++ " forks:\">15\"" ++ dateRange]
                    & param "per_page" .~ ["200"] & param "page" .~ [T.pack . show $ page]
                    & authHeader token
                    & header "User-Agent" .~ ["steveshogren"]

bugSearch repo = do
    token <- readFile "token"
    getWith (bugSearchOpts repo token) "https://api.github.com/search/issues"

repoSearch lang page = do
    token <- readFile "token"
    getWith (repoSearchOpts lang token page) "https://api.github.com/search/repositories"

collectTotalCount lang = do
    r <- repoSearch lang 1
    return $ r ^? responseBody . key "total_count" . _Number

collectNames :: String -> Int -> IO [String]
collectNames lang page = do
    r <- repoSearch lang page
    return . map T.unpack $ r ^.. responseBody . key "items" . values . key "full_name" . _String

collectAllNames :: String -> IO [String]
collectAllNames lang =
    concat <$> (sequence $ map (collectNames lang) [1..10])

collectBugCountsForRepo repo = do
    bugs <- bugSearch repo
    return $ bugs ^? responseBody . key "total_count" . _Number

-- looks like search now limits to 30/hr, or 1 every 2min
-- i want to search 1000 repos per lang... or, 33 hours a lang...
collectBugCountsForLang lang = do
     repos <- collectNames lang 1
     sequence $ map collectBugCountsForRepo repos

someFunc :: IO ()
someFunc = (repoSearch "ruby" 1) >>= print
