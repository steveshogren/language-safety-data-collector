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

-- repo_count lang date-range "+forks:\">15\"&per_page=200&page=" num
-- repo_count "https://api.github.com/search/repositories"

opts :: String -> String -> Options
opts lang token = defaults
                    & param "q" .~ [T.pack $ "language:" ++ lang ++ " forks:\">15\""]
                    & param "per_page" .~ ["200"]
                    & param "page" .~ ["1"]
                           & header "Authorization" .~ [BS.pack $ "token " ++ token]

query lang = do
    token <- readFile "token"
    let options = (opts lang token)
    print options
    r <- getWith options "https://api.github.com/search/repositories"
    let repo_names = r ^.. responseBody . key "items" . values . key "name" . _String
    let repo_counts = r ^? responseBody . key "total_count" . _Number
    return $ (repo_names, repo_counts)

someFunc :: IO ()
someFunc = (query "ruby") >>= print
