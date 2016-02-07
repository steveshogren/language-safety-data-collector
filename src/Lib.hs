{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Network.Wreq
import Control.Monad
import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

loadToken :: IO String
loadToken = readFile "token"

opts l token = defaults & param "q" .~ [T.pack $ "tetris language:" ++ l]
                        & param "order" .~ ["desc"]
                        & param "sort" .~ ["stars"]
                        & header "User-Agent" .~ ["steveshogren"]
                        & header "Authorization" .~ [BS.pack $ "token " ++ token]

query lang = do
    token <- loadToken
    r <- getWith (opts lang token) "https://api.github.com/search/repositories"
    return $ r ^? responseBody . key "total_count" . _Number

someFunc :: IO ()
someFunc = (query "ruby") >>= print
