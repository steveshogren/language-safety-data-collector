{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Lib
    ( someFunc
    ) where

import Control.Monad
import Network
import Network.HTTP.Conduit
import Network.HTTP.Types.Header
import qualified Data.ByteString.Char8 as BD
import GHC.Generics
import Data.Aeson

data ResultCount = ResultCount {
  total_count :: Int }
  deriving (Generic, Show)

instance ToJSON ResultCount
instance FromJSON ResultCount

query :: String -> IO (Maybe Int)
query lang = do
    initReq <- parseUrl "https://api.github.com/search/repositories?q=tetris+language:assembly"
    let r = initReq
                   { method = "GET"
                    , requestHeaders = [(hUserAgent, "steveshogren")
                                      , (hAuthorization, "token")]}
    let request = setQueryString [("q", Just (BD.pack $ " language:" ++ lang))
                                 , ("order", Just "desc")
                                 ,("sort", Just "stars")] r
    manager <- newManager tlsManagerSettings
    res <- httpLbs request manager
    return . liftM total_count . decode . responseBody $ res

someFunc :: IO ()
someFunc = (query "ruby") >>= print
