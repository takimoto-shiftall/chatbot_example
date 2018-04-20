{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}

module Network.ElasticsearchSpec where

import Test.Hspec
import GHC.Generics
import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Map as M
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Aeson
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types
import Data.Resource
import Network.Elasticsearch.Connection
import Network.Elasticsearch.API

spec :: Spec
spec = do
    describe "bulk" $ do
        it "index new documents" $ do
            execOnIndex "test" $ do
                res <- bulk "test" [ Index Nothing $ (M.fromList [("id", "t1"), ("name", "test1")] :: M.Map String String)
                                   , Index Nothing $ (M.fromList [("id", "t2"), ("name", "test2")] :: M.Map String String)
                                   , Index Nothing $ (M.fromList [("id", "t2"), ("name", "test3")] :: M.Map String String)
                                   ]
                map bulkStatus res `shouldBe` replicate 3 True
            return ()

execOnIndex :: B.ByteString
            -> (With '[ESContext] => IO ())
            -> IO ()
execOnIndex index f = (do
        r <- newIORef $ Elasticsearch "localhost" 9200 False
        withContext @'[ESContext] (r `RCons` RNil) $ do
            createTestIndex index
            f
        return ()
    ) `finally` (do
        deleteTestIndex index
    )

createTestIndex :: B.ByteString -> IO ()
createTestIndex index = do
    let req = setRequestPath index
            . setRequestPort 9200
            . setRequestHost "localhost"
            . setRequestMethod "PUT" $ defaultRequest 
    res <- httpBS req
    statusCode (getResponseStatus res) `shouldBe` 200

data CountResponse = CountResponse { count :: Int } deriving (Generic)

instance FromJSON CountResponse where

countTestIndex :: B.ByteString -> IO Int
countTestIndex index = do
    let req = setRequestPath (C8.concat [index, "/_count"])
            . setRequestPort 9200
            . setRequestHost "localhost"
            . setRequestMethod "GET" $ defaultRequest 
    count <$> getResponseBody <$> httpJSON req

deleteTestIndex :: B.ByteString -> IO ()
deleteTestIndex index = do
    let req = setRequestPath index
            . setRequestPort 9200
            . setRequestHost "localhost"
            . setRequestMethod "DELETE" $ defaultRequest 
    res <- httpBS req
    statusCode (getResponseStatus res) `shouldBe` 200
