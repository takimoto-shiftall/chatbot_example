{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.Elasticsearch.API where

import GHC.Generics
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Aeson
import Control.Lens hiding ((:>))
import Data.Extensible
import Data.Resource
import Network.Elasticsearch.Connection
import Network.Elasticsearch.Resource

data BulkAction a = Index (Maybe Integer) a
                  | Create a
                  | Update Integer a
                  | Delete Integer

ndJson :: (ToJSON a)
       => BulkAction a
       -> [Value]
ndJson (Index (Just did) v) = [toJSON $ M.fromList [("index" :: String, M.fromList [("_id" :: String, did)])], toJSON v]
ndJson (Index Nothing v) = [toJSON $ M.fromList [("index" :: String, M.empty :: M.Map String String)], toJSON v]
ndJson (Create v) = [toJSON $ M.fromList [("create" :: String, M.empty :: M.Map String String)], toJSON v]
ndJson (Update did v) = [toJSON $ M.fromList [("update" :: String, M.fromList [("_id" :: String, did)])], toJSON v]
ndJson (Delete did) = [toJSON $ M.fromList [("delete" :: String, M.fromList [("_id" :: String, did)])]]

data BulkResult = BulkResult { _id :: Int 
                             , status :: Int
                             } deriving (Generic)

instance ToJSON BulkResult where
instance FromJSON BulkResult where

type BulkResult' = Record '[
    "took" >: Int
  , "errors" >: Bool
  , "items" >: [BulkResult]
  ]

bulk :: (With '[ESContext], ToJSON a)
     => BS.ByteString
     -> [BulkAction a]
     -> IO [BulkResult]
bulk index actions = do
    c <- readIORef $ contextOf @ESContext ?cxt
    res <- executeBulk c
                "POST"
                (BS.concat ["/", index, "/_doc/_bulk"])
                [("Content-Type", "application/x-ndjson")]
                []
                (concat $ map ndJson actions) :: IO BulkResult'
    return $ view #items res