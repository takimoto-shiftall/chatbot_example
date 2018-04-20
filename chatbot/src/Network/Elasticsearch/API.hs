{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Network.Elasticsearch.API where

import GHC.Generics
import Data.IORef
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Map as M
import Data.Aeson
import Control.Lens hiding ((:>))
import Data.Extensible
import Data.Resource
import Network.Elasticsearch.Connection
import Network.Elasticsearch.Resource

data BulkAction a = Index (Maybe String) a
                  | Create (Maybe String) a
                  | Update String a
                  | Delete String
                  deriving (Show, Eq)

ndJson :: (ToJSON a)
       => BulkAction a
       -> [Value]
ndJson (Index did v) = [toJSON $ actionJson "index" did, toJSON v]
ndJson (Create did v) = [toJSON $ actionJson "create" did, toJSON v]
ndJson (Update did v) = [toJSON $ actionJson "update" (Just did), toJSON v]
ndJson (Delete did) = [toJSON $ actionJson "delete" (Just did)]

actionJson :: String
           -> Maybe String
           -> M.Map String (M.Map String String)
actionJson action (Just did) = M.fromList [("index", M.fromList [("_id", did)])] 
actionJson action Nothing = M.fromList [("index", M.empty)]

data BulkResult = BulkResult { _id :: String 
                             , status :: Int
                             } deriving (Show, Generic)

type BulkDocumentResult = M.Map String BulkResult

bulkStatus :: BulkDocumentResult
           -> Bool
bulkStatus r = (status $ snd $ head (M.toList r)) `div` 100 == 2

instance ToJSON BulkResult where
instance FromJSON BulkResult where

type BulkResult' = Record '[
    "took" >: Int
  , "errors" >: Bool
  , "items" >: [BulkDocumentResult]
  ]

bulk :: (With '[ESContext], ToJSON a)
     => BS.ByteString
     -> [BulkAction a]
     -> IO [BulkDocumentResult]
bulk index actions = do
    c <- readIORef $ contextOf @ESContext ?cxt

    $(logQD' elasticsearchTag) ?cxt $
        "Execute " ++ show (length actions) ++ " bulk actions to " ++ esurl c

    res <- executeBulk c
                "POST"
                (BS.concat ["/", index, "/_doc/_bulk"])
                [("Content-Type", "application/x-ndjson")]
                []
                (concat $ map ndJson actions) :: IO BulkResult'

    let results = view #items res

    $(logQD' elasticsearchTag) ?cxt $
        let (s, f) = L.partition bulkStatus results
        in "    Success: " ++ show (length s) ++ " docs, Failure: " ++ show (length f) ++ " docs"

    return results