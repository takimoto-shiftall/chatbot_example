{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Elasticsearch.Connection where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString hiding (map)
import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Network.HTTP.Types

data Elasticsearch =
    Elasticsearch { host :: ByteString
                  , port :: Int
                  , secure :: Bool
                  }

data ESContext = ESContext Elasticsearch

execute :: (MonadIO m, ToJSON a, FromJSON b)
        => ESContext
        -> ByteString
        -> ByteString
        -> [Header]
        -> Query
        -> a
        -> m b
execute (ESContext (Elasticsearch {..})) method path headers queries body = getResponseBody <$> httpJSON req
    where
        req = setRequestBodyJSON body
            . setRequestQueryString queries
            . setRequestHeaders headers
            . setRequestPath path
            . setRequestPort port
            . setRequestHost host
            . setRequestMethod method
            . setRequestSecure secure $ defaultRequest

executeBulk :: (MonadIO m, ToJSON a, FromJSON b)
            => ESContext
            -> ByteString
            -> ByteString
            -> [Header]
            -> Query
            -> [a]
            -> m b
executeBulk (ESContext (Elasticsearch {..})) method path headers queries items = getResponseBody <$> httpJSON req
    where
        body = B.intercalate "\n" (map encode items)
        req = setRequestBodyLBS (B.append body "\n")
            . setRequestQueryString queries
            . setRequestHeaders headers
            . setRequestPath path
            . setRequestPort port
            . setRequestHost host
            . setRequestMethod method
            . setRequestSecure secure $ defaultRequest
