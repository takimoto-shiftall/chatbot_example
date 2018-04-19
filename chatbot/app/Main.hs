{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import Data.IORef
import Control.Lens
import Data.Extensible
import Control.Monad.Logger
import System.Log.FastLogger
import Data.Resource
import Database.ORM
import Database.ORM.Dialect.PostgreSQL
import Network.Elasticsearch
import Services

main :: IO ()
main = do
    db <- newResource (PostgreSQL "postgresql://postgres:postgres@127.0.0.1:15432/chatbot" 10)
    es <- newIORef $ Elasticsearch "127.0.0.1" 9200 False
    lr <- newLoggingResource [(anyTag, LevelDebug, LogStdout defaultBufSize, Nothing)] >>= newIORef

    let resources = es `RCons` db `RCons` lr `RCons` RNil

    total <- fst <$> withContext @'[DBContext PostgreSQL, ESContext] resources importAll

    print $ "Import " ++ show total ++ " posts"
