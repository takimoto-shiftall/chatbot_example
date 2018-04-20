{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Elasticsearch.Resource where

import Control.Monad.IO.Class
import Data.IORef
import Data.Resource
import Network.Elasticsearch.Connection

elasticsearchTag = "Network.Elasticsearch"

instance Resource Elasticsearch where
    type ContextType Elasticsearch = ESContext

    newContext r = liftIO $ readIORef r >>= newIORef . ESContext

instance ResourceContext ESContext where
    type ResourceType ESContext = Elasticsearch

    closeContext c b = return c
    execContext c f = f