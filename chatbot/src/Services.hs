module Services where

import Data.Proxy
import Data.Model.Graph
import Data.Resource
import Database.ORM
import Database.ORM.Dialect.PostgreSQL
import Models

listBots :: (With '[DBContext PostgreSQL])
         => IO (Graph Bot)
listBots = do
    selectNodes (Proxy :: Proxy (Graph Bot))
                (Proxy :: Proxy Bot)
                (..?)
                (../)
                Nothing