{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLabels #-}

module Main where

import Control.Monad
import Data.IORef
import Control.Lens
import Data.Extensible
import Network.TypeTalk
import Data.Model.Graph
import Data.Resource
import Database.ORM
import Database.ORM.Dialect.PostgreSQL
import Services
import Models

main :: IO ()
main = do
    db <- newResource (PostgreSQL "postgresql://postgres:postgres@127.0.0.1:15432/chatbot" 10)

    let resources = db `RCons` RNil

    graph <- fst <$> withContext @'[DBContext PostgreSQL] resources listBots

    let bots = map (\b -> let r = getRecord b in (view #token r, fromInteger $ view #topic_id r)) (values graph :: [Bot])

    forM_ bots $ \(token, topicId) -> do
        ttr <- newIORef $ TypeTalkBot token topicId

        let ttResources = ttr `RCons` RNil

        messages <- fst <$> withContext @'[TypeTalkBotC] ttResources (topicMessages Nothing Nothing)

        print messages

