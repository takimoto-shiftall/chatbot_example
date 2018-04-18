{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.TypeTalk.API where

import Data.IORef
import Data.String (fromString)
import Data.Maybe (catMaybes)
import Data.Time.Clock
import Data.Extensible
import Data.Resource
import Network.TypeTalk.Rest
import Network.TypeTalk.Model

type TopicMessages = Record '[
    "topic" :> Topic
  , "posts" :> [TopicPost]
  ]

topicMessages :: (With '[TypeTalkBotC])
              => Maybe Int
              -> Maybe Int
              -> IO TopicMessages
topicMessages count from = do
    bot@(TypeTalkBotC (TypeTalkBot _ topicId)) <- readIORef $ contextOf @TypeTalkBotC ?cxt
    let conv k = (k, ) . Just . fromString . show
    let q = catMaybes [conv "count" <$> count, conv "from" <$> from]
    get bot ("/v1/topics/" ++ show topicId) q