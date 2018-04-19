{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Network.TypeTalk.API where

import Data.IORef
import Data.String (fromString)
import Data.Maybe (catMaybes, maybe)
import Data.Time.Clock
import Data.Extensible
import Control.Lens
import Data.Resource
import Network.TypeTalk.Rest
import Network.TypeTalk.Model

type TopicMessages = Record '[
    "topic" >: Topic
  , "posts" >: [TopicPost]
  ]

maximumMessageCount = 200

topicMessages :: (With '[TypeTalkBotC])
              => Maybe Int
              -> Maybe Int
              -> Bool
              -> IO TopicMessages
topicMessages count from forward = do
    bot@(TypeTalkBotC (TypeTalkBot _ topicId)) <- readIORef $ contextOf @TypeTalkBotC ?cxt
    let conv k = (k, ) . Just . fromString . show
    let q = catMaybes [ conv "count" <$> count
                      , conv "from" <$> from
                      , Just ("direction", Just $ if forward then "forward" else "backward")]
    get bot ("/v1/topics/" ++ show topicId) q

allTopicMessages :: (With '[TypeTalkBotC])
                 => Maybe Int
                 -> IO [TopicPost]
allTopicMessages latest = get (maybe 0 id latest)
    where
        get :: Int -> IO [TopicPost]
        get mid = do 
            messages <- topicMessages (Just maximumMessageCount) (Just mid) True
            let posts = view #posts messages
            case reverse posts of
                [] -> return []
                p:_ -> (posts ++) <$> allTopicMessages (Just $ view #id p)