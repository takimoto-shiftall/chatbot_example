{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

module Services where

import Control.Monad
import Data.IORef
import Data.Proxy
import Data.Time
import Control.Lens
import Data.Extensible
import Data.Model.Graph
import Data.Resource
import Database.ORM
import Database.ORM.Dialect.PostgreSQL
import Network.Elasticsearch
import Network.TypeTalk
import Model.Tables
import Model.Indices

importAll :: (With '[DBContext PostgreSQL, ESContext])
          => IO Int
importAll = do
    bots <- with @'[DBContext PostgreSQL] listBots
    cs <- forM (values bots :: [Bot]) $ \b -> do
        let (token, topicId, lastPost) = let r = getRecord b in (view #token r, view #topic_id r, view #last_post r)
        ttr <- newIORef $ TypeTalkBot token (fromInteger topicId)
        posts <- fst <$> withContext @'[TypeTalkBotC] (ttr `RCons` RNil) (allTopicMessages (Just $ fromInteger lastPost))
        importPosts b posts
        return $ length posts
    return $ sum cs

listBots :: (With '[DBContext PostgreSQL])
         => IO (Graph Bot)
listBots = do
    selectNodes (Proxy :: Proxy (Graph Bot))
                (Proxy :: Proxy Bot)
                (..?)
                (../)
                Nothing

type BotPostUpdate = "bot" :// Record (BotCols ^@ '["id", "last_post", "modified_at"])
type PostUpdateGraph = Graph BotPostUpdate

importPosts :: (With '[DBContext PostgreSQL, ESContext])
            => Bot
            -> [TopicPost]
            -> IO ()
importPosts _ [] = return ()
importPosts bot posts = do
    now <- getCurrentTime
    let newBot = let r = getRecord bot in Model ( #id @= view #id r
                                               <: #last_post @= toInteger (view #id (last posts))
                                               <: #modified_at @= now
                                               <: emptyRecord) :: BotPostUpdate
    let graph = fst $ newBot +< (newGraph :: PostUpdateGraph)
    with @'[DBContext PostgreSQL] $ restoreGraph graph (Proxy :: Proxy (Serialize PostUpdateGraph))

    let docs = (`map` posts) $ \p -> ( #post_id @= view #id p
                                    <: #topic_id @= view #topicId p
                                    <: #message @= view #message p
                                    <: #account_id @= (view #id . view #account) p
                                    <: #likes @= map (view #id . view #account) (view #likes p)
                                    <: #talks @= map (view #id) (view #talks p)
                                    <: #created_at @= view #createdAt p
                                    <: #modified_at @= view #updatedAt p
                                    <: emptyRecord)
    with @'[ESContext] $ do
        forM_ (sep 100 docs) $ \ds -> do
            bulk "post" (map (Index Nothing) ds)
    where
        sep :: Int -> [a] -> [[a]]
        sep len vs
            | length vs > len = take len vs : sep len (drop len vs)
            | length vs == 0 = []
            | otherwise = [vs]