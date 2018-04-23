{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Service.ImportSpec where

import Test.Hspec
import Data.IORef
import Data.Time
import Data.Proxy
import System.Environment
import Control.Lens
import Data.Extensible
import Database.PostgreSQL.Simple
import Data.Model.Graph
import Data.Resource
import Database.ORM
import Database.ORM.Dialect.PostgreSQL
import Network.TypeTalk
import Model.Tables
import Service.Import

type BotGraph = Graph Bot

spec :: Spec
spec = beforeAll prepareTestDB
     $ around wrapAction $ do
        describe "updateLastPost" $ do
            it "update" $ \cxt -> do
                let ?cxt = cxt
                graph <- newBot >>= do \b -> restoreGraph (fst $ b +< (newGraph :: BotGraph))
                                                          (Proxy :: Proxy (Serialize BotGraph))

                let bot = (values graph :: [Bot]) !! 0
                topicPost >>= updateLastPost bot 

                graph <- selectNodes (Proxy :: Proxy BotGraph)
                                     (Proxy :: Proxy Bot)
                                     ((==?) @Bot "id" (view #id $ getRecord bot))
                                     (../)
                                     Nothing
                let bs = values graph :: [Bot]
                length bs `shouldBe` 1
                view #last_post (getRecord $ bs !! 0) `shouldBe` 1000

prepareTestDB :: IO ()
prepareTestDB = do
    conn <- connectPostgreSQL "postgresql://postgres:postgres@127.0.0.1:15432/chatbot_template"
    execute_ conn "DROP DATABASE IF EXISTS chatbot_test"
    execute_ conn "CREATE DATABASE chatbot_test TEMPLATE chatbot_template"
    return ()

wrapAction :: ActionWith (Contexts '[IORef (DBContext PostgreSQL)]) -> IO ()
wrapAction action = do
    r <- newResource $ PostgreSQL "postgresql://postgres:postgres@127.0.0.1:15432/chatbot_test" 10
    withContext @'[DBContext PostgreSQL] (r `RCons` RNil) $ do
        execAction (action ?cxt)
    return ()

execAction :: (With '[DBContext PostgreSQL])
           => IO ()
           -> IO ()
execAction action = do
    action
    modifyIORef (contextOf @(DBContext PostgreSQL) ?cxt) $ \(DBContext {..}) -> DBContext { status = False, .. }

newBot :: IO Bot
newBot = do
    now <- getCurrentTime
    return $ Model
           ( #id @= 0
          <: #token @= "token"
          <: #topic_id @= 1
          <: #last_post @= 0
          <: #created_at @= now
          <: #modified_at @= now
          <: emptyRecord
           )

topicPost :: IO TopicPost
topicPost = do
    now <- getCurrentTime
    return ( #id @= 1000
          <: #topicId @= 1
          <: #replyTo @= Nothing
          <: #message @= ""
          <: #account @= undefined
          <: #likes @= []
          <: #talks @= []
          <: #createdAt @= now
          <: #updatedAt @= now
          <: emptyRecord
           )