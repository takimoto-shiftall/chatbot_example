{-# LANGUAGE OverloadedLabels #-}

module Network.ElasticsearchSpec where

import Test.Hspec
import Data.IORef
import System.Environment
import Control.Lens
import Data.Resource
import Network.TypeTalk

spec :: Spec
spec = do
    describe "topicMessages" $ do
        it "should succeed" $ do
            token <- getEnv "CHATBOT_BOT_TOKEN"
            topicId <- read <$> getEnv "CHATBOT_BOT_TOPIC"

            r <- newIORef $ TypeTalkBot token topicId
            withContext @'[TypeTalkBotC] (r `RCons` RNil) $ do
                messages <- topicMessages Nothing Nothing False
                view #id (view #topic messages) `shouldBe` topicId