{-# LANGUAGE OverloadedLabels #-}

module Network.TypeTalkSpec where

import Test.Hspec
import Data.IORef
import Data.Maybe (maybe)
import System.Environment
import Control.Lens
import Data.Resource
import Network.TypeTalk

spec :: Spec
spec = do
    ttb@(TypeTalkBot token topicId) <- runIO $
        TypeTalkBot <$> (maybe "" id <$> lookupEnv "CHATBOT_BOT_TOKEN")
                    <*> (maybe 0 read <$> lookupEnv "CHATBOT_BOT_TOPIC")

    r <- runIO $ newIORef ttb

    describe "topicMessages" $ do
        it "should succeed" $ do
            withContext @'[TypeTalkBotC] (r `RCons` RNil) $ do
                messages <- topicMessages Nothing Nothing False
                view #id (view #topic messages) `shouldBe` topicId
            return ()