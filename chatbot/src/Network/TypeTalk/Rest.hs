{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Network.TypeTalk.Rest where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.IORef
import Data.ByteString
import Data.String
import Data.CaseInsensitive
import Data.Aeson
import Network.HTTP.Conduit
import Network.HTTP.Simple
import Data.Resource

typeTalkApiUrl :: String
               -> String
typeTalkApiUrl path = "https://typetalk.com/api" ++ path

authenticationHeader :: ByteString
authenticationHeader = "X-Typetalk-Token"

data TypeTalkBot = TypeTalkBot String Int
data TypeTalkBotC = TypeTalkBotC TypeTalkBot

instance Resource TypeTalkBot where
    type ContextType TypeTalkBot = TypeTalkBotC

    newContext r = do
        bot <- liftIO $ readIORef r
        liftIO $ newIORef (TypeTalkBotC bot)

instance ResourceContext TypeTalkBotC where
    type ResourceType TypeTalkBotC = TypeTalkBot

    closeContext c _ = return c
    execContext cr f = f

makeRequest :: (MonadThrow m)
            => TypeTalkBot
            -> String
            -> m Request
makeRequest (TypeTalkBot token _) path =
    setRequestHeader (mk authenticationHeader) [fromString token]
        <$> parseRequest (typeTalkApiUrl path)

get :: forall a m. (MonadIO m, MonadThrow m, FromJSON a)
    => TypeTalkBotC
    -> String
    -> Query
    -> m a
get (TypeTalkBotC bot) path q = do
    r <- setRequestQueryString q <$> makeRequest bot path
    getResponseBody <$> httpJSON (r { method = "GET" })
