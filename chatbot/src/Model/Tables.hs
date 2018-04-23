{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}

module Model.Tables where

import Control.Lens hiding ((:>))
import Data.Time
import Data.Extensible
import Database.ORM
import Database.ORM.Dialect.PostgreSQL

generateModel (PostgreSQL "postgresql://postgres:postgres@127.0.0.1:15432/chatbot" 10) "bot" "BotCols"

type Bot = "bot" :++ Record BotCols

--newBot :: IO Bot
--newBot = do
--    now <- getCurrentTime
--    return $ Model
--           ( #id @= 1
--          <: #token @= "token"
--          <: #topic_id @= 1000
--          <: #last_post @= 1000
--          <: #created_at @= now
--          <: #modified_at @= now
--          <: emptyRecord
--           )