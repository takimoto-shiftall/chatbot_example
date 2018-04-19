module Model.Indices where

import Data.Time
import Data.Extensible

type Post = Record '[
    "post_id" >: Integer
  , "topic_id" >: Integer
  , "message" >: String
  , "account_id" >: Int
  , "reply_to" >: Maybe Int
  , "likes" >: [Int]
  , "talks" >: [Int]
  , "created_at" >: UTCTime
  , "modified_at" >: UTCTime
  ]