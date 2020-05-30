{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Group
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Slack.Group
  ( Group(..)
  , fromConversation
  , ListRsp(..)
  )
  where

-- aeson
import Data.Aeson.TH

-- base
import GHC.Generics (Generic)

-- slack-web
import Web.Slack.Common
import Web.Slack.Conversation (Conversation(..))
import Web.Slack.Util

-- text
import Data.Text (Text)

-- time
import Data.Time.Clock.POSIX

data Group =
  Group
    { groupId :: Text
    , groupName :: Text
    , groupIsMpim :: Bool
    , groupCreated :: POSIXTime
    , groupCreator :: UserId
    , groupIsArchived :: Bool
    , groupMembers :: [UserId]
    }
  deriving (Eq, Generic, Show)

fromConversation :: Conversation -> Maybe Group
fromConversation conversation =
  if conversationIsPrivate conversation == Just True then do
    groupName <- conversationName conversation
    groupIsMpim <- conversationIsMpim conversation
    groupCreated <- fromInteger <$> conversationCreated conversation
    groupCreator <- conversationCreator conversation
    groupIsArchived <- conversationIsArchived conversation
    pure $ Group {..}
  else
    Nothing
  where
    groupId = conversationId conversation
    groupMembers = [] -- please use conversations.members api

$(deriveFromJSON (jsonOpts "group") ''Group)

data ListRsp =
  ListRsp
    { listRspGroups :: [Group]
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "listRsp") ''ListRsp)
