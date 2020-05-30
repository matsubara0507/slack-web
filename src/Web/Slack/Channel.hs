{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Channel
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Slack.Channel
  ( Channel(..)
  , fromConversation
  , Purpose(..)
  , Topic(..)
  , CreateReq(..)
  , mkCreateReq
  , CreateRsp(..)
  , ListReq(..)
  , mkListReq
  , ListRsp(..)
  )
  where

-- aeson
import Data.Aeson.TH

-- base
import GHC.Generics (Generic)

-- http-api-data
import Web.FormUrlEncoded

-- slack-web
import Web.Slack.Common
import Web.Slack.Conversation (Conversation(..))
import Web.Slack.Util

-- text
import Data.Text (Text)

-- re-export
import Web.Slack.Conversation (Purpose (..), Topic (..))


-- |
--
--

data Channel =
  Channel
    { channelId :: Text
    , channelName :: Text
    , channelCreated :: Integer
    , channelCreator :: UserId
    , channelIsArchived :: Bool
    , channelIsMember :: Bool
    , channelIsGeneral :: Bool
    , channelLastRead :: Maybe Text
    , channelLatest :: Maybe Text
    , channelUnreadCount :: Maybe Integer
    , channelUnreadCountDisplay :: Maybe Integer
    , channelMembers :: [UserId]
    , channelTopic :: Topic
    , channelPurpose :: Purpose
    }
  deriving (Eq, Generic, Show)


fromConversation :: Conversation -> Maybe Channel
fromConversation conversation =
  if conversationIsChannel conversation == Just True then do
    channelName <- conversationName conversation
    channelCreated <- conversationCreated conversation
    channelCreator <- conversationCreator conversation
    channelIsArchived <- conversationIsArchived conversation
    channelIsMember <- conversationIsMember conversation
    channelIsGeneral <- conversationIsGeneral conversation
    channelTopic <- conversationTopic conversation
    channelPurpose <- conversationPurpose conversation
    pure $ Channel {..}
  else
    Nothing
  where
    channelId = conversationId conversation
    channelLastRead = conversationLastRead conversation
    channelLatest = conversationLatest conversation
    channelUnreadCount = conversationUnreadCount conversation
    channelUnreadCountDisplay = conversationUnreadCountDisplay conversation
    channelMembers = [] -- please use conversations.members api


-- |
--
--

$(deriveFromJSON (jsonOpts "channel") ''Channel)


-- |
--
--

data CreateReq =
  CreateReq
    { createReqName :: Text
    , createReqValidate :: Maybe Bool
    }
  deriving (Eq, Generic, Show)


-- |
--
--

$(deriveJSON (jsonOpts "createReq") ''CreateReq)


-- |
--
--

instance ToForm CreateReq where
  toForm =
    genericToForm (formOpts "createReq")


-- |
--
--

mkCreateReq
  :: Text
  -> CreateReq
mkCreateReq name =
  CreateReq
    { createReqName = name
    , createReqValidate = Nothing
    }


-- |
--
--

data CreateRsp =
  CreateRsp
    { createRspChannel :: Channel
    }
  deriving (Eq, Generic, Show)


-- |
--
--
$(deriveFromJSON (jsonOpts "createRsp") ''CreateRsp)

data ListReq =
  ListReq
    { listReqExcludeArchived :: Maybe Bool
    , listReqExcludeMembers :: Maybe Bool
    }
  deriving (Eq, Generic, Show)


-- |
--
--

$(deriveJSON (jsonOpts "listReq") ''ListReq)


-- |
--
--

instance ToForm ListReq where
  toForm =
    genericToForm (formOpts "listReq")


-- |
--
--

mkListReq
  :: ListReq
mkListReq =
  ListReq
    { listReqExcludeArchived = Nothing
    , listReqExcludeMembers = Nothing
    }


-- |
--
--

data ListRsp =
  ListRsp
    { listRspChannels :: [Channel]
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "listRsp") ''ListRsp)
