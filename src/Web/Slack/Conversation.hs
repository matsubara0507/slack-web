{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Conversation
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Slack.Conversation
    ( Conversation (..)
    , Purpose(..)
    , Topic(..)
    , CreateReq(..)
    , mkCreateReq
    , CreateRsp(..)
    , ListReq(..)
    , mkListReq
    , ListRsp(..)
    , ListRspMetadata(..)
    ) where

-- aeson
import Data.Aeson.TH

-- base
import GHC.Generics (Generic)

-- http-api-data
import Web.FormUrlEncoded

-- slack-web
import Web.Slack.Common
import Web.Slack.Util

-- text
import Data.Text (Text)

-- time
import Data.Time.Clock.POSIX


-- |
--
--

data Conversation =
  Conversation
    { conversationId :: Text
    , conversationName :: Maybe Text
    , conversationIsChannel :: Maybe Bool
    , conversationIsGroup :: Maybe Bool
    , conversationIsIm :: Maybe Bool
    , conversationCreated :: Maybe POSIXTime
    , conversationcreator :: Maybe UserId
    , conversationIsArchived :: Maybe Bool
    , conversationIsGeneral :: Maybe Bool
    , conversationUnlinked :: Maybe Int
    , conversationNameNormalized :: Maybe Text
    , conversationIsReadOnly :: Maybe Bool
    , conversationIsShared :: Maybe Bool
    , conversationIsExtShared :: Maybe Bool
    , conversationIsOrgShared :: Maybe Bool
    , conversationPendingShared :: Maybe [Text]
    , conversationIsPendingExtShared :: Maybe Bool
    , conversationIsMember :: Maybe Bool
    , conversationIsPrivate :: Maybe Bool
    , conversationIsMpim :: Maybe Bool
    , conversationLastRead :: Maybe Text
    , conversationTopic :: Maybe Topic
    , conversationPurpose :: Maybe Purpose
    , conversationPreviousNames :: Maybe [Text]
    , conversationNumMembers :: Maybe Int
    , conversationLocale :: Maybe Text
    , conversationUser :: Maybe UserId
    , conversationIsUserDeleted :: Maybe Bool
    }
  deriving (Eq, Generic, Show)


-- |
--
--

data Purpose =
  Purpose
    { purposeValue :: Text
    , purposeCreator :: Text
    , purposeLastSet :: Integer
    }
  deriving (Eq, Generic, Show)


-- |
--
--

data Topic =
  Topic
    { topicValue :: Text
    , topicCreator :: Text
    , topicLastSet :: Integer
    }
  deriving (Eq, Generic, Show)


-- |
--
--

$(deriveFromJSON (jsonOpts "conversation") ''Conversation)


-- |
--
--

$(deriveJSON (jsonOpts "purpose") ''Purpose)


-- |
--
--

$(deriveJSON (jsonOpts "topic") ''Topic)


-- |
--
--

data CreateReq =
  CreateReq
    { createReqName :: Text
    , createReqIsPrivate :: Maybe Bool
    , createReqUserIds :: Maybe Text -- ^ e.g. "W1234567890,U2345678901,U3456789012"
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
    , createReqIsPrivate = Nothing
    , createReqUserIds = Nothing
    }


-- |
--
--

data CreateRsp =
  CreateRsp
    { createRspChannel :: Conversation
    }
  deriving (Eq, Generic, Show)


-- |
--
--
$(deriveFromJSON (jsonOpts "createRsp") ''CreateRsp)

data ListReq =
  ListReq
    { listReqCursor :: Maybe Text
    , listReqExcludeArchived :: Maybe Bool
    , listReqLimit :: Maybe Int
    , listReqTypes :: Maybe Text -- ^ e.g. "public_channel,private_channel,mpim,im"
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
    { listReqCursor = Nothing
    , listReqExcludeArchived = Nothing
    , listReqLimit = Nothing
    , listReqTypes = Nothing
    }


-- |
--
--

data ListRspMetadata =
  ListRspMetadata
    { listRspMetadataNextCursor :: Text
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "listRspMetadata") ''ListRspMetadata)


-- |
--
--

data ListRsp =
  ListRsp
    { listRspChannels :: [Conversation]
    , listRspResponseMetadata :: ListRspMetadata
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "listRsp") ''ListRsp)
