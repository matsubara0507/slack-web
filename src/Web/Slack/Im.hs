{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

----------------------------------------------------------------------
-- |
-- Module: Web.Slack.Im
-- Description:
--
--
--
----------------------------------------------------------------------

module Web.Slack.Im
  ( Im(..)
  , fromConversation
  , ListRsp(..)
  )
  where

-- aeson
import Data.Aeson.TH

-- base
import GHC.Generics (Generic)

-- slack-web
import Web.Slack.Util
import Web.Slack.Common
import Web.Slack.Conversation (Conversation(..))

-- text
import Data.Text (Text)

-- time
import Data.Time.Clock.POSIX

data Im =
  Im
    { imId :: Text
    , imIsIm :: Bool
    , imUser :: UserId
    , imCreated :: POSIXTime
    , imIsUserDeleted :: Bool
    }
  deriving (Eq, Generic, Show)

fromConversation :: Conversation -> Maybe Im
fromConversation conversation =
  if conversationIsIm conversation == Just True then do
    imIsIm <- conversationIsIm conversation
    imUser <- conversationUser conversation
    imCreated <- fromInteger <$> conversationCreated conversation
    imIsUserDeleted <- conversationIsUserDeleted conversation
    pure $ Im {..}
  else
    Nothing
  where
    imId = conversationId conversation

$(deriveFromJSON (jsonOpts "im") ''Im)

data ListRsp =
  ListRsp
    { listRspIms :: [Im]
    }
  deriving (Eq, Generic, Show)

$(deriveFromJSON (jsonOpts "listRsp") ''ListRsp)
