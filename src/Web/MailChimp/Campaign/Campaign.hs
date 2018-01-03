{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Web.MailChimp.Campaign.Campaign (
  ) where

import           Data.Aeson
import qualified Data.Aeson             as Aeson
import           Data.Text              (Text)
import           Generics.SOP
import           GHC.Generics
import           Servant.API
import           Servant.Client
import           Servant.Client.Generic

import           Web.MailChimp.Common

type CampaignApi =
    ReqBody '[JSON] CampaignCreateRequest
      :> Post '[JSON] CampaignCreateResponse

data CampaignClient =
  CampaignClient {
    addCampaign :: CampaignCreateRequest -> ClientM CampaignCreateResponse
} deriving (GHC.Generics.Generic)

instance Generics.SOP.Generic CampaignClient

instance (Client CampaignApi ~ client) => ClientLike client CampaignClient

data CampaignType = Regular | Plain | ABSplit | Rss | Variate
  deriving (Show)

instance ToJSON CampaignType where
  toJSON Regular = "regular"
  toJSON Plain   = "plaintext"
  toJSON ABSplit = "absplit"
  toJSON Rss     = "rss"
  toJSON Variate = "variate"


data CampaignCreateRequest = CampaignCreateRequest {
  campaignType :: CampaignType,
  listId       :: Text,
  subjectLine  :: Text,
  fromName     :: Text,
  replyTo      :: Text
} deriving (Show)

instance ToJSON CampaignCreateRequest where
  toJSON CampaignCreateRequest {..} =
    Aeson.object $
      ["type" .= campaignType]

data CampaignCreateResponse = CampaignCreateResponse {

} deriving (Show)
