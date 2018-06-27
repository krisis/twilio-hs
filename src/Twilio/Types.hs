{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Twilio.Types where

import           Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import           Data.Aeson                (FromJSON, Value, parseJSON,
                                            withObject, (.:))
import qualified Data.Text                 as T
import           Data.Time                 (UTCTime)
import qualified Network.HTTP.Conduit      as NC
import qualified Network.HTTP.Simple       as H
import qualified Network.HTTP.Types        as HT
import qualified System.Environment        as Env

import           Lib.Prelude


data TwilioCreds = TwilioCreds
                   { tcAcctSID :: Text
                   , tcToken   :: Text
                   , tcSID     :: Text
                   } deriving (Eq, Show)

newtype TwilioM a = TwilioM
                 { unTwilioM :: ReaderT TwilioCreds IO a
                 } deriving ( Functor, Applicative, Monad
                            , MonadReader TwilioCreds, MonadIO)

runTwilioM :: TwilioCreds -> TwilioM a -> IO a
runTwilioM tc m = flip runReaderT tc $ unTwilioM m

loadTwilioCreds :: IO (Maybe TwilioCreds)
loadTwilioCreds = runMaybeT $ do
    accId <- MaybeT $ Env.lookupEnv "TWILIO_ACCOUNT_ID"
    tok <- MaybeT $ Env.lookupEnv "TWILIO_AUTH_TOKEN"
    sId <- MaybeT $ Env.lookupEnv "TWILIO_SID"
    return $ TwilioCreds (T.pack accId) (T.pack tok) (T.pack sId)

twilioBaseRequest :: NC.Request
twilioBaseRequest = NC.parseRequest_ "https://chat.twilio.com/"

twilioBaseAPI :: Text
twilioBaseAPI = "v2/Services"

mkTwilioGetReq :: Text -> TwilioM NC.Request
mkTwilioGetReq apiPath = do
    TwilioCreds accId tok sid <- ask
    let resource = toS $ T.intercalate "/"
                   $ [twilioBaseAPI, sid, apiPath]
    return $ H.setRequestMethod HT.methodGet
      $ H.setRequestBasicAuth (toS accId) (toS tok)
      $ H.setRequestPath resource
      $ twilioBaseRequest

mkTwilioPostReq :: (ToUrlEncoded a) => Text -> a -> TwilioM NC.Request
mkTwilioPostReq apiPath params = do
    TwilioCreds accId tok sid <- ask
    let resource = toS $ T.intercalate "/"
                   $ [twilioBaseAPI, sid, apiPath]

    return $ H.setRequestMethod HT.methodPost
      $ H.setRequestBasicAuth (toS accId) (toS tok)
      $ H.setRequestPath resource
      $ H.setRequestBodyURLEncoded (toUrlEncode params)
      $ twilioBaseRequest

data TwilioUser = TwilioUser
                  { tuIdentity            :: Text
                  , tuAttrs               :: Maybe Value
                  , tuFriendlyName        :: Maybe Text
                  , tuRoleSid             :: Maybe Text
                  , tuDateCreated         :: UTCTime
                  , tuDateUpdated         :: UTCTime
                  , tuIsNotifiable        :: Maybe Bool
                  , tuIsOnline            :: Maybe Bool
                  , tuJoinedChannelsCount :: Int
                  , tuServiceSid          :: Text
                  , tuSid                 :: Text
                  , tuUrl                 :: Text
                  } deriving (Eq, Show)

instance FromJSON TwilioUser where
    parseJSON = withObject "TwilioUser" $ \v -> TwilioUser
        <$> v .: "identity"
        <*> v .: "attributes"
        <*> v .: "friendly_name"
        <*> v .: "role_sid"
        <*> v .: "date_created"
        <*> v .: "date_updated"
        <*> v .: "is_notifiable"
        <*> v .: "is_online"
        <*> v .: "joined_channels_count"
        <*> v .: "service_sid"
        <*> v .: "sid"
        <*> v .: "url"

data TwilioChannel = TwilioChannel
                     { tcAccountSid    :: Text
                     , tcSid           :: Text
                     , tcAttrs         :: Maybe Value
                     , tcFriendlyName  :: Maybe Text
                     , tcCreatedBy     :: Text
                     , tcDateCreated   :: UTCTime
                     , tcDateUpdate    :: UTCTime
                     , tcMembersCount  :: Int
                     , tcMessagesCount :: Int
                     , tcUniqueName    :: Maybe Text
                     , tcURL           :: Text
                     } deriving (Eq, Show)

instance FromJSON TwilioChannel where
    parseJSON = withObject "TwilioChannel" $ \v -> TwilioChannel
        <$> v .: "account_sid"
        <*> v .: "sid"
        <*> v .: "attributes"
        <*> v .: "friendly_name"
        <*> v .: "created_by"
        <*> v .: "date_created"
        <*> v .: "date_updated"
        <*> v .: "members_count"
        <*> v .: "messages_count"
        <*> v .: "unique_name"
        <*> v .: "url"


data TwilioUserReq = TwilioUserReq
                     { turIdentity     :: Text
                     , turRoleSid      :: Maybe Text
                     , turFriendlyName :: Maybe Text
                     } deriving (Eq, Show)

class ToUrlEncoded a where
    toUrlEncode :: a -> [(ByteString, ByteString)]

instance ToUrlEncoded TwilioUserReq where
    toUrlEncode v = [ ("Identity", toS $ turIdentity v) ]
                    ++ maybe [] (\role -> [("RoleSid", toS role )]) (turRoleSid v)
                    ++ maybe [] (\fname -> [("FriendlyName", toS fname )]) (turFriendlyName v)

data TwilioMeta = TwilioMeta
                  { tmNextPgUrl :: Maybe Text
                  } deriving (Eq, Show)

instance FromJSON TwilioMeta where
    parseJSON = withObject "TwilioMeta" $ \v -> TwilioMeta
        <$> v .: "next_page_url"

data TwilioList a = TwilioList { tlData :: [a]
                               , tlMeta :: TwilioMeta
                               } deriving (Eq, Show)


instance FromJSON (TwilioList TwilioUser) where
    parseJSON = withObject "TwilioList" $ \v -> TwilioList
        <$> v .: "users"
        <*> v .: "meta"

instance FromJSON (TwilioList TwilioChannel) where
    parseJSON = withObject "TwilioList" $ \v -> TwilioList
        <$> v .: "channels"
        <*> v .: "meta"
