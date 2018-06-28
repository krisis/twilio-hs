module Twilio.API where

import qualified Data.Aeson          as A
import qualified Network.HTTP.Simple as H

import           Lib.Prelude

import           Twilio.Types

-- List users
listUsers :: TwilioM (TwilioList TwilioUser)
listUsers = do
    req <- mkTwilioGetReq "Users"
    users <- liftIO $ httpJSON' req
    return users

listChannels :: TwilioM (TwilioList TwilioChannel)
listChannels = do
    req <- mkTwilioGetReq "Channels"
    channels <- liftIO $ httpJSON' req
    return channels

getService :: TwilioM ()
getService = do
    req <- mkTwilioGetReq ""
    val <- liftIO $ httpJSON' req
    print (val :: A.Value)


createUser :: Text -> TwilioM TwilioUser
createUser userName = do
    let user = TwilioUserReq userName Nothing Nothing
    req <- mkTwilioPostReq "Users" user
    liftIO $ httpJSON' req


createChannel :: Text -> TwilioM TwilioChannel
createChannel channelName = do
    let channel = defaultChannelReq { tcrFriendlyName = Just channelName}
    req <- mkTwilioPostReq "Channels" channel
    liftIO $ httpJSON' req


data TwilioError = TwilioInvalidRsp Text
                 deriving (Show)

instance Exception TwilioError

httpJSON' :: (MonadIO m, A.FromJSON a) => H.Request -> m a
httpJSON' req = do
    rsp <- H.httpLbs req
    let body = H.getResponseBody rsp
    either
      (throwIO . TwilioInvalidRsp . show)
      return
      (A.eitherDecode body)
