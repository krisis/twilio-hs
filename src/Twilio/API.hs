module Twilio.API where

import qualified Data.Aeson          as A
import qualified Network.HTTP.Simple as H


import           Lib.Prelude

import           Twilio.Types

-- List users
listUsers :: TwilioM (TwilioList TwilioUser)
listUsers = do
    req <- mkTwilioGetReq "Users"
    rsp <- H.httpJSON req
    return $ H.getResponseBody rsp

listChannels :: TwilioM (TwilioList TwilioChannel)
listChannels = do
    req <- mkTwilioGetReq "Channels"
    rsp <- H.httpJSON req
    return $ H.getResponseBody rsp

getService :: TwilioM ()
getService = do
    req <- mkTwilioGetReq ""
    rsp <- H.httpJSON req
    let val = H.getResponseBody rsp
    print (val :: A.Value)


createUser :: Text -> TwilioM TwilioUser
createUser userName = do
    let user = TwilioUserReq userName Nothing Nothing
    req <- mkTwilioPostReq "Users" user
    rsp <- H.httpJSON req
    return $ H.getResponseBody rsp


createChannel :: Text -> TwilioM TwilioChannel
createChannel channelName = do
    let channel = defaultChannelReq { tcrFriendlyName = Just channelName}
    req <- mkTwilioPostReq "Channels" channel
    rsp <- H.httpJSON req
    return $ H.getResponseBody rsp
