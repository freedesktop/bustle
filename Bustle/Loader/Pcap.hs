{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
{-
Bustle.Loader.Pcap: loads logs out of pcap files
Copyright © 2011–2012 Collabora Ltd.
Copyright © 2017–2018 Will Thompson

This library is free software; you can redistribute it and/or
modify it under the terms of the GNU Lesser General Public
License as published by the Free Software Foundation; either
version 2.1 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public
License along with this library; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}
{-# LANGUAGE PatternGuards, FlexibleContexts #-}
module Bustle.Loader.Pcap
  ( readPcap

  , convert
  )
where

import Data.Maybe (fromMaybe)
import Data.Either (partitionEithers)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Exception (try)
import Control.Monad.State
import System.IO.Error ( mkIOError
                       , userErrorType
                       )

import Network.Pcap

import DBus

import qualified Data.ByteString as BS

import qualified Bustle.Types as B

-- Conversions from dbus-core's types into Bustle's more stupid types. This
-- whole section is pretty upsetting.
stupifyBusName :: BusName
               -> B.TaggedBusName
stupifyBusName n
    | isUnique n = B.U $ B.UniqueName n
    | otherwise  = B.O $ B.OtherName n

isUnique :: BusName -> Bool
isUnique n = head (formatBusName n) == ':'

convertBusName :: String
               -> Maybe BusName
               -> B.TaggedBusName
convertBusName fallback n =
    stupifyBusName (fromMaybe fallback_ n)
  where
    fallback_ = busName_ fallback

convertMember :: (a -> ObjectPath)
              -> (a -> Maybe InterfaceName)
              -> (a -> MemberName)
              -> a
              -> B.Member
convertMember getObjectPath getInterfaceName getMemberName m =
    B.Member (getObjectPath m)
             (getInterfaceName m)
             (getMemberName m)

type PendingMessages = Map (Maybe BusName, Serial)
                           (MethodCall, B.Detailed B.Message)

popMatchingCall :: (MonadState PendingMessages m)
                => Maybe BusName
                -> Serial
                -> m (Maybe (MethodCall, B.Detailed B.Message))
popMatchingCall name serial = do
    ret <- tryPop (name, serial)
    case (ret, name) of
        -- If we don't get an answer, but we know a destination, this may be
        -- because we didn't know the sender's bus name because it was the
        -- logger itself. So try looking up pending replies whose sender is
        -- Nothing.
        (Nothing, Just _) -> tryPop (Nothing, serial)
        _                 -> return ret
  where
    tryPop key = do
        call <- gets $ Map.lookup key
        modify $ Map.delete key
        return call

insertPending :: MonadState PendingMessages m
              => Maybe BusName
              -> Serial
              -> MethodCall
              -> B.Detailed B.Message
              -> m ()
insertPending n s rawCall b = modify $ Map.insert (n, s) (rawCall, b)

isNOC :: Maybe BusName -> Signal -> Maybe (BusName, Maybe BusName, Maybe BusName)
isNOC (Just sender) s | looksLikeNOC =
    case names of
        [Just n, old, new] -> Just (n, old, new)
        _                  -> Nothing
  where
    names :: [Maybe BusName]
    names = map fromVariant $ signalBody s

    looksLikeNOC =
      (sender == B.dbusName) &&
        (signalInterface s == B.dbusInterface) &&
          (formatMemberName (signalMember s) == "NameOwnerChanged")

isNOC _ _ = Nothing


bustlifyNOC :: (BusName, Maybe BusName, Maybe BusName)
            -> B.NOC
bustlifyNOC ns@(name, oldOwner, newOwner)
    | isUnique name =
          case (oldOwner, newOwner) of
              (Nothing, Just _) -> B.Connected (uniquify name)
              (Just _, Nothing) -> B.Disconnected (uniquify name)
              _                 -> error $ "wtf: NOC" ++ show ns
    | otherwise = B.NameChanged (otherify name) $
          case (oldOwner, newOwner) of
              (Just old, Nothing)  -> B.Released (uniquify old)
              (Just old, Just new) -> B.Stolen (uniquify old) (uniquify new)
              (Nothing, Just new)  -> B.Claimed (uniquify new)
              (Nothing, Nothing)   -> error $ "wtf: NOC" ++ show ns
  where
    uniquify = B.UniqueName
    otherify = B.OtherName

tryBustlifyGetNameOwnerReply :: Maybe (MethodCall, a)
                             -> MethodReturn
                             -> Maybe B.NOC
tryBustlifyGetNameOwnerReply maybeCall mr = do
    -- FIXME: obviously this should be more robust:
    --  • check that the service really is the bus daemon
    --  • don't crash if the body of the call or reply doesn't contain one bus name.
    (rawCall, _) <- maybeCall
    guard (formatMemberName (methodCallMember rawCall) == "GetNameOwner")
    ownedName <- fromVariant (head (methodCallBody rawCall))
    return $ bustlifyNOC ( ownedName
                         , Nothing
                         , fromVariant (head (methodReturnBody mr))
                         )

bustlify :: MonadState PendingMessages m
         => B.Microseconds
         -> Int
         -> ReceivedMessage
         -> m B.DetailedEvent
bustlify µs bytes m = do
    bm <- buildBustledMessage
    return $ B.Detailed µs bm bytes m
  where
    sender = receivedMessageSender m
    -- FIXME: can we do away with the un-Maybe-ing and just push that Nothing
    -- means 'the monitor' downwards? Or skip the message if sender is Nothing.
    wrappedSender = convertBusName "sen.der" sender

    buildBustledMessage = case m of
        (ReceivedMethodCall serial mc) -> do
            let call = B.MethodCall
                             { B.serial = serialValue serial
                             , B.sender = wrappedSender
                             , B.destination = convertBusName "method.call.destination" $ methodCallDestination mc
                             , B.member = convertMember methodCallPath methodCallInterface methodCallMember mc
                             }
            -- FIXME: we shouldn't need to construct almost the same thing here
            -- and 10 lines above maybe?
            insertPending sender serial mc (B.Detailed µs call bytes m)
            return $ B.MessageEvent call

        (ReceivedMethodReturn _serial mr) -> do
            call <- popMatchingCall (methodReturnDestination mr) (methodReturnSerial mr)

            return $ case tryBustlifyGetNameOwnerReply call mr of
                Just noc -> B.NOCEvent noc
                Nothing  -> B.MessageEvent $ B.MethodReturn
                               { B.inReplyTo = fmap snd call
                               , B.sender = wrappedSender
                               , B.destination = convertBusName "method.return.destination" $ methodReturnDestination mr
                               }

        (ReceivedMethodError _serial e) -> do
            call <- popMatchingCall (methodErrorDestination e) (methodErrorSerial e)
            return $ B.MessageEvent $ B.Error
                        { B.inReplyTo = fmap snd call
                        , B.sender = wrappedSender
                        , B.destination = convertBusName "method.error.destination" $ methodErrorDestination e
                        }

        (ReceivedSignal _serial sig)
            | Just names <- isNOC sender sig -> return $ B.NOCEvent $ bustlifyNOC names
            | otherwise                      -> return $ B.MessageEvent $
                B.Signal { B.sender = wrappedSender
                         , B.member = convertMember signalPath (Just . signalInterface) signalMember sig
                         , B.signalDestination = stupifyBusName <$> signalDestination sig
                         }

        _ -> error "woah there! someone added a new message type."

convert :: MonadState PendingMessages m
        => B.Microseconds
        -> BS.ByteString
        -> m (Either String B.DetailedEvent)
convert µs body =
    case unmarshal body of
        Left e  -> return $ Left $ unmarshalErrorMessage e
        Right m -> Right <$> bustlify µs (BS.length body) m

data Result e a =
    EOF
  | Packet (Either e a)
  deriving Show

readOne :: (MonadState s m, MonadIO m)
        => PcapHandle
        -> (B.Microseconds -> BS.ByteString -> m (Either e a))
        -> m (Result e a)
readOne p f = do
    (hdr, body) <- liftIO $ nextBS p
    -- No really, nextBS just returns null packets when you hit the end of the
    -- file.
    --
    -- It occurs to me that we could stream by just polling this every second
    -- or something?
    if hdrCaptureLength hdr == 0
        then return EOF
        else Packet <$> f (fromIntegral (hdrTime hdr)) body

-- This shows up as the biggest thing on the heap profile. Which is kind of a
-- surprise. It's supposedly the list.
mapBodies :: (MonadState s m, MonadIO m)
          => PcapHandle
          -> (B.Microseconds -> BS.ByteString -> m (Either e a))
          -> m [Either e a]
mapBodies p f = do
    ret <- readOne p f
    case ret of
        EOF      -> return []
        Packet x -> do
            xs <- mapBodies p f
            return $ x:xs

readPcap :: MonadIO m
         => FilePath
         -> m (Either IOError ([String], [B.DetailedEvent]))
readPcap path = liftIO $ try $ do
    p <- openOffline path
    dlt <- datalink p
    -- DLT_NULL for extremely old logs.
    -- DLT_DBUS is missing: https://github.com/bos/pcap/pull/8
    unless (dlt `elem` [DLT_NULL, DLT_UNKNOWN 231]) $ do
        let message = "Incorrect link type " ++ show dlt
        ioError $ mkIOError userErrorType message Nothing (Just path)

    partitionEithers <$> evalStateT (mapBodies p convert) Map.empty
