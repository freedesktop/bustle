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
import Data.List (isSuffixOf)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Exception (try, tryJust)
import Control.Monad.State
import System.IO.Error ( mkIOError
                       , userErrorType
                       , isUserError
                       , ioeGetErrorString
                       , ioeSetErrorString
                       )

import Network.Pcap

import DBus

import qualified Data.ByteString as BS

import qualified Bustle.Types as B
import Bustle.Translation (__)


convertBusName :: String
               -> Maybe BusName
               -> B.TaggedBusName
convertBusName fallback n =
    B.tagBusName (fromMaybe fallback_ n)
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

insertPending :: (MonadState PendingMessages m)
              => Maybe BusName
              -> Serial
              -> MethodCall
              -> B.Detailed B.Message
              -> m ()
insertPending n s rawCall b = modify $ Map.insert (n, s) (rawCall, b)

bustlify :: Monad m
         => B.Microseconds
         -> Int
         -> ReceivedMessage
         -> StateT PendingMessages m B.DetailedEvent
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
            return call

        (ReceivedMethodReturn _serial mr) -> do
            call <- popMatchingCall (methodReturnDestination mr) (methodReturnSerial mr)

            return $  B.MethodReturn
                               { B.inReplyTo = fmap snd call
                               , B.sender = wrappedSender
                               , B.destination = convertBusName "method.return.destination" $ methodReturnDestination mr
                               }

        (ReceivedMethodError _serial e) -> do
            call <- popMatchingCall (methodErrorDestination e) (methodErrorSerial e)
            return $  B.Error
                        { B.inReplyTo = fmap snd call
                        , B.sender = wrappedSender
                        , B.destination = convertBusName "method.error.destination" $ methodErrorDestination e
                        }

        (ReceivedSignal _serial sig) -> return $
                B.Signal { B.sender = wrappedSender
                         , B.member = convertMember signalPath (Just . signalInterface) signalMember sig
                         , B.signalDestination = B.tagBusName <$> signalDestination sig
                         }

        _ -> error "woah there! someone added a new message type."

convert :: Monad m
        => B.Microseconds
        -> BS.ByteString
        -> StateT PendingMessages m (Either String B.DetailedEvent)
convert µs body =
    case unmarshal body of
        Left e  -> return $ Left $ unmarshalErrorMessage e
        Right m -> Right <$> bustlify µs (BS.length body) m

data Result e a =
    EOF
  | Packet (Either e a)
  deriving Show

readOne :: (Monad m, MonadIO m)
        => PcapHandle
        -> (B.Microseconds -> BS.ByteString -> StateT s m (Either e a))
        -> StateT s m (Result e a)
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
mapBodies :: (Monad m, MonadIO m)
          => PcapHandle
          -> (B.Microseconds -> BS.ByteString -> StateT s m (Either e a))
          -> StateT s m [Either e a]
mapBodies p f = do
    ret <- readOne p f
    case ret of
        EOF      -> return []
        Packet x -> do
            xs <- mapBodies p f
            return $ x:xs

readPcap :: FilePath
         -> IO (Either IOError ([String], [B.DetailedEvent]))
readPcap path = try $ do
    p_ <- tryJust matchSnaplenBug $ openOffline path
    p <- either ioError return p_
    dlt <- datalink p
    -- DLT_NULL for extremely old logs.
    -- DLT_DBUS is missing: https://github.com/bos/pcap/pull/8
    unless (dlt `elem` [DLT_NULL, DLT_UNKNOWN 231]) $ do
        let message = "Incorrect link type " ++ show dlt
        ioError $ mkIOError userErrorType message Nothing (Just path)

    partitionEithers <$> evalStateT (mapBodies p convert) Map.empty
  where
    snaplenErrorString = "invalid file capture length 134217728, bigger than maximum of 262144"
    snaplenBugReference = __ "libpcap 1.8.0 and 1.8.1 are incompatible with Bustle. See \
                             \https://bugs.freedesktop.org/show_bug.cgi?id=100220#c7 for \
                             \details. Distributions should apply downstream patches until \
                             \until a new upstream release is made; users should install \
                             \Bustle from Flathub, which already includes the necessary \
                             \patches: https://flathub.org/apps/details/org.freedesktop.Bustle"
    matchSnaplenBug e =
      if isUserError e && (snaplenErrorString `isSuffixOf` ioeGetErrorString e)
          then Just $ ioeSetErrorString e snaplenBugReference
          else Nothing
