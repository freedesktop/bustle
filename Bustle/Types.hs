{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-
Bustle.Types: defines types used by Bustle
Copyright (C) 2008 Collabora Ltd.

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
module Bustle.Types
  ( ObjectPath
  , formatObjectPath

  , InterfaceName
  , formatInterfaceName

  , MemberName
  , formatMemberName

  , Serial

  , UniqueName(..)
  , OtherName(..)
  , TaggedBusName(..)
  , isUnique
  , isOther
  , unUniqueName
  , unOtherName
  , unBusName

  , NameFilter(..)
  , emptyNameFilter

  , dbusName
  , dbusInterface

  , fakeUniqueName

  , Microseconds(..)
  , µsToMs

  , Member(..)
  , Message(..)
  , NOC(..)
  , Event(..)
  , Detailed(..)
  , DetailedEvent
  , Change(..)
  , partitionDetaileds
  , mentionedNames
  , Log
  )
where

import Data.Word (Word32)
import DBus ( ObjectPath, formatObjectPath
            , InterfaceName, formatInterfaceName, interfaceName_
            , MemberName, formatMemberName
            , BusName, formatBusName, busName_
            , ReceivedMessage
            )
import Data.Maybe (maybeToList)
import Data.Either (partitionEithers)
import Data.Set (Set)
import qualified Data.Set as Set

type Serial = Word32

newtype UniqueName = UniqueName BusName
  deriving (Ord, Show, Eq)
newtype OtherName = OtherName BusName
  deriving (Ord, Show, Eq)
data TaggedBusName =
    U UniqueName
  | O OtherName
  deriving (Ord, Show, Eq)

isUnique, isOther :: TaggedBusName -> Bool
isUnique (U _) = True
isUnique (O _) = False
isOther = not . isUnique

unUniqueName :: UniqueName -> String
unUniqueName (UniqueName x) = formatBusName x

unOtherName :: OtherName -> String
unOtherName (OtherName x) = formatBusName x

unBusName :: TaggedBusName -> String
unBusName (U (UniqueName x)) = formatBusName x
unBusName (O (OtherName  x)) = formatBusName x

data NameFilter =
    NameFilter { nfOnly :: Set UniqueName
               , nfNever :: Set UniqueName
               }

emptyNameFilter :: NameFilter
emptyNameFilter = NameFilter Set.empty Set.empty

-- These useful constants disappeared from dbus in the grand removing of the
-- -core suffix.
dbusName :: BusName
dbusName = busName_ "org.freedesktop.DBus"

dbusInterface :: InterfaceName
dbusInterface = interfaceName_ "org.freedesktop.DBus"

-- FIXME: nothing stops someone passing in garbage
-- http://www.youtube.com/watch?v=WorPANO_ANU
fakeUniqueName :: String
               -> UniqueName
fakeUniqueName = UniqueName . busName_ . (":fake." ++)

newtype Microseconds = Microseconds Integer
  deriving (Show, Ord, Eq, Num, Real, Enum, Integral)

µsToMs :: Microseconds
       -> Integer
µsToMs (Microseconds µs) = µs `div` 1000

data Member = Member { path :: ObjectPath
                     , iface :: Maybe InterfaceName
                     , membername :: MemberName
                     }
  deriving (Ord, Show, Eq)

data Event = MessageEvent Message
           | NOCEvent NOC
  deriving (Show, Eq, Ord)

data Message = MethodCall { serial :: Serial
                          , sender :: TaggedBusName
                          , destination :: TaggedBusName
                          , member :: Member
                          }
             | MethodReturn { inReplyTo :: Maybe (Detailed Message)
                            , sender :: TaggedBusName
                            , destination :: TaggedBusName
                            }
             | Signal { sender :: TaggedBusName
                      , signalDestination :: Maybe TaggedBusName
                      , member :: Member
                      }
             | Error { inReplyTo :: Maybe (Detailed Message)
                     , sender :: TaggedBusName
                     , destination :: TaggedBusName
                     }
  deriving (Show, Eq, Ord)

data NOC = Connected { actor :: UniqueName
                     }
         | Disconnected { actor :: UniqueName
                        }
         | NameChanged { changedName :: OtherName
                       , change :: Change
                       }
  deriving (Show, Eq, Ord)

type MessageSize = Int

data Detailed e =
    Detailed { deTimestamp :: Microseconds
             , deEvent :: e
             , deMessageSize :: MessageSize
             , deReceivedMessage :: ReceivedMessage
             }
  deriving (Show, Eq, Functor)

type DetailedEvent = Detailed Event

instance Ord e => Ord (Detailed e) where
    compare (Detailed µs x _ _) (Detailed µs' y _ _)
        = compare (µs, x) (µs', y)

data Change = Claimed UniqueName
            | Stolen UniqueName UniqueName
            | Released UniqueName
  deriving (Show, Eq, Ord)

partitionDetaileds :: [DetailedEvent]
                   -> ([Detailed NOC], [Detailed Message])
partitionDetaileds = partitionEithers . map f
  where
    f (Detailed µs e size rm) =
        case e of
            NOCEvent n -> Left $ Detailed µs n size rm
            MessageEvent m -> Right $ Detailed µs m size rm

mentionedNames :: Message -> [TaggedBusName]
mentionedNames m = sender m:dest
  where
    dest = case m of
        Signal {} -> maybeToList $ signalDestination m
        _         -> [destination m]

type Log = [DetailedEvent]

-- vim: sw=2 sts=2
