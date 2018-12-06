{-
Bustle.UI.DetailsView: displays the bodies of D-Bus messages
Copyright © 2011–2012 Collabora Ltd.

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
module Bustle.UI.DetailsView
  ( DetailsView
  , detailsViewNew
  , detailsViewGetTop
  , detailsViewUpdate
  )
where

import Data.List (intercalate)
import Graphics.UI.Gtk hiding (Signal)

import qualified DBus as D
import DBus.Internal.Message (MethodError(..))
import DBus.Internal.Types (ErrorName(..))

import Bustle.Types
import Bustle.Marquee
import Bustle.VariantFormatter

type OptionalRow = (Label, Label)

data DetailsView =
    DetailsView { detailsGrid :: Grid
                , detailsType :: Stack
                , detailsSender :: Label
                , detailsDestination :: OptionalRow
                , detailsPath :: Label
                , detailsMember :: Label
                , detailsErrorName :: OptionalRow
                , detailsBodyView :: TextView
                }

detailsViewNew :: Builder
               -> IO DetailsView
detailsViewNew builder = DetailsView
    <$> builderGetObject builder castToGrid "detailsGrid"
    <*> builderGetObject builder castToStack "detailsType"
    <*> builderGetObject builder castToLabel "detailsSender"
    <*> optionalRow "detailsDestination"
    <*> builderGetObject builder castToLabel "detailsPath"
    <*> builderGetObject builder castToLabel "detailsMember"
    <*> optionalRow "detailsErrorName"
    <*> builderGetObject builder castToTextView "detailsArguments"
  where
    optionalRow labelId = (,)
        <$> builderGetObject builder castToLabel (labelId ++ "Caption")
        <*> builderGetObject builder castToLabel labelId


pickType :: Detailed Message -> String
pickType (Detailed _ m _ _) = case m of
    MethodCall {} -> "methodCall"
    MethodReturn {} -> "methodReturn"
    Error {} -> "error"
    Signal { signalDestination = d } ->
        maybe "signal" (const "directedSignal") d

getMemberMarkup :: Member -> String
getMemberMarkup m =
    toPangoMarkup $ formatMember (iface m) (membername m)

getMember :: Detailed Message -> Maybe Member
getMember (Detailed _ m _ _) = case m of
    MethodCall {}   -> Just $ member m
    Signal {}       -> Just $ member m
    MethodReturn {} -> callMember
    Error {}        -> callMember
  where
    callMember = member . deEvent <$> inReplyTo m

getDestination :: Detailed Message -> Maybe TaggedBusName
getDestination (Detailed _ m _ _) = case m of
    Signal { signalDestination = d } -> d
    _                                -> Just (destination m)

getErrorName :: Detailed a -> Maybe String
getErrorName (Detailed _ _ _ rm) = case rm of
    (D.ReceivedMethodError _ (MethodError { methodErrorName = ErrorName en})) -> Just en
    _                                                                         -> Nothing

formatMessage :: Detailed Message -> String
formatMessage (Detailed _ _ _ rm) =
    formatArgs $ D.receivedMessageBody rm
  where
    formatArgs = intercalate "\n" . map (format_Variant VariantStyleSignature)
-- TODO: suppress escaping and type sig for errors, which are always (s)

detailsViewGetTop :: DetailsView -> Widget
detailsViewGetTop = toWidget . detailsGrid

setOptionalRow :: OptionalRow
               -> Maybe String
               -> IO ()
setOptionalRow (caption, label) s_ = do
    case s_ of
        Just s -> do
            labelSetText label s
            widgetShow label
            widgetShow caption
        Nothing -> do
            widgetHide label
            widgetHide caption

detailsViewUpdate :: DetailsView
                  -> Detailed Message
                  -> IO ()
detailsViewUpdate d m = do
    buf <- textViewGetBuffer $ detailsBodyView d
    let member_ = getMember m
    stackSetVisibleChildName (detailsType d) (pickType m)

    -- TODO: these would be a lot more useful if we could resolve unique names
    -- to/from well-known names and show both
    labelSetText (detailsSender d) (unBusName . sender . deEvent $ m)
    setOptionalRow (detailsDestination d) (unBusName <$> getDestination m)
    setOptionalRow (detailsErrorName d) (getErrorName m)

    labelSetText (detailsPath d) (maybe unknown (D.formatObjectPath . path) member_)
    labelSetMarkup (detailsMember d) (maybe unknown getMemberMarkup member_)
    textBufferSetText buf $ formatMessage m
  where
    unknown = ""
