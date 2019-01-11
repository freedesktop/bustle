{-# LANGUAGE FlexibleContexts #-}
{-
Bustle.UI.FilterDialog: allows the user to filter the displayed log
Copyright © 2011 Collabora Ltd.
Copyright © 2019 Will Thompson

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
module Bustle.UI.FilterDialog
  ( runFilterDialog
  )
where

import Control.Monad (forM_)
import Data.List (intercalate, groupBy, elemIndices, elemIndex)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Function as F
import Data.IORef

import Graphics.UI.Gtk
import Graphics.UI.Gtk.ModelView.CellRendererCombo (cellComboTextModel)

import Bustle.Translation (__)
import Bustle.Types

import Paths_bustle

data NameVisibility = NameVisibilityDefault
                    | NameVisibilityOnly
                    | NameVisibilityNever
  deriving (Show, Eq, Ord, Enum, Bounded)

nameVisibilityName :: NameVisibility
                   -> String
nameVisibilityName v = case v of
    NameVisibilityDefault -> __ "Default"
    NameVisibilityOnly    -> __ "Only this"
    NameVisibilityNever   -> __ "Hidden"

data NameEntry = NameEntry { neUniqueName :: UniqueName
                           , neOtherNames :: Set OtherName
                           , neVisibility :: NameVisibility
                           }

namespace :: String
          -> (String, String)
namespace name = case reverse (elemIndices '.' name) of
    []    -> ("", name)
    (i:_) -> splitAt (i + 1) name

formatNames :: NameEntry
            -> String
formatNames ne
    | Set.null os = unUniqueName (neUniqueName ne)
    | otherwise = intercalate "\n" . map (formatGroup . groupGroup) $ groups
  where
    os = neOtherNames ne

    groups = groupBy ((==) `F.on` fst) . map (namespace . unOtherName) $ Set.toAscList os

    groupGroup [] = error "unpossible empty group from groupBy"
    groupGroup xs@((ns, _):_) = (ns, map snd xs)

    formatGroup (ns, [y]) = ns ++ y
    formatGroup (ns, ys)  = ns ++ "{" ++ intercalate "," ys ++ "}"

type NameStore = ListStore NameEntry

makeStore :: [(UniqueName, Set OtherName)]
          -> NameFilter
          -> IO NameStore
makeStore names nameFilter =
    listStoreNew $ map toNameEntry names
  where
    toNameEntry (u, os) = NameEntry { neUniqueName = u
                                    , neOtherNames = os
                                    , neVisibility = toVisibility u
                                    }
    toVisibility u | Set.member u (nfOnly nameFilter)  = NameVisibilityOnly
                   | Set.member u (nfNever nameFilter) = NameVisibilityNever
                   | otherwise                         = NameVisibilityDefault

nameStoreUpdate :: NameStore
                -> Int
                -> (NameEntry -> NameEntry)
                -> IO ()
nameStoreUpdate nameStore i f = do
    ne <- listStoreGetValue nameStore i
    listStoreSetValue nameStore i $ f ne

makeView :: NameStore
         -> TreeView
         -> IO ()
makeView nameStore nameView = do
    treeViewSetModel nameView (Just nameStore)

    -- Bus name column
    nameCell <- cellRendererTextNew
    nameColumn <- treeViewColumnNew
    nameColumn `set` [ treeViewColumnTitle := __ "Bus Name"
                     , treeViewColumnExpand := True
                     ]
    treeViewColumnPackStart nameColumn nameCell True
    treeViewAppendColumn nameView nameColumn

    cellLayoutSetAttributes nameColumn nameCell nameStore $ \ne ->
        [ cellText := formatNames ne ]

    -- TreeStore of possible visibility states
    let nameVisibilities = [minBound..]
    let nameVisibilityNames = map nameVisibilityName nameVisibilities
    visibilityModel <- listStoreNew nameVisibilityNames
    let visibilityNameCol = makeColumnIdString 1
    treeModelSetColumn visibilityModel visibilityNameCol id

    -- Visibility column
    comboCell <- cellRendererComboNew
    comboCell `set` [ cellTextEditable := True
                    , cellComboHasEntry := False
                    ]

    comboColumn <- treeViewColumnNew
    comboColumn `set` [ treeViewColumnTitle := __ "Visibility"
                      , treeViewColumnExpand := False
                      ]
    treeViewColumnPackStart comboColumn comboCell True
    treeViewAppendColumn nameView comboColumn

    cellLayoutSetAttributes comboColumn comboCell nameStore $ \ne ->
        [ cellComboTextModel := (visibilityModel, visibilityNameCol)
        , cellText :=> do
            let Just j = elemIndex (neVisibility ne) nameVisibilities
            listStoreGetValue visibilityModel j
        ]
    comboCell `on` edited $ \[i] str -> do
        let (Just j) = elemIndex str nameVisibilityNames
        nameStoreUpdate nameStore i $ \ne ->
            ne { neVisibility = nameVisibilities !! j }

    return ()


runFilterDialog :: WindowClass parent
                => parent -- ^ The window to which to attach the dialog
                -> [(UniqueName, Set OtherName)] -- ^ Names, in order of appearance
                -> NameFilter -- ^ Current filter
                -> IO NameFilter -- ^ New filter
runFilterDialog parent names currentFilter = do
    builder <- builderNew
    builderAddFromFile builder =<< getDataFileName "data/FilterDialog.ui"

    d <- builderGetObject builder castToDialog ("filterDialog" :: String)
    (_, windowHeight) <- windowGetSize parent
    windowSetDefaultSize d (-1) (windowHeight * 3 `div` 4)
    d `set` [ windowTransientFor := parent ]

    nameStore <- makeStore names currentFilter
    makeView nameStore =<< builderGetObject builder castToTreeView ("filterTreeView" :: String)

    resetButton <- builderGetObject builder castToButton ("resetButton" :: String)
    resetButton `on` buttonActivated $ do
        n <- listStoreGetSize nameStore
        forM_ [0..n-1] $ \i -> do
            ne <- listStoreGetValue nameStore i
            case neVisibility ne of
                NameVisibilityDefault -> return ()
                _                     -> listStoreSetValue nameStore i $
                    ne { neVisibility = NameVisibilityDefault }

    -- TODO: expose this ref to the caller, with change notification, so the
    -- diagram can update instantly
    filterRef <- newIORef currentFilter
    let updateResetSensitivity = do
            nf <- readIORef filterRef
            let isEmpty = Set.null (nfOnly nf) && Set.null (nfNever nf)
            widgetSetSensitive resetButton $ not isEmpty

    updateResetSensitivity
    nameStore `on` rowChanged $ \[i] _iter -> do
        ne <- listStoreGetValue nameStore i
        let u = neUniqueName ne
        -- Should we smush this into nameFilterModify, move the enum into
        -- Bustle.Types?
        let f = case neVisibility ne of
                NameVisibilityDefault -> nameFilterRemove
                NameVisibilityOnly    -> nameFilterAddOnly
                NameVisibilityNever   -> nameFilterAddNever
        modifyIORef' filterRef $ f u
        updateResetSensitivity

    _ <- dialogRun d

    widgetDestroy d

    readIORef filterRef
