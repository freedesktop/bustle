{-# LANGUAGE OverloadedStrings #-}
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

import Data.List (intercalate, groupBy, elemIndices)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Function as F

import Graphics.UI.Gtk

import Bustle.Types

import Paths_bustle

data NameEntry = NameEntry { neUniqueName :: UniqueName
                           , neOtherNames :: Set OtherName
                           , neVisible :: Bool
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
          -> Set UniqueName
          -> IO NameStore
makeStore names currentlyHidden =
    listStoreNew $ map toNameEntry names
  where
    toNameEntry (u, os) = NameEntry { neUniqueName = u
                                    , neOtherNames = os
                                    , neVisible = not (Set.member u currentlyHidden)
                                    }

makeView :: NameStore
         -> TreeView
         -> IO ()
makeView nameStore nameView = do
    treeViewSetModel nameView (Just nameStore)

    tickyCell <- cellRendererToggleNew
    tickyColumn <- treeViewColumnNew
    treeViewColumnPackStart tickyColumn tickyCell True
    treeViewAppendColumn nameView tickyColumn

    cellLayoutSetAttributes tickyColumn tickyCell nameStore $ \ne ->
        [ cellToggleActive := neVisible ne ]

    on tickyCell cellToggled $ \pathstr -> do
        let [i] = stringToTreePath pathstr
        ne <- listStoreGetValue nameStore i
        listStoreSetValue nameStore i (ne { neVisible = not (neVisible ne) })

    nameCell <- cellRendererTextNew
    nameColumn <- treeViewColumnNew
    treeViewColumnPackStart nameColumn nameCell True
    treeViewAppendColumn nameView nameColumn

    cellLayoutSetAttributes nameColumn nameCell nameStore $ \ne ->
        [ cellText := formatNames ne ]

runFilterDialog :: WindowClass parent
                => parent -- ^ The window to which to attach the dialog
                -> [(UniqueName, Set OtherName)] -- ^ Names, in order of appearance
                -> Set UniqueName -- ^ Currently-hidden names
                -> IO (Set UniqueName) -- ^ The set of names to *hide*
runFilterDialog parent names currentlyHidden = do
    builder <- builderNew
    builderAddFromFile builder =<< getDataFileName "data/FilterDialog.ui"

    d <- builderGetObject builder castToDialog ("filterDialog" :: String)
    (windowWidth, windowHeight) <- windowGetSize parent
    windowSetDefaultSize d (windowWidth * 7 `div` 8) (windowHeight `div` 2)
    d `set` [ windowTransientFor := parent ]

    nameStore <- makeStore names currentlyHidden
    makeView nameStore =<< builderGetObject builder castToTreeView ("filterTreeView" :: String)

    _ <- dialogRun d

    widgetDestroy d

    results <- listStoreToList nameStore
    return $ Set.fromList [ neUniqueName ne
                          | ne <- results
                          , not (neVisible ne)
                          ]
