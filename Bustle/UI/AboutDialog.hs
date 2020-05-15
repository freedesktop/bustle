{-
Bustle.UI.AboutDialog: just the about dialog…
Copyright © 2008–2012 Collabora Ltd.

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
module Bustle.UI.AboutDialog
  (
    showAboutDialog
  )
where

import Prelude

import Data.Version (showVersion)
import Control.Exception
import Control.Monad (when)

import Graphics.UI.Gtk

import Bustle.Translation (__)
import Bustle.Util
import Paths_bustle


showAboutDialog :: Window
                -> IO ()
showAboutDialog window = do
    dialog <- aboutDialogNew

    license <- (Just `fmap` (readFile =<< getDataFileName "LICENSE"))
               `catch` (\e -> warn (show (e :: IOException)) >> return Nothing)

    dialog `set` [ aboutDialogName := __ "Bustle"
                 , aboutDialogVersion := showVersion version
                 , aboutDialogComments := __ "Someone's favourite D-Bus profiler"
                 , aboutDialogWebsite := "https://gitlab.freedesktop.org/bustle/bustle#readme"
                 , aboutDialogAuthors := authors
                 , aboutDialogArtists := artists
                 , aboutDialogCopyright := "© 2008–2017 Will Thompson, Collabora Ltd. and contributors"
                 , aboutDialogLicense := license
                 , aboutDialogLogoIconName := Just "org.freedesktop.Bustle"
                 , windowModal := True
                 , windowTransientFor := window
                 ]
    dialog `after` response $ \resp ->
        when (resp == ResponseCancel) (widgetDestroy dialog)

    widgetShowAll dialog

authors :: [String]
authors = [ "Will Thompson <will@willthompson.co.uk>"
          , "Dafydd Harries"
          , "Chris Lamb"
          , "Marc Kleine-Budde"
          , "Cosimo Alfarano"
          , "Sergei Trofimovich"
          , "Alex Merry"
          , "Philip Withnall"
          , "Jonny Lamb"
          , "Daniel Firth"
          ]

artists :: [String]
artists = [ "Tobias Bernard"
          ]
