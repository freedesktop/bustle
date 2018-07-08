{-
Bustle.Loader: loads logs using one of the two sub-loaders
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
module Bustle.Loader
  ( readLog
  , LoadError(..)
  )
where

import Control.Monad.Except

import qualified Bustle.Loader.Pcap as Pcap
import Bustle.Types
import Bustle.Util (io)

data LoadError = LoadError FilePath String

-- this nested case stuff is ugly, but it's less ugly than it looked with
-- combinators to turn IO (Either a b) into ErrorT LoadError IO b using various
-- a -> LoadError functions.
readLog :: MonadIO io
        => FilePath
        -> ExceptT LoadError io ([String], Log)
readLog f = do
    pcapResult <- io $ Pcap.readPcap f
    case pcapResult of
        Right ms -> return $ ms
        Left ioe -> throwError $ LoadError f (show ioe)
