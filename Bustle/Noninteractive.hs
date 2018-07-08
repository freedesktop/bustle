{-
Bustle.Noninteractive: driver for ASCII-art statistics generation
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
module Bustle.Noninteractive
  ( runCount
  , runTime
  , runDot
  )
where

import Prelude hiding (log)

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Data.Maybe (mapMaybe)
import Data.List (nub)
import Control.Monad.Except
import Text.Printf

import Bustle.Loader
import Bustle.Translation (__)
import Bustle.Types
import Bustle.Stats

warn :: String -> IO ()
warn = hPutStrLn stderr

process :: FilePath -> (Log -> [a]) -> (a -> String) -> IO ()
process filepath analyze format = do
    ret <- runExceptT $ readLog filepath
    case ret of
        Left (LoadError _ err) -> do
            warn $ printf (__ "Couldn't parse '%s': %s") filepath err
            exitFailure
        Right (warnings, log) -> do
            mapM_ warn warnings
            mapM_ (putStrLn . format) $ analyze log

formatInterface :: Maybe InterfaceName -> String
formatInterface = maybe (__ "(no interface)") formatInterfaceName

runCount :: FilePath -> IO ()
runCount filepath = process filepath frequencies format
  where
    format :: FrequencyInfo -> String
    format (FrequencyInfo c t i m) =
        printf " %4d %6s %s.%s" c (typeName t) (formatInterface i) (formatMemberName m)

    typeName TallyMethod = "method"
    typeName TallySignal = "signal"

runTime :: FilePath -> IO ()
runTime filepath = process filepath methodTimes format
  where
    format :: TimeInfo -> String
    format (TimeInfo interface method total ncalls mean) =
        printf " %9.4f %3d %9.4f %s.%s" total ncalls mean
           (formatInterface interface) (formatMemberName method)

runDot :: FilePath -> IO ()
runDot filepath = process filepath makeDigraph id
  where
    makeDigraph log = ["digraph bustle {"] ++ makeDigraph' log ++ ["}"]

    makeDigraph' log =
        [ concat ["  \"", unBusName s, "\" -> \"", unBusName d, "\";"]
        | (s, d) <- nub . mapMaybe (methodCall . deEvent) $ log
        ]

    methodCall (MethodCall {sender = s, destination = d}) = Just (s, d)
    methodCall _ = Nothing
