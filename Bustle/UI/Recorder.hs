module Bustle.UI.Recorder
  (
    recorderChooseFile
  , recorderRun
  )
where

import Control.Monad (when)
import Control.Concurrent.MVar
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid
import Control.Monad.State (runStateT)

import System.Glib.GError
import Graphics.UI.Gtk

import Bustle.Loader.Pcap (convert)
import Bustle.Loader (isRelevant)
import Bustle.Monitor
import Bustle.Renderer
import Bustle.Types
import Bustle.UI.Util (displayError)
import Bustle.Util

type RecorderIncomingCallback = RendererResult Participants
                             -> IO ()
type RecorderFinishedCallback = IO ()

recorderRun :: FilePath
            -> Maybe Window
            -> RecorderIncomingCallback
            -> RecorderFinishedCallback
            -> IO ()
recorderRun filename mwindow incoming finished = handleGError newFailed $ do
    monitor <- monitorNew BusTypeSession filename NoDebugOutput
    dialog <- dialogNew

    maybe (return ()) (windowSetTransientFor dialog) mwindow
    dialog `set` [ windowModal := True ]

    label <- labelNew Nothing
    labelSetMarkup label "Logged <b>0</b> messages…"
    n <- newMVar (0 :: Integer)
    loaderStateRef <- newMVar Map.empty
    rendererStateRef <- newMVar rendererStateNew
    -- FIXME: this is stupid. If we have to manually combine the outputs, it's
    -- basically just more state.
    rendererResultRef <- newMVar mempty
    let updateLabel body = do
        -- of course, modifyMVar and runStateT have their tuples back to front.
        m <- modifyMVar loaderStateRef $ \s -> do
            (m, s') <- runStateT (convert 0 body) s
            return (s', m)

        case m of
            Left e -> warn e
            Right message
              | isRelevant (dmMessage message) -> do
                rr <- modifyMVar rendererStateRef $ \s -> do
                    let (rr, s') = processSome [message] [] s
                    return (s', rr)

                when (not (null (rrShapes rr))) $ do
                    -- If the renderer produced some output, count it as a
                    -- message from the user's perspective.
                    i <- takeMVar n
                    let j = i + 1
                    labelSetMarkup label $
                        "Logged <b>" ++ show j ++ "</b> messages…"
                    putMVar n j

                oldRR <- takeMVar rendererResultRef
                let rr' = oldRR `mappend` rr
                putMVar rendererResultRef rr'
                incoming rr'
              | otherwise -> return ()

    handlerId <- monitor `on` monitorMessageLogged $ updateLabel

    bar <- progressBarNew
    pulseId <- timeoutAdd (progressBarPulse bar >> return True) 100

    vbox <- dialogGetUpper dialog
    boxPackStart vbox label PackGrow 0
    boxPackStart vbox bar PackNatural 0

    dialogAddButton dialog "gtk-media-stop" ResponseClose

    dialog `afterResponse` \_ -> do
        monitorStop monitor
        signalDisconnect handlerId
        timeoutRemove pulseId
        widgetDestroy dialog
        finished

    widgetShowAll dialog
  where
    newFailed (GError _ _ message) = do
        displayError mwindow message Nothing

recorderChooseFile :: FilePath
                   -> Maybe Window
                   -> (FilePath -> IO ())
                   -> IO ()
recorderChooseFile name mwindow callback = do
    chooser <- fileChooserDialogNew Nothing mwindow FileChooserActionSave
             [ ("gtk-cancel", ResponseCancel)
             , ("gtk-new", ResponseAccept)
             ]
    fileChooserSetCurrentName chooser name
    chooser `set` [ windowModal := True
                  , fileChooserLocalOnly := True
                  , fileChooserDoOverwriteConfirmation := True
                  ]

    chooser `afterResponse` \resp -> do
        when (resp == ResponseAccept) $ do
            Just fn <- fileChooserGetFilename chooser
            callback fn
        widgetDestroy chooser

    widgetShowAll chooser
