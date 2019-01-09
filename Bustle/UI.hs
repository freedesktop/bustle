{-
Bustle.UI: displays charts of D-Bus activity
Copyright © 2008–2011 Collabora Ltd.
Copyright © 2018      Will Thompson

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
{-# LANGUAGE ScopedTypeVariables #-}
module Bustle.UI
  ( uiMain
  )
where

import Control.Monad (void)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except

import Data.IORef
import qualified Data.Map as Map
import Data.List (intercalate)
import Data.Time
import Data.Tuple (swap)
import Data.Monoid (mempty)
import Text.Printf

import Paths_bustle
import Bustle.Application.Monad
import Bustle.Renderer
import Bustle.Types
import Bustle.Diagram
import qualified Bustle.Marquee as Marquee
import Bustle.Monitor
import Bustle.Util
import Bustle.UI.AboutDialog
import Bustle.UI.Canvas
import Bustle.UI.DetailsView
import Bustle.UI.FilterDialog
import Bustle.UI.OpenTwoDialog (setupOpenTwoDialog)
import Bustle.UI.Recorder
import Bustle.UI.RecordAddressDialog (showRecordAddressDialog)
import Bustle.StatisticsPane
import Bustle.Translation (__)
import Bustle.Loader
import Bustle.Loader.Pcap (convert)

import qualified Control.Exception as C
import System.Glib.GError (GError(..), failOnGError)
import System.GIO.Enums (IOErrorEnum(IoErrorCancelled))

import Graphics.UI.Gtk

import Graphics.Rendering.Cairo (withPDFSurface, renderWith)

import System.FilePath ( splitFileName, takeFileName, takeDirectory
                       , dropExtension, dropTrailingPathSeparator
                       , (</>), (<.>)
                       )
import System.GIO.File.File (fileFromParseName, fileMove, FileCopyFlags(..))

type B a = Bustle BConfig BState a

data LogDetails =
    RecordedLog FilePath
  | SingleLog FilePath
  | TwoLogs FilePath FilePath

-- Must be kept in sync with the names in the GtkBuilder file
data Page =
    InstructionsPage
  | PleaseHoldPage
  | CanvasPage
  deriving
    (Show)

data WindowInfo =
    WindowInfo { wiWindow :: Window
               , wiTitle :: Label
               , wiSubtitle :: Label
               , wiSpinner :: Spinner
               , wiRecord :: Button
               , wiStop :: Button
               , wiOpen :: Button
               , wiSave :: Button
               , wiExport :: Button
               , wiViewStatistics :: CheckMenuItem
               , wiFilterNames :: MenuItem
               , wiErrorBar :: InfoBar
               , wiErrorBarTitle :: Label
               , wiErrorBarDetails :: Label
               , wiStack :: Stack
               , wiSidebarHeader :: Widget -- TODO, GtkHeaderBar
               , wiSidebarStack :: Stack
               , wiStatsPane :: StatsPane
               , wiContentPaned :: Paned
               , wiCanvas :: Canvas (Detailed Message)
               , wiDetailsView :: DetailsView

               , wiLogDetails :: IORef (Maybe LogDetails)
               }

newtype BConfig =
    BConfig { debugEnabled :: Bool
            }

data BState = BState { windows :: Int
                     , initialWindow :: Maybe WindowInfo
                     }

modifyWindows :: (Int -> Int) -> B ()
modifyWindows f = modify $ \s -> s { windows = f (windows s) }

incWindows :: B ()
incWindows = modifyWindows (+1)

decWindows :: B Int
decWindows = modifyWindows (subtract 1) >> gets windows

uiMain :: IO ()
uiMain = failOnGError $ do
    args <- initGUI

    -- FIXME: get a real option parser
    let debug = any isDebug args

    let config = BConfig { debugEnabled = debug
                         }
        initialState = BState { windows = 0
                              , initialWindow = Nothing
                              }

    runB config initialState $ mainB (filter (not . isDebug) args)
  where
    isDebug = (== "--debug")

mainB :: [String] -> B ()
mainB args = do
  case args of
      ["--pair", sessionLogFile, systemLogFile] ->
          loadLog (TwoLogs sessionLogFile systemLogFile)
      _ -> mapM_ (loadLog . SingleLog) args

  -- If no windows are open (because none of the arguments, if any, were loaded
  -- successfully) create an empty window
  n <- gets windows
  when (n == 0) createInitialWindow

  io mainGUI

createInitialWindow :: B ()
createInitialWindow = do
  wi <- emptyWindow
  putInitialWindow wi

consumeInitialWindow :: B WindowInfo
consumeInitialWindow = do
    x <- gets initialWindow
    case x of
        Nothing   -> emptyWindow
        Just windowInfo -> do
            modify $ \s -> s { initialWindow = Nothing }
            return windowInfo

putInitialWindow :: WindowInfo
                 -> B ()
putInitialWindow wi =
    modify $ \s -> s { initialWindow = Just wi }

loadInInitialWindow :: LogDetails -> B ()
loadInInitialWindow = loadLogWith consumeInitialWindow

loadLog :: LogDetails -> B ()
loadLog = loadLogWith emptyWindow

openLog :: MonadIO io
        => LogDetails
        -> ExceptT LoadError io ( ([String], [DetailedEvent])
                                , ([String], [DetailedEvent])
                                )
openLog (RecordedLog filepath) = do
    result <- readLog filepath
    return (result, ([], []))
openLog (SingleLog filepath) = do
    result <- readLog filepath
    return (result, ([], []))
openLog (TwoLogs session system) = do
    sessionResult <- readLog session
    systemResult <- readLog system
    return (sessionResult, systemResult)

loadLogWith :: B WindowInfo   -- ^ action returning a window to load the log(s) in
            -> LogDetails
            -> B ()
loadLogWith getWindow logDetails = do
    windowInfo <- getWindow

    ret <- runExceptT $ do
        ((sessionWarnings, sessionMessages),
         (systemWarnings, systemMessages)) <- openLog logDetails

        -- FIXME: pass the log file name into the renderer
        let rr = process sessionMessages systemMessages
        io $ mapM warn $ sessionWarnings ++ systemWarnings ++ rrWarnings rr

        lift $ displayLog windowInfo
                          logDetails
                          sessionMessages
                          systemMessages
                          rr

    case ret of
      Left (LoadError f e) -> do
          let title = printf (__ "Could not read '%s'") f
          io $ displayError windowInfo title (Just e)
          putInitialWindow windowInfo
      Right () ->
          io $ hideError windowInfo

    io $ windowPresent (wiWindow windowInfo)


updateRecordingSubtitle :: WindowInfo
                        -> Int
                        -> IO ()
updateRecordingSubtitle wi j = do
    let message = printf (__ "Logged <b>%u</b> messages") j :: String
    labelSetMarkup (wiSubtitle wi) message


processBatch :: IORef [DetailedEvent]
             -> IORef Int
             -> WindowInfo
             -> IO (IO Bool)
processBatch pendingRef n wi = do
    rendererStateRef <- newIORef rendererStateNew
    -- FIXME: this is stupid. If we have to manually combine the outputs, it's
    -- basically just more state.
    rendererResultRef <- newIORef mempty

    return $ do
        pending <- readIORef pendingRef
        writeIORef pendingRef []

        unless (null pending) $ do
            rr <- atomicModifyIORef' rendererStateRef $ \s ->
                swap $ processSome (reverse pending) [] s

            oldRR <- readIORef rendererResultRef
            let rr' = oldRR `mappend` rr
            writeIORef rendererResultRef rr'

            unless (null (rrShapes rr)) $ do
                -- If the renderer produced some visible output, count it as a
                -- message from the user's perspective.
                modifyIORef' n (+ length pending)
                j <- readIORef n
                updateRecordingSubtitle wi j

                aChallengerAppears wi rr'

        return True


recorderRun :: WindowInfo
            -> Either BusType String
            -> FilePath
            -> BustleEnv BConfig BState
            -> IO ()
recorderRun wi target filename r = C.handle newFailed $ do
    -- TODO: I'm pretty sure the monitor is leaked
    monitor <- monitorNew target filename
    loaderStateRef <- newIORef Map.empty
    pendingRef <- newIORef []

    let updateLabel µs body = do
            s <- readIORef loaderStateRef
            (m, s') <- runStateT (convert µs body) s
            s' `seq` writeIORef loaderStateRef s'

            case m of
                Left e -> warn e
                Right message
                  | isRelevant (deEvent message) ->
                        modifyIORef' pendingRef (message:)
                  | otherwise -> return ()

    n <- newIORef (0 :: Int)
    processor <- processBatch pendingRef n wi
    processorId <- timeoutAdd processor 200

    stopActivatedId <- wiStop wi `on` buttonActivated $ monitorStop monitor
    handlerId <- monitor `on` monitorMessageLogged $ updateLabel
    void $ monitor `on` monitorStopped $ \domain code message -> do
        handleError domain code message

        signalDisconnect stopActivatedId
        signalDisconnect handlerId

        -- Flush out any last messages from the queue.
        timeoutRemove processorId
        processor

        hadOutput <- fmap (/= 0) (readIORef n)
        finished hadOutput
  where
    newFailed (GError domain code message) = do
        finished False
        handleError domain code message

    finished hadOutput =
        makeCallback (finishedRecording wi filename hadOutput) r

    -- Filter out IoErrorCancelled. In theory one should use
    --   catchGErrorJust IoErrorCancelled computation (\_ -> return ())
    -- but IOErrorEnum does not have an instance for GError domain.
    handleError domain code message = do
        gIoErrorQuark <- quarkFromString "g-io-error-quark"
        let cancelled = fromEnum IoErrorCancelled
        unless (domain == gIoErrorQuark && code == cancelled) $
            displayError wi (Marquee.toString message) Nothing


startRecording :: Either BusType String
               -> B ()
startRecording target = do
    wi <- consumeInitialWindow

    zt <- io getZonedTime
    -- I hate time manipulation
    let yyyy_mm_dd_hh_mm_ss = takeWhile (/= '.') (show zt)

    cacheDir <- io getCacheDir
    let filename = cacheDir </> yyyy_mm_dd_hh_mm_ss <.> "pcap"

    let title = printf (__ "Recording %s&#8230;") $ case target of
            Left BusTypeNone    -> error "whoops, this value shouldn't exist"
            Left BusTypeSession -> "session bus"
            Left BusTypeSystem  -> "system bus"
            Right address       -> address

    io $ do
        hideError wi
        widgetHide (wiRecord wi)
        widgetHide (wiOpen wi)
        widgetShow (wiStop wi)
        widgetGrabFocus (wiStop wi)
        spinnerStart (wiSpinner wi)
        labelSetMarkup (wiTitle wi) (title :: String)
        updateRecordingSubtitle wi 0

    setPage wi PleaseHoldPage
    embedIO $ recorderRun wi target filename

aChallengerAppears :: WindowInfo
                   -> RendererResult a
                   -> IO ()
aChallengerAppears wi rr = do
    updateDisplayedLog wi rr
    canvasScrollToBottom (wiCanvas wi)
    setPage wi CanvasPage

onMenuItemActivate :: MenuItemClass menuItem
                   => menuItem
                   -> IO ()
                   -> IO (ConnectId menuItem)
onMenuItemActivate mi = on mi menuItemActivate

finishedRecording :: WindowInfo
                  -> FilePath
                  -> Bool
                  -> B ()
finishedRecording wi tempFilePath producedOutput = do
    io $ do
        widgetShow (wiRecord wi)
        widgetShow (wiOpen wi)
        widgetHide (wiStop wi)
        spinnerStop (wiSpinner wi)

    if producedOutput
      then void $ do
        -- TODO: There is a noticable lag when reloading big files. It would be
        -- nice to either make the loading faster, or eliminate the reload.
        loadLogWith (return wi) (RecordedLog tempFilePath)

        let saveItem     = wiSave wi

        io $ do
            widgetSetSensitivity saveItem True
            saveItem `on` buttonActivated $ showSaveDialog wi (return ())
      else do
        setPage wi InstructionsPage
        putInitialWindow wi
        updateDisplayedLog wi (mempty :: RendererResult ())
        io $ do
            wiTitle wi `set` [ labelText := "" ]
            wiSubtitle wi `set` [ labelText := "" ]

showSaveDialog :: WindowInfo
               -> IO ()
               -> IO ()
showSaveDialog wi savedCb = do
    Just (RecordedLog tempFilePath) <- readIORef (wiLogDetails wi)
    let mwindow      = Just (wiWindow wi)
        tempFileName = takeFileName tempFilePath

    recorderChooseFile tempFileName mwindow $ \newFilePath -> do
        let tempFile = fileFromParseName tempFilePath
        let newFile  = fileFromParseName newFilePath

        result <- C.try $ fileMove tempFile newFile [FileCopyOverwrite] Nothing Nothing
        case result of
            Right _ -> do
                widgetSetSensitivity (wiSave wi) False
                wiSetLogDetails wi (SingleLog newFilePath)
                hideError wi
                savedCb
            Left (GError _ _ msg) -> do
                let title = __ "Couldn't save log: " ++ Marquee.toString msg
                    secondary = printf
                        (__ "You might want to manually recover the log from the temporary file at \
                            \\"%s\".") tempFilePath
                displayError wi title (Just secondary)

-- | Show a confirmation dialog if the log is unsaved. Suitable for use as a
--   'delete-event' handler.
promptToSave :: MonadIO io
             => WindowInfo
             -> io Bool -- ^ True if we showed a prompt; False if we're
                        --   happy to quit
promptToSave wi = io $ do
    mdetails <- readIORef (wiLogDetails wi)
    case mdetails of
        Just (RecordedLog tempFilePath) -> do
            let tempFileName = takeFileName tempFilePath
                title = printf (__ "Save log '%s' before closing?") tempFileName :: String
            prompt <- messageDialogNew (Just (wiWindow wi))
                                       [DialogModal]
                                       MessageWarning
                                       ButtonsNone
                                       title
            messageDialogSetSecondaryText prompt
                (__ "If you don't save, this log will be lost forever.")
            dialogAddButton prompt (__ "Close _Without Saving") ResponseClose
            dialogAddButton prompt stockCancel ResponseCancel
            dialogAddButton prompt stockSave ResponseYes

            widgetShowAll prompt
            prompt `after` response $ \resp -> do
                let closeUp = widgetDestroy (wiWindow wi)
                case resp of
                    ResponseYes -> showSaveDialog wi closeUp
                    ResponseClose -> closeUp
                    _ -> return ()
                widgetDestroy prompt

            return True
        _ -> return False

maybeQuit :: B ()
maybeQuit = do
  n <- decWindows
  when (n == 0) (io mainQuit)

emptyWindow :: B WindowInfo
emptyWindow = do
  builder <- io builderNew
  io $ builderAddFromFile builder =<< getDataFileName "data/bustle.ui"

  -- Grab a bunch of widgets. Surely there must be a better way to do this?
  let getW cast name = io $ builderGetObject builder cast name

  window <- getW castToWindow "diagramWindow"

  title    <- getW castToLabel "headerTitle"
  subtitle <- getW castToLabel "headerSubtitle"
  spinner  <- getW castToSpinner "headerSpinner"

  [openItem, openTwoItem] <- mapM (getW castToMenuItem) ["open", "openTwo"]
  recordSessionItem <- getW castToMenuItem "recordSession"
  recordSystemItem <- getW castToMenuItem "recordSystem"
  recordAddressItem <- getW castToMenuItem "recordAddress"
  headerRecord <- getW castToButton "headerRecord"
  headerStop   <- getW castToButton "headerStop"
  headerOpen   <- getW castToButton "headerOpen"
  headerSave   <- getW castToButton "headerSave"
  headerExport <- getW castToButton "headerExport"

  viewStatistics <- getW castToCheckMenuItem "statistics"
  filterNames <- getW castToMenuItem "filter"
  aboutItem <- getW castToMenuItem "about"

  errorBar        <- getW castToInfoBar "errorBar"
  errorBarTitle   <- getW castToLabel "errorBarTitle"
  errorBarDetails <- getW castToLabel "errorBarDetails"

  io $ errorBar `on` infoBarResponse $ \_ ->
      widgetHide errorBar

  stack <- getW castToStack "diagramOrNot"
  sidebarHeader <- getW castToWidget "sidebarHeader"
  sidebarStack <- getW castToStack "sidebarStack"
  contentPaned <- getW castToPaned "contentPaned"

  -- Open two logs dialog
  openTwoDialog <- embedIO $ \r ->
      setupOpenTwoDialog window $ \f1 f2 ->
          makeCallback (loadInInitialWindow (TwoLogs f1 f2)) r

  -- Set up the window itself
  embedIO $ (window `on` objectDestroy) . makeCallback maybeQuit

  -- File menu and related buttons
  embedIO $ \r -> do
      onMenuItemActivate recordSessionItem $
          makeCallback (startRecording (Left BusTypeSession)) r
      onMenuItemActivate recordSystemItem $
          makeCallback (startRecording (Left BusTypeSystem)) r
      onMenuItemActivate recordAddressItem $
          showRecordAddressDialog window $ \address ->
              makeCallback (startRecording (Right address)) r

      onMenuItemActivate openItem $ makeCallback (showOpenDialog window) r
      onMenuItemActivate openTwoItem $ widgetShowAll openTwoDialog

  -- TODO: really this wants to live in the application menu, but that entails binding GApplication,
  -- GtkApplication, GMenu, GActionMap, GActionEntry, ...
  --
  -- Similarly, the drop-down menus would look better as popovers. But here we are.
  io $ onMenuItemActivate aboutItem $ showAboutDialog window

  statsPane <- io $ statsPaneNew builder

  details <- io $ detailsViewNew builder

  -- The stats start off hidden.
  io $ widgetHide sidebarStack

  showBounds <- asks debugEnabled
  canvas <- io $ canvasNew builder showBounds (updateDetailsView details)

  logDetailsRef <- io $ newIORef Nothing
  let windowInfo = WindowInfo { wiWindow = window
                              , wiTitle = title
                              , wiSubtitle = subtitle
                              , wiSpinner = spinner
                              , wiRecord = headerRecord
                              , wiOpen   = headerOpen
                              , wiStop = headerStop
                              , wiSave = headerSave
                              , wiExport = headerExport
                              , wiViewStatistics = viewStatistics
                              , wiFilterNames = filterNames
                              , wiErrorBar = errorBar
                              , wiErrorBarTitle = errorBarTitle
                              , wiErrorBarDetails = errorBarDetails
                              , wiStack = stack
                              , wiSidebarHeader = sidebarHeader
                              , wiSidebarStack = sidebarStack
                              , wiStatsPane = statsPane
                              , wiContentPaned = contentPaned
                              , wiCanvas = canvas
                              , wiDetailsView = details
                              , wiLogDetails = logDetailsRef
                              }

  io $ window `on` deleteEvent $ promptToSave windowInfo
  incWindows
  io $ widgetShow window
  return windowInfo

updateDetailsView :: DetailsView
                  -> Maybe (Detailed Message)
                  -> IO ()
updateDetailsView detailsView newMessage =
    case newMessage of
        Nothing ->
            widgetHide $ detailsViewGetTop detailsView
        Just m  -> do
            detailsViewUpdate detailsView m
            widgetShow $ detailsViewGetTop detailsView

updateDisplayedLog :: MonadIO io
                   => WindowInfo
                   -> RendererResult a
                   -> io ()
updateDisplayedLog wi rr = io $ do
    let shapes = rrShapes rr
        regions = rrRegions rr
        canvas = wiCanvas wi

    (windowWidth, _) <- windowGetSize (wiWindow wi)

    canvasSetShapes canvas shapes regions (rrCentreOffset rr) windowWidth

splitFileName_ :: String
               -> (String, String)
splitFileName_ s = (dropTrailingPathSeparator d, f)
  where
      (d, f) = splitFileName s

logWindowTitle :: LogDetails
               -> (String, String)
logWindowTitle (RecordedLog filepath) = ("*" ++ takeFileName filepath, "")
logWindowTitle (SingleLog   filepath) = (name, directory)
  where
    (directory, name) = splitFileName_ filepath
logWindowTitle (TwoLogs sessionPath systemPath) =
    -- TODO: this looks terrible, need a custom widget
    (sessionName ++ " & " ++ systemName,
     if sessionDirectory == systemDirectory
        then sessionDirectory
        else sessionDirectory ++ " & " ++ systemDirectory)
  where
    (sessionDirectory, sessionName) = splitFileName_ sessionPath
    (systemDirectory,  systemName ) = splitFileName_ systemPath

logTitle :: LogDetails
         -> String
logTitle (RecordedLog filepath) = dropExtension $ takeFileName filepath
logTitle (SingleLog   filepath) = dropExtension $ takeFileName filepath
logTitle (TwoLogs sessionPath systemPath) =
    intercalate " & " . map (dropExtension . takeFileName)
                      $ [sessionPath, systemPath]

wiSetLogDetails :: WindowInfo
                -> LogDetails
                -> IO ()
wiSetLogDetails wi logDetails = do
    writeIORef (wiLogDetails wi) (Just logDetails)
    let (title, subtitle) = logWindowTitle logDetails
    wiWindow wi `set` [ windowTitle := title ]
    wiTitle wi `set` [ labelText := title ]
    wiSubtitle wi `set` [ labelText := subtitle ]

setPage :: MonadIO io
        => WindowInfo
        -> Page
        -> io ()
setPage wi page = io $ stackSetVisibleChildName (wiStack wi) (show page)

displayLog :: WindowInfo
           -> LogDetails
           -> Log
           -> Log
           -> RendererResult Participants
           -> B ()
displayLog wi@WindowInfo { wiWindow = window
                       , wiExport = exportItem
                       , wiViewStatistics = viewStatistics
                       , wiFilterNames = filterNames
                       , wiCanvas = canvas
                       , wiSidebarHeader = sidebarHeader
                       , wiSidebarStack = sidebarStack
                       , wiStatsPane = statsPane
                       }
           logDetails
           sessionMessages
           systemMessages
           rr = io $ void $ do
    wiSetLogDetails wi logDetails

    nameFilterRef <- newIORef emptyNameFilter

    updateDisplayedLog wi rr

    widgetSetSensitivity exportItem True
    exportItem `on` buttonActivated $ do
        shapes <- canvasGetShapes canvas
        saveToPDFDialogue wi shapes

    setPage wi CanvasPage
    canvasFocus canvas

    -- FIXME: this currently shows stats for all messages, not post-filtered messages
    statsPaneSetMessages statsPane sessionMessages systemMessages

    widgetSetSensitivity viewStatistics True
    viewStatistics `on` checkMenuItemToggled $ do
        active <- checkMenuItemGetActive viewStatistics
        if active
            then do widgetShow sidebarStack
                    widgetShow sidebarHeader
            else do widgetHide sidebarStack
                    widgetHide sidebarHeader

    widgetSetSensitivity filterNames True
    onMenuItemActivate filterNames $ do
        nameFilter <- readIORef nameFilterRef
        -- FIXME: also allow filtering system bus in two-bus case
        nameFilter' <- runFilterDialog window (sessionParticipants $ rrApplications rr) nameFilter
        writeIORef nameFilterRef nameFilter'
        let rr' = processWithFilters (sessionMessages, nameFilter') (systemMessages, emptyNameFilter)

        updateDisplayedLog wi rr'

showOpenDialog :: Window
               -> B ()
showOpenDialog window = embedIO $ \r -> do
  chooser <- fileChooserDialogNew Nothing (Just window) FileChooserActionOpen
             [ ("gtk-cancel", ResponseCancel)
             , ("gtk-open", ResponseAccept)
             ]
  chooser `set` [ windowModal := True
                , fileChooserLocalOnly := True
                ]

  chooser `after` response $ \resp -> do
      when (resp == ResponseAccept) $ do
          Just fn <- fileChooserGetFilename chooser
          makeCallback (loadInInitialWindow (SingleLog fn)) r
      widgetDestroy chooser

  widgetShowAll chooser

saveToPDFDialogue :: WindowInfo
                  -> Diagram
                  -> IO ()
saveToPDFDialogue wi shapes = do
  let parent = Just (wiWindow wi)
  chooser <- fileChooserDialogNew Nothing parent FileChooserActionSave
             [ ("gtk-cancel", ResponseCancel)
             , ("gtk-save", ResponseAccept)
             ]
  chooser `set` [ windowModal := True
                , fileChooserLocalOnly := True
                , fileChooserDoOverwriteConfirmation := True
                ]

  Just logDetails <- readIORef $ wiLogDetails wi

  let filename  = logTitle logDetails <.> "pdf"
  fileChooserSetCurrentName chooser filename

  -- If the currently-loaded log has a meaningful directory, suggest that as
  -- the default.
  let mdirectory = case logDetails of
          RecordedLog _ -> Nothing
          SingleLog p   -> Just $ takeDirectory p
          TwoLogs p _   -> Just $ takeDirectory p
  forM_ mdirectory $ fileChooserSetCurrentFolder chooser

  chooser `after` response $ \resp -> do
      when (resp == ResponseAccept) $ do
          Just fn <- io $ fileChooserGetFilename chooser
          let (width, height) = diagramDimensions shapes

          r <- C.try $ withPDFSurface fn width height $ \surface ->
              renderWith surface $ drawDiagram False shapes
          case r of
              Left (e :: C.IOException) -> do
                  let title = __ "Couldn't export log as PDF: " ++ show e
                  displayError wi title Nothing
              Right () ->
                  hideError wi

      widgetDestroy chooser

  widgetShowAll chooser


displayError :: WindowInfo
             -> String
             -> Maybe String
             -> IO ()
displayError wi title mbody = do
    labelSetMarkup (wiErrorBarTitle wi) . Marquee.toPangoMarkup
                                        . Marquee.b
                                        $ Marquee.escape title

    let details = wiErrorBarDetails wi
    case mbody of
        Just body -> do
            labelSetMarkup details . Marquee.toPangoMarkup
                                   . Marquee.small
                                   $ Marquee.escape body
            widgetShow details
        Nothing   -> widgetHide details

    widgetShow $ wiErrorBar wi


hideError :: WindowInfo
          -> IO ()
hideError = widgetHide . wiErrorBar

-- vim: sw=2 sts=2
