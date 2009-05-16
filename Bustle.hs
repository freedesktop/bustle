{-
Bustle: a tool to draw charts of D-Bus activity
Copyright (C) 2008–2009 Collabora Ltd.

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
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
module Main (main)
where

import Prelude hiding (log, catch)

import Control.Arrow ((&&&))
import Control.Exception
import Control.Monad (when, forM)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Error

import Data.IORef
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Version (showVersion)

import Paths_bustle
import Bustle.Parser
import Bustle.Renderer
import Bustle.Types
import Bustle.Diagram
import Bustle.Upgrade (upgrade)

import System.Glib.GError (GError(..), catchGError)
import Graphics.UI.Gtk
-- FIXME: Events is deprecated in favour of EventM
import Graphics.UI.Gtk.Gdk.Events
import Graphics.Rendering.Cairo (withPDFSurface, renderWith)

import System.Process (runProcess)
import System.Environment (getArgs)
import System.FilePath (splitFileName, dropExtension)
import System.IO (hPutStrLn, stderr)

{-
Cunning threadable monad. Inspired by Monadic Tunnelling
<http://www.haskell.org/pipermail/haskell-cafe/2007-July/028501.html>

This is a state monad, secretly implemented with an IORef. The idea is to make
it possible to reconstitute the monad within a Gtk callback. Given:

  x :: Badger
  onDance :: Badger -> IO a -> IO ()
  dancedCB :: B a

  onMeme :: Badger -> (Mushroom -> IO a) -> IO ()
  memedCB :: Mushroom -> B a

One can write:

  embedIO $ onDance x . makeCallback dancedCB
  embedIO $ \r -> onMeme x (reconstruct r . dancedCB)

I'm not sure which of makeCallback and reconstruct are more useful.
-}

newtype B a = B (ReaderT (IORef BState) IO a)
  deriving (Functor, Monad, MonadIO)

type Details = (FilePath, Diagram)
type WindowInfo = (Window, ImageMenuItem, Notebook, Layout)

data BState = BState { windows :: Int
                     , initialWindow :: Maybe WindowInfo
                     }

instance MonadState BState B where
  get = B $ ask >>= liftIO . readIORef
  put x = B $ ask >>= \r -> liftIO $ writeIORef r x

embedIO :: (IORef BState -> IO a) -> B a
embedIO act = B $ do
  r <- ask
  liftIO $ act r

makeCallback :: B a -> IORef BState -> IO a
makeCallback (B act) x = runReaderT act x

reconstruct :: IORef BState -> B a -> IO a
reconstruct = flip makeCallback

runB :: B a -> IO a
runB (B act) = runReaderT act =<< newIORef (BState 0 Nothing)

{- And now, some convenience functions -}

io :: MonadIO m => IO a -> m a
io = liftIO

modifyWindows :: (Int -> Int) -> B ()
modifyWindows f = modify $ \s -> s { windows = f (windows s) }

incWindows :: B ()
incWindows = modifyWindows (+1)

decWindows :: B Int
decWindows = modifyWindows (subtract 1) >> gets windows

{- End of boilerplate. -}

-- Used to log warnings which aren't important to the user, but which should
-- probably be noted.
warn :: String -> IO ()
warn = hPutStrLn stderr . ("Warning: " ++)

main :: IO ()
main = runB mainB

mainB :: B ()
mainB = do
  io initGUI

  -- Try to load arguments, if any.
  mapM_ loadLog =<< io getArgs

  -- If no windows are open (because none of the arguments, if any, were loaded
  -- successfully) create an empty window
  n <- gets windows
  when (n == 0) createInitialWindow

  io mainGUI

createInitialWindow :: B ()
createInitialWindow = do
  misc <- emptyWindow
  modify $ \s -> s { initialWindow = Just misc }

loadInInitialWindow :: FilePath -> B ()
loadInInitialWindow = loadLogWith consumeInitialWindow
  where consumeInitialWindow = do
          x <- gets initialWindow
          case x of
            Nothing   -> emptyWindow
            Just misc -> do
              modify $ \s -> s { initialWindow = Nothing }
              return misc

loadLog :: FilePath -> B ()
loadLog = loadLogWith emptyWindow

-- Displays a modal error dialog, with the given strings as title and body
-- respectively.
displayError :: String -> String -> IO ()
displayError title body = do
  dialog <- messageDialogNew Nothing [DialogModal] MessageError ButtonsClose title
  messageDialogSetSecondaryText dialog body
  dialog `afterResponse` \_ -> widgetDestroy dialog
  widgetShowAll dialog

-- Converts an Either to an action in an ErrorT.
toET :: (Monad m, Error e') => (e -> e') -> Either e a -> ErrorT e' m a
toET f = either (throwError . f) return

-- Catches exceptions from an IO action, and maps them into ErrorT
etio :: (Error e', MonadIO io)
     => (Exception -> e') -> IO a -> ErrorT e' io a
etio f act = toET f =<< io (try act)

loadLogWith :: B WindowInfo -> FilePath -> B ()
loadLogWith act f = do
  ret <- runErrorT llw'
  case ret of
    Left e -> io $ displayError ("Could not read '" ++ f ++ "'") e
    Right () -> return ()

  where llw' = do
          input <- etio show $ readFile f
          log <- toET (("Parse error " ++) . show) $ readLog input
          shapes <- toET id $ process (upgrade log)
          misc <- lift act
          lift (displayLog misc f shapes)


maybeQuit :: B ()
maybeQuit = do
  n <- decWindows
  when (n == 0) (io mainQuit)

emptyWindow :: B WindowInfo
emptyWindow = do
  window <- mkWindow
  (menuBar, saveItem) <- mkMenuBar window
  layout <- io $ layoutNew Nothing Nothing
  nb <- io $ notebookNew

  io $ do
    vbox <- vBoxNew False 0
    containerAdd window vbox

    boxPackStart vbox menuBar PackNatural 0

    nb `set` [ notebookShowTabs := False
             , notebookShowBorder := False
             ]
    boxPackStart vbox nb PackGrow 0

    instructions <- labelNew Nothing
    labelSetMarkup instructions
        "<b>No diagram loaded</b>\n\n\
        \Having saved the output of <tt>bustle-dbus-monitor</tt> to a file,\n\
        \open that file to see a sequence diagram of D-Bus activity."
    notebookAppendPage nb instructions "Instructions"

    scrolledWindow <- scrolledWindowNew Nothing Nothing
    scrolledWindowSetPolicy scrolledWindow PolicyAutomatic PolicyAlways
    containerAdd scrolledWindow layout
    windowSetDefaultSize window 900 700

    notebookAppendPage nb scrolledWindow "Diagram"

    hadj <- layoutGetHAdjustment layout
    adjustmentSetStepIncrement hadj 50
    vadj <- layoutGetVAdjustment layout
    adjustmentSetStepIncrement vadj 50

    window `onKeyPress` \event -> case event of
        Key { eventKeyName=kn } -> print kn >> case kn of
          "Up"        -> decStep vadj
          "Down"      -> incStep vadj
          "Left"      -> decStep hadj
          "Right"     -> incStep hadj

          "Page_Down" -> incPage vadj
          "space"     -> incPage vadj
          "Page_Up"   -> decPage vadj
          _ -> return False
        _ -> return False

    widgetShowAll window

  incWindows
  return (window, saveItem, nb, layout)

displayLog :: WindowInfo -> FilePath -> Diagram -> B ()
displayLog (window, saveItem, nb, layout) filename shapes = do
  let (width, height) = dimensions shapes
      details = (filename, shapes)

  io $ do
    windowSetTitle window $ filename ++ " — D-Bus Sequence Diagram"

    widgetSetSensitivity saveItem True
    onActivateLeaf saveItem $ saveToPDFDialogue window details

    layoutSetSize layout (floor width) (floor height)
    layout `onExpose` update layout shapes

    notebookSetCurrentPage nb 1

    return ()

update :: Layout -> Diagram -> Event -> IO Bool
update layout shapes (Expose {}) = do
  win <- layoutGetDrawWindow layout

  hadj <- layoutGetHAdjustment layout
  hpos <- adjustmentGetValue hadj
  hpage <- adjustmentGetPageSize hadj

  vadj <- layoutGetVAdjustment layout
  vpos <- adjustmentGetValue vadj
  vpage <- adjustmentGetPageSize vadj

  let r = (hpos, vpos, hpos + hpage, vpos + vpage)

  renderWithDrawable win $ drawRegion r False shapes
  return True
update _layout _act _ = return False

-- Add/remove one step/page increment from an Adjustment, limited to the top of
-- the last page.
incStep, decStep, incPage, decPage :: Adjustment -> IO Bool
incStep = incdec (+) adjustmentGetStepIncrement
decStep = incdec (-) adjustmentGetStepIncrement
incPage = incdec (+) adjustmentGetPageIncrement
decPage = incdec (-) adjustmentGetPageIncrement

incdec :: (Double -> Double -> Double) -- How to combine the increment
       -> (Adjustment -> IO Double)    -- Action to discover the increment
       -> Adjustment
       -> IO Bool
incdec (+-) f adj = do
    pos <- adjustmentGetValue adj
    step <- f adj
    page <- adjustmentGetPageSize adj
    lim <- adjustmentGetUpper adj
    adjustmentSetValue adj $ min (pos +- step) (lim - page)
    return True

withIcon :: (Pixbuf -> IO ()) -> IO ()
withIcon act = do
  iconName <- getDataFileName "bustle.png"
  (pixbufNewFromFile iconName >>= act) `catchGError`
    \(GError _ _ msg) -> warn msg

mkWindow :: B Window
mkWindow = do
    window <- io windowNew

    io $ do
      windowSetTitle window "No document — D-Bus Sequence Diagram"
      withIcon (windowSetIcon window)

    embedIO $ onDestroy window . makeCallback maybeQuit

    return window

openDialogue :: Window -> B ()
openDialogue window = embedIO $ \r -> do
  chooser <- fileChooserDialogNew Nothing (Just window) FileChooserActionOpen
             [ ("gtk-cancel", ResponseCancel)
             , ("gtk-open", ResponseAccept)
             ]
  chooser `set` [ windowModal := True
                , fileChooserLocalOnly := True
                ]

  chooser `afterResponse` \response -> do
      when (response == ResponseAccept) $ do
          Just fn <- fileChooserGetFilename chooser
          makeCallback (loadInInitialWindow fn) r
      widgetDestroy chooser

  widgetShowAll chooser

saveToPDFDialogue :: Window -> Details -> IO ()
saveToPDFDialogue window (filename, shapes) = do
  chooser <- fileChooserDialogNew Nothing (Just window) FileChooserActionSave
             [ ("gtk-cancel", ResponseCancel)
             , ("gtk-save", ResponseAccept)
             ]
  chooser `set` [ windowModal := True
                , fileChooserLocalOnly := True
                , fileChooserDoOverwriteConfirmation := True
                ]

  let (dir, base) = splitFileName filename
  fileChooserSetCurrentFolder chooser dir
  fileChooserSetCurrentName chooser $ dropExtension base ++ ".pdf"

  chooser `afterResponse` \response -> do
      when (response == ResponseAccept) $ do
          Just fn <- io $ fileChooserGetFilename chooser
          let (width, height) = dimensions shapes
          withPDFSurface fn width height $
            \surface -> renderWith surface $ drawDiagram False shapes
      widgetDestroy chooser

  widgetShowAll chooser


mkMenuBar :: Window -> B (MenuBar, ImageMenuItem)
mkMenuBar window = embedIO $ \r -> do
  menuBar <- menuBarNew

  -- File menu
  file <- menuItemNewWithMnemonic "_File"
  fileMenu <- menuNew
  menuItemSetSubmenu file fileMenu

  openItem <- imageMenuItemNewFromStock stockOpen
  menuShellAppend fileMenu openItem
  onActivateLeaf openItem $ reconstruct r (openDialogue window)

  saveItem <- imageMenuItemNewFromStock stockSaveAs
  menuShellAppend fileMenu saveItem
  widgetSetSensitivity saveItem False

  menuShellAppend fileMenu =<< separatorMenuItemNew

  closeItem <- imageMenuItemNewFromStock stockClose
  menuShellAppend fileMenu closeItem
  closeItem `onActivateLeaf` widgetDestroy window

  menuShellAppend menuBar file

  -- Help menu
  help <- menuItemNewWithMnemonic "_Help"
  helpMenu <- menuNew
  menuItemSetSubmenu help helpMenu

  about <- imageMenuItemNewFromStock stockAbout
  menuShellAppend helpMenu about
  onActivateLeaf about $ do
      dialog <- aboutDialogNew

      license <- (Just `fmap` (readFile =<< getDataFileName "LICENSE"))
                 `catch` (\e -> warn (show e) >> return Nothing)

      dialog `set` [ aboutDialogName := "Bustle"
                   , aboutDialogVersion := showVersion version
                   , aboutDialogComments := "D-Bus activity visualiser"
                   , aboutDialogWebsite := "http://willthompson.co.uk/bustle"
                   , aboutDialogAuthors := authors
                   , aboutDialogCopyright := "© 2008–2009 Collabora Ltd."
                   , aboutDialogLicense := license
                   ]
      dialog `afterResponse` \response ->
          when (response == ResponseCancel) (widgetDestroy dialog)
      windowSetTransientFor dialog window
      windowSetModal dialog True
      withIcon (aboutDialogSetLogo dialog . Just)

      widgetShowAll dialog

  -- As long as I set *a* URL hook, the URL button works.
  aboutDialogSetUrlHook (const (return ()))
  -- but I have to actually do something in the email hook apparently.
  aboutDialogSetEmailHook mailto

  menuShellAppend menuBar help

  return (menuBar, saveItem)

authors :: [String]
authors = [ "Will Thompson <will.thompson@collabora.co.uk>"
          , "Dafydd Harries"
          , "Chris Lamb"
          , "Marc Kleine-Budde"
          ]

mailto :: String -> IO ()
mailto address = do
  let n = Nothing
  runProcess "xdg-open" ["mailto:" ++ address] n n n n n
  return ()

-- vim: sw=2 sts=2
