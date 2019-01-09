{-# LANGUAGE DeriveFunctor, OverloadedStrings, GeneralizedNewtypeDeriving #-}
{-
Bustle.Renderer: render nice Cairo diagrams from a list of D-Bus messages
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
module Bustle.Renderer
    (
    -- * Processing entire logs
      process
    , processWithFilters

    -- * Processing logs incrementally
    , RendererState
    , rendererStateNew
    , processSome

    -- * Output of processing
    , RendererResult(..)
    , Participants
    , sessionParticipants
    )
where

import Bustle.Types
import Bustle.Diagram
import Bustle.Regions

import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Map (Map)

import Control.Arrow (first)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Data.List (sort, sortBy)
import Data.Maybe (fromJust, fromMaybe, catMaybes)
import Data.Ord (comparing)

data Bus = SessionBus
         | SystemBus
    deriving (Show, Eq, Ord)

describeBus :: Bus -> String
describeBus SessionBus = "session"
describeBus SystemBus = "system"

-- We keep the column in the map to allow the Monoid instance to preserve the
-- ordering returned by sessionParticipants, which is the only view on this
-- type exported.
data Participants =
    Participants { pSession
                 , _pSystem :: Map (Double, UniqueName) (Set OtherName)
                 }
  deriving
    (Show, Eq)

instance Semigroup Participants where
    (<>) (Participants sess1 sys1) (Participants sess2 sys2) =
          Participants (f sess1 sess2)
                       (f sys1  sys2)
      where
        f = Map.unionWith Set.union

instance Monoid Participants where
    mempty = Participants Map.empty Map.empty

sessionParticipants :: Participants
                    -> [(UniqueName, Set OtherName)] -- ^ sorted by column
sessionParticipants = map (first snd) . Map.toAscList . pSession

data RendererResult apps =
    RendererResult { rrCentreOffset :: Double
                   , rrTopOffset :: Double -- ^ you shouldn't really need this outside of here.
                   , rrShapes :: [Shape]
                   , rrRegions :: Regions (Detailed Message)
                   , rrApplications :: apps
                   , rrWarnings :: [String]
                   }
  deriving
    (Show, Functor, Eq) -- Using Functor is a slight hack really

-- Yikes.
--
-- When combining two segments of a diagram, we may need to translate
-- one or other segment in either axis. For instance, if the first message
-- involves a service with only one bus name, but the second involves a service
-- with a hundred names, we're going to need a massive downwards translation to
-- shift the first set of messages down to match the second.
--
-- This is extremely unpleasant but it's a Monday. There's a test case in
-- Test/Renderer.hs because I don't trust myself.

instance Semigroup apps => Semigroup (RendererResult apps) where
    rr1 <> rr2 = RendererResult centreOffset topOffset shapes regions applications warnings
      where
        centreOffset = rrCentreOffset rr1 `max` rrCentreOffset rr2
        topOffset = rrTopOffset rr1 `max` rrTopOffset rr2

        shapes = shapes1 ++ shapes2
        versus x y = if x < y then Just (y - x) else Nothing
        translation rr = ( rrCentreOffset rr `versus` centreOffset
                         , rrTopOffset    rr `versus` topOffset
                         )
        translateShapes rr =
            case translation rr of
                -- Hooray for premature optimization
                (Nothing, Nothing) -> rrShapes rr
                (mx,      my) -> translateDiagram (fromMaybe 0 mx, fromMaybe 0 my) $ rrShapes rr
        shapes1 = translateShapes rr1
        shapes2 = translateShapes rr2

        translatedRegions rr =
            case snd $ translation rr of
                Nothing -> rrRegions rr
                Just  y -> translateRegions y $ rrRegions rr

        regions = translatedRegions rr1 ++ translatedRegions rr2

        applications = rrApplications rr1 <> rrApplications rr2
        warnings = rrWarnings rr1 <> rrWarnings rr2


instance Monoid apps => Monoid (RendererResult apps) where
    mempty = RendererResult 0 0 [] [] mempty []

processWithFilters :: (Log, NameFilter)
                   -> (Log, NameFilter)
                   -> RendererResult ()
processWithFilters (sessionBusLog, sessionFilter)
                   (systemBusLog,  systemFilter ) =
    void $ fst $ processSome sessionBusLog systemBusLog rs
  where
    rs = initialState sessionFilter systemFilter

process :: Log
        -> Log
        -> RendererResult Participants
process sessionBusLog systemBusLog =
    fst $ processSome sessionBusLog systemBusLog rendererStateNew

-- Doesn't let you filter
rendererStateNew :: RendererState
rendererStateNew = initialState emptyNameFilter emptyNameFilter

buildResult :: RendererOutput
            -> RendererState
            -> RendererResult Participants
buildResult (RendererOutput diagram messageRegions warnings) rs =
    RendererResult x y diagram' regions' participants warnings
  where
    (_translation@(x, y), diagram') = topLeftJustifyDiagram diagram
    regions' = translateRegions y messageRegions

    stripApps bs = Map.fromList [ ((column, u), aiEverNames ai)
                                | (u, ai) <- Map.assocs (apps bs)
                                , Just column <- [everColumn $ aiColumn ai]
                                ]
    sessionApps = stripApps $ sessionBusState rs
    systemApps = stripApps $ systemBusState rs
    participants = Participants sessionApps systemApps

processSome :: Log -- ^ freshly-arrived session bus messages
            -> Log -- ^ freshly-arrived system bus messages
            -> RendererState -- ^ the saved state from last time
            -> ( RendererResult Participants -- ^ the output from these messages
               , RendererState               -- ^ state to re-use later
               )
processSome sessionBusLog systemBusLog rs = (buildResult output rs', rs')
  where
    log' = combine sessionBusLog systemBusLog

    (output, rs') = runRenderer (mapM_ (uncurry processOne) log') rs

-- Combines a series of messages on the session bus and system bus into a
-- single ordered list, annotated by timestamp. Assumes both the source lists
-- are sorted.
combine :: Log -- ^ session bus messages
        -> Log -- ^ system bus messages
        -> [(Bus, DetailedEvent)]
combine [] [] = []
combine xs [] = zip (repeat SessionBus) xs
combine [] ys = zip (repeat SystemBus) ys
combine xs@(x:xs') ys@(y:ys') =
    if deTimestamp x < deTimestamp y
        then (SessionBus, x):combine xs' ys
        else (SystemBus, y):combine xs ys'

newtype Renderer a =
    Renderer (WriterT RendererOutput
                 (StateT RendererState Identity)
                    a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState RendererState
           , MonadWriter RendererOutput
           )

runRenderer :: Renderer ()
            -> RendererState
            -> ( RendererOutput
               , RendererState
               )
runRenderer (Renderer act) st = runIdentity $ runStateT (execWriterT act) st

data RendererOutput =
    RendererOutput ![Shape]
                   !(Regions (Detailed Message))
                   ![String]
  deriving
    (Show)

instance Semigroup RendererOutput where
    (<>) (RendererOutput s1 r1 w1)
         (RendererOutput s2 r2 w2) = RendererOutput (s1 ++ s2)
                                                       (r1 ++ r2)
                                                       (w1 ++ w2)
instance Monoid RendererOutput where
    mempty = RendererOutput [] [] []

data BusState =
    BusState { apps :: Applications
             , firstColumn :: Double
             , nextColumn :: Double
             , columnsInUse :: Set Double
             , pending :: Pending
             , bsFilter :: NameFilter
             }

data RendererState =
    RendererState { sessionBusState :: BusState
                  , systemBusState :: BusState
                  , row :: Double
                  , mostRecentLabels :: Double
                  , startTime :: Microseconds
                  }

initialBusState :: NameFilter
                -> Double
                -> BusState
initialBusState ignore x =
    BusState { apps = Map.empty
             , firstColumn = x
             , nextColumn = x
             , columnsInUse = Set.empty
             , pending = Map.empty
             , bsFilter = ignore
             }

initialSessionBusState, initialSystemBusState :: NameFilter -> BusState
initialSessionBusState f =
    initialBusState f $ timestampAndMemberWidth + firstColumnOffset
initialSystemBusState f =
    initialBusState f $ negate firstColumnOffset

initialState :: NameFilter
             -> NameFilter
             -> RendererState
initialState sessionFilter systemFilter = RendererState
    { sessionBusState = initialSessionBusState sessionFilter
    , systemBusState = initialSystemBusState systemFilter
    , row = 0
    , mostRecentLabels = 0
    , startTime = 0
    }

-- Maps unique connection name to the column representing that name, if
-- allocated, and a set of non-unique names for the connection, if any.
data Column = NoColumn
            | CurrentColumn Double
            | FormerColumn (Maybe Double)
  deriving
    Show

currentColumn :: Column
              -> Maybe Double
currentColumn (CurrentColumn x) = Just x
currentColumn _ = Nothing

everColumn :: Column
           -> Maybe Double
everColumn NoColumn          = Nothing
everColumn (CurrentColumn x) = Just x
everColumn (FormerColumn mx) = mx

data ApplicationInfo =
    ApplicationInfo { aiColumn :: Column
                    , aiCurrentNames :: Set OtherName
                    , aiEverNames :: Set OtherName
                    }
  deriving
    Show

aiCurrentColumn :: ApplicationInfo -> Maybe Double
aiCurrentColumn = currentColumn . aiColumn

type Applications = Map UniqueName ApplicationInfo

-- Map from a method call message to the coordinates at which the arc to its
-- return should start.
type Pending = Map (Detailed Message) (Double, Double)

getBusState :: Bus -> Renderer BusState
getBusState = getsBusState id

getsBusState :: (BusState -> a) -> Bus -> Renderer a
getsBusState f SessionBus = gets (f . sessionBusState)
getsBusState f SystemBus = gets (f . systemBusState)

modifyBusState :: Bus -> (BusState -> BusState) -> Renderer ()
modifyBusState bus f = case bus of
    SessionBus -> modify $ \rs -> rs { sessionBusState = f (sessionBusState rs)
                                     }
    SystemBus -> modify $ \rs -> rs { systemBusState = f (systemBusState rs)
                                    }

getApps :: Bus -> Renderer Applications
getApps bus = apps <$> getBusState bus

getsApps :: (Applications -> a) -> Bus -> Renderer a
getsApps f = getsBusState (f . apps)

lookupUniqueName :: Bus
                 -> UniqueName
                 -> Renderer ApplicationInfo
lookupUniqueName bus u = do
    thing <- getsApps (Map.lookup u) bus
    case thing of
        Just nameInfo -> return nameInfo
        -- This happens with pcap logs where we don't (currently) have
        -- explicit change notification for unique names in the stream of
        -- DetailedEvents.
        Nothing       -> addUnique bus u

lookupOtherName :: Bus
                -> OtherName
                -> Renderer (UniqueName, ApplicationInfo)
lookupOtherName bus o = do
    as <- getApps bus
    case filter (Set.member o . aiCurrentNames . snd) (Map.assocs as) of
        [details] -> return details

        -- No matches indicates a corrupt log, which we try to recover from …
        []        -> do
            warn $ concat [ "'"
                          , unOtherName o
                          , "' appeared unheralded on the "
                          , describeBus bus
                          , " bus; making something up..."
                          ]
            let namesInUse = Map.keys as
                candidates = map (fakeUniqueName . show)
                                 ([1..] :: [Integer])
                u = head $ filter (not . (`elem` namesInUse)) candidates
            addUnique bus u
            addOther bus o u
            ai <- lookupUniqueName bus u
            return (u, ai)

        -- … but more than one match means we've messed up. This can happen
        -- with logs generated by dbus-monitor --pcap, which doesn't perform
        -- an initial dump of all names on the bus.
        (d:ds)    -> do
            warn $ concat [ unOtherName o
                          , " owned by several apps: "
                          , show (d:ds)
                          ]
            return d

-- Finds a TaggedBusName in a map of applications
lookupApp :: Bus
          -> TaggedBusName
          -> Renderer (UniqueName, ApplicationInfo)
lookupApp bus name = case name of
    U u -> do
        details <- lookupUniqueName bus u
        return (u, details)
    O o -> lookupOtherName bus o

-- Finds a TaggedBusName in the current state, yielding its column if it exists.  If
-- it exists, but previously lacked a column, a column is allocated.
appCoordinate :: Bus -> TaggedBusName -> Renderer Double
appCoordinate bus n = do
    (u, details) <- lookupApp bus n
    case aiColumn details of
        NoColumn        -> assignColumn u (aiCurrentNames details)
        CurrentColumn x -> return x
        FormerColumn c  -> do
            warn $ show n ++ "(owned by " ++ show u ++ ") spontaneously reappeared"
            case c of
                Just x  -> return x
                Nothing -> assignColumn u (aiCurrentNames details)
  where assignColumn :: UniqueName -> Set OtherName -> Renderer Double
        assignColumn u os = do
            x <- nextColumn <$> getBusState bus

            -- FIXME: ick
            let f = case bus of
                    SessionBus -> (+ columnWidth)
                    SystemBus -> subtract columnWidth
            modifyBusState bus $ \bs -> bs { nextColumn = f x
                                           , columnsInUse = Set.insert x (columnsInUse bs)
                                           }
            modifyApps bus $ Map.adjust (\ai -> ai { aiColumn = CurrentColumn x }) u

            -- FIXME: Does this really live here?
            currentRow <- gets row
            let ns = bestNames u os
                h  = headerHeight ns
            shape $ Header ns x (currentRow - (10 + h))
            shape $ ClientLines (x :| []) (currentRow - 5) (currentRow + 15)

            return x

-- Modify the application table directly.
modifyApps :: Bus -> (Applications -> Applications) -> Renderer ()
modifyApps bus f = modifyBusState bus $ \bs -> bs { apps = f (apps bs) }

-- Updates the current set of applications in response to a well-known name's
-- owner changing.
updateApps :: Bus -- ^ bus on which a name's owner has changed
           -> OtherName -- name whose owner has changed.
           -> Change -- details of the change
           -> Renderer ()
updateApps bus n c = case c of
    Claimed new -> addOther bus n new
    Stolen old new -> remOther bus n old >> addOther bus n new
    Released old -> remOther bus n old

-- Adds a new unique name
addUnique :: Bus -> UniqueName -> Renderer ApplicationInfo
addUnique bus n = do
    let ai = ApplicationInfo NoColumn Set.empty Set.empty
    existing <- getsApps (Map.lookup n) bus
    forM_ existing $ const $ warn $ concat [ "Unique name '"
                                           , unUniqueName n
                                           , "' apparently connected to the bus twice"
                                           ]
    modifyApps bus $ Map.insert n ai
    return ai

-- Removes a unique name from the diagram. If we ever try to reuse columns
-- we'll have to revisit the FormerColumn concept to include a range of time.
remUnique :: Bus -> UniqueName -> Renderer ()
remUnique bus n = do
    ai <- lookupUniqueName bus n
    let mcolumn = aiCurrentColumn ai
    modifyApps bus $ Map.insert n (ai { aiColumn = FormerColumn mcolumn })
    forM_ mcolumn $ \x ->
        modifyBusState bus $ \bs ->
            bs { columnsInUse = Set.delete x (columnsInUse bs) }

addOther, remOther :: Bus -> OtherName -> UniqueName -> Renderer ()
-- Add a new well-known name to a unique name.
addOther bus n u = do
    ai <- lookupUniqueName bus u
    let ai' = ai { aiCurrentNames = Set.insert n (aiCurrentNames ai)
                 , aiEverNames = Set.insert n (aiEverNames ai)
                 }
    modifyApps bus $ Map.insert u ai'

-- Remove a well-known name from a unique name
remOther bus n u = do
    ai <- lookupUniqueName bus u
    let ai' = ai { aiCurrentNames = Set.delete n (aiCurrentNames ai) }
    modifyApps bus $ Map.insert u ai'

shape :: Shape -> Renderer ()
shape s = tellShapes [s]

tellShapes :: [Shape] -> Renderer ()
tellShapes ss = tell $ RendererOutput ss [] []

region :: Stripe -> Detailed Message -> Renderer ()
region r m = tell $ RendererOutput [] [(r, m)] []

warn :: String -> Renderer ()
warn warning = tell $ RendererOutput [] [] [warning]

modifyPending :: Bus
              -> (Pending -> Pending)
              -> Renderer ()
modifyPending bus f = modifyBusState bus $ \bs ->
    bs { pending = f (pending bs) }

addPending :: Bus
           -> Detailed Message
           -> Renderer ()
addPending bus m = do
    x <- destinationCoordinate bus m
    y <- gets row
    modifyPending bus $ Map.insert m (x, y)

findCallCoordinates :: Bus
                    -> Maybe (Detailed Message)
                    -> Renderer (Maybe (Detailed Message, (Double, Double)))
findCallCoordinates bus = maybe (return Nothing) $ \m -> do
    ret <- getsBusState (Map.lookup m . pending) bus
    modifyPending bus $ Map.delete m
    return $ fmap ((,) m) ret

-- The adjustments here leave space for a new app's headers to be drawn
-- without overlapping the rule.
getLeftMargin, getRightMargin :: Renderer Double
getLeftMargin =
    maybe 0 (subtract 35) <$> edgemostApp SystemBus
getRightMargin =
    maybe timestampAndMemberWidth (+ 35) <$> edgemostApp SessionBus

advanceBy :: Double -> Renderer ()
advanceBy d = do
    lastLabelling <- gets mostRecentLabels

    current' <- gets row

    when (current' - lastLabelling > 400) $ do
        xs <- (++) <$> getsApps Map.toList SessionBus
                   <*> getsApps Map.toList SystemBus
        let xs' = [ (x, bestNames u os)
                  | (u, ApplicationInfo (CurrentColumn x) os _) <- xs
                  ]
        let (height, ss) = headers xs' (current' + 20)
        tellShapes ss
        modify $ \bs -> bs { mostRecentLabels = current' + height + 10
                           , row = row bs + height + 10
                           }
    current <- gets row
    modify (\bs -> bs { row = row bs + d })
    next <- gets row

    leftMargin <- getLeftMargin
    rightMargin <- getRightMargin
    shape $ Rule leftMargin rightMargin (current + 15)

    let appColumns :: Applications -> [Double]
        appColumns = catMaybes . Map.foldr ((:) . aiCurrentColumn) []
    xs <- (++) <$> getsApps appColumns SessionBus
               <*> getsApps appColumns SystemBus
    case xs of
        (x:xs') -> shape $ ClientLines (x :| xs') (current + 15) (next + 15)
        _       -> return ()

bestNames :: UniqueName -> Set OtherName -> [String]
bestNames u os
    | Set.null os = [unUniqueName u]
    | otherwise   = (sortBy (flip (comparing length)) . map readable) $ Set.toList os
  where readable = reverse . takeWhile (/= '.') . reverse . unOtherName

edgemostApp :: Bus -> Renderer (Maybe Double)
edgemostApp bus = do
    columns <- getsBusState columnsInUse bus
    return $ if Set.null columns
        then Nothing
        else Just $ findMinMax columns
  where
    findMinMax = case bus of
        SessionBus -> Set.findMax
        SystemBus  -> Set.findMin

senderCoordinate :: Bus
                 -> Detailed Message
                 -> Renderer Double
senderCoordinate bus de = appCoordinate bus . sender $ deEvent de

destinationCoordinate :: Bus
                      -> Detailed Message
                      -> Renderer Double
destinationCoordinate bus de = appCoordinate bus . destination $ deEvent de

signalDestinationCoordinate :: Bus
                            -> Detailed Message
                            -> Renderer (Maybe Double)
signalDestinationCoordinate bus m =
    case signalDestination $ deEvent m of
        Nothing -> return Nothing
        Just n  -> Just <$> appCoordinate bus n

memberName :: Detailed Message
           -> Bool
           -> Renderer ()
memberName message isReturn = do
    current <- gets row
    let Member p i m = member $ deEvent message
    shape $ memberLabel p i m isReturn current

getTimeOffset :: Microseconds
              -> Renderer Microseconds
getTimeOffset µs = do
    base <- gets startTime
    if base == 0
      then do
        modify (\s -> s { startTime = µs })
        return 0
      else
        return (µs - base)

relativeTimestamp :: Detailed a -> Renderer ()
relativeTimestamp dm = do
    relative <- getTimeOffset (deTimestamp dm)
    current <- gets row
    shape $ timestampLabel (show (µsToMs relative) ++ "ms") current

returnArc :: Bus
          -> Detailed Message
          -> Double
          -> Double
          -> Microseconds
          -> Renderer ()
returnArc bus mr callx cally duration = do
    destinationx <- destinationCoordinate bus mr
    currentx     <- senderCoordinate bus mr
    currenty     <- gets row

    shape $ Arc { topx = callx, topy = cally
                , bottomx = currentx, bottomy = currenty
                , arcside = if destinationx > currentx then L else R
                , caption = show (µsToMs duration) ++ "ms"
                }

addMessageRegion :: Detailed Message
                 -> Renderer ()
addMessageRegion m = do
    newRow <- gets row

    -- FIXME: wtf. "row" points to the ... middle ... of the current row.
    region (Stripe (newRow - eventHeight / 2) (newRow + eventHeight / 2)) m

shouldShow :: Bus
           -> Message
           -> Renderer Bool
shouldShow bus m = do
    nameFilter <- getsBusState bsFilter bus
    names <- mapM (fmap fst . lookupApp bus) (mentionedNames m)
    return $
        not (any (flip Set.member $ nfNever nameFilter) names)
        && (Set.null (nfOnly nameFilter)
            || any (flip Set.member $ nfOnly nameFilter) names)

processOne :: Bus
           -> Detailed Event
           -> Renderer ()
processOne bus de = case deEvent de of
    NOCEvent n     -> processNOC bus n
    MessageEvent m -> processMessage bus (fmap (const m) de)

processMessage :: Bus
               -> Detailed Message
               -> Renderer ()
processMessage bus dm@(Detailed _ m _ _) = do
    orly <- shouldShow bus m
    when orly $ case m of
        Signal {}       -> do
            advance
            relativeTimestamp dm
            memberName dm False
            signal bus dm
            addMessageRegion dm

        MethodCall {}   -> do
            advance
            relativeTimestamp dm
            memberName dm False
            methodCall bus dm
            addPending bus dm
            addMessageRegion dm

        MethodReturn {} -> returnOrError $ methodReturn bus
        Error {}        -> returnOrError $ errorReturn bus

  where advance = advanceBy eventHeight -- FIXME: use some function of timestamp?
        returnOrError f = do
            call <- findCallCoordinates bus (inReplyTo m)
            forM_ call $ \(dm', (x,y)) -> do
                advance
                relativeTimestamp dm
                memberName dm' True
                f dm
                let duration = deTimestamp dm - deTimestamp dm'
                returnArc bus dm x y duration
                addMessageRegion dm

processNOC :: Bus
           -> NOC
           -> Renderer ()
processNOC bus noc =
    case noc of
        Connected { actor = u } -> void (addUnique bus u)
        Disconnected { actor = u } -> remUnique bus u
        NameChanged { changedName = n
                    , change = c
                    } -> updateApps bus n c

methodCall, methodReturn, errorReturn :: Bus
                                      -> Detailed Message
                                      -> Renderer ()
methodCall = methodLike Nothing Above
methodReturn = methodLike Nothing Below
errorReturn = methodLike (Just $ Colour 1 0 0) Below

methodLike :: Maybe Colour
           -> Arrowhead
           -> Bus
           -> Detailed Message
           -> Renderer ()
methodLike colour a bus dm = do
    sc <- senderCoordinate bus dm
    dc <- destinationCoordinate bus dm
    t <- gets row
    shape $ Arrow colour a sc dc t

signal :: Bus -> Detailed Message -> Renderer ()
signal bus dm = do
    t <- gets row
    emitter <- senderCoordinate bus dm
    mtarget <- signalDestinationCoordinate bus dm

    case mtarget of
        Just target ->
            shape $ DirectedSignalArrow emitter target t
        Nothing -> do
            -- fromJust is safe here because we must have an app to have a
            -- signal. It doesn't make me very happy though.
            outside <- fromJust <$> edgemostApp bus
            inside <- getsBusState firstColumn bus
            let [x1, x2] = sort [outside, inside]

            shape $ SignalArrow (x1 - 20) emitter (x2 + 20) t

-- vim: sw=2 sts=2
