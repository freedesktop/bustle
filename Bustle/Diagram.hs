{-
Bustle.Diagram: shapes for sequence diagrams
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
{-# LANGUAGE CPP #-}
module Bustle.Diagram
  ( Diagram

  -- Shapes, and smart constructors therefore
  , Shape(..)
  , memberLabel
  , timestampLabel
  , headers
  , headerHeight

  -- Attributes of shapes
  , Arrowhead(..)
  , Side(..)
  , Colour(..)
  , Rect

  -- Annoying constants that users of this module need.
  , columnWidth
  , timestampAndMemberWidth
  , firstColumnOffset

  -- Displaying diagrams
  , diagramDimensions
  , topLeftJustifyDiagram
  , drawDiagram
  , drawRegion

  -- Only for the icon drawing code
  , drawArcCurve
  )
where

import Data.Maybe (maybe)
import Data.List (unzip4)
import Control.Arrow ((&&&))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_)

import Control.Monad.Reader

import Graphics.Rendering.Cairo
import Graphics.UI.Gtk.Cairo (cairoCreateContext, showLayout)
#if MIN_VERSION_gtk(0,11,0)
import Graphics.Rendering.Pango.Layout
import Graphics.Rendering.Pango.Font
#else
import Graphics.UI.Gtk.Pango.Layout
import Graphics.UI.Gtk.Pango.Font
#endif

type Point = (Double, Double)
type Rect = (Double, Double, Double, Double)

data Arrowhead = Above | Below
  deriving (Eq, Show, Read, Ord)

above, below :: Arrowhead -> Bool
above Above = True
above Below = False
below = not . above

voffset :: Num a => Arrowhead -> (a -> a -> a)
voffset Above = (-)
voffset Below = (+)

data Side = L | R
  deriving (Eq, Show, Read, Ord)

offset :: Num a => Side -> (a -> a -> a)
offset L = (-)
offset R = (+)

data Colour = Colour Double Double Double
  deriving (Eq, Show, Read, Ord)

data Shape = Header { strs :: [String]
                    , shapex, shapey :: Double
                    }
           | MemberLabel { labelPath, labelMember :: String
                         , shapex :: Double -- The coordinates of the *centre*
                         , shapey :: Double -- of the label
                         }
           | TimestampLabel { str :: String
                            , shapex :: Double -- The coordinates of the
                            , shapey :: Double -- *centre* of the timestamp
                            }
           | ClientLine { shapex, shapey1, shapey2 :: Double }
           | Rule { shapex1, shapex2, shapey :: Double }
           | Arrow { shapecolour :: Maybe Colour
                   , arrowhead :: Arrowhead
                   , shapex1, shapex2, shapey :: Double
                   }
           | SignalArrow { shapex1, epicentre, shapex2, shapey :: Double }
           | Arc { topx, topy, bottomx, bottomy :: Double
                 , arcside :: Side
                 , caption :: String
                 }
  deriving (Show, Read, Eq)

-- Smart constructors for TimestampLabel and MemberLabel that fill in the
-- hardcoded (spit) x coordinates.
memberLabel :: String -> String -> Double -> Shape
memberLabel p m y = MemberLabel p m memberx y

timestampLabel :: String -> Double -> Shape
timestampLabel s y = TimestampLabel s timestampx y

type Diagram = [Shape]

arcControlPoints :: Shape -> (Point, Point)
arcControlPoints (Arc { topx=x1, topy=y1, bottomx=x2, bottomy=y2, arcside=s }) =
    let (+-) = offset s
        cp1 = (x1 +- 60, y1 + 10)
        cp2 = (x2 +- 60, y2 - 10)
    in (cp1, cp2)
arcControlPoints _ = error "i see you've played arcy-shapey before"

mapX, mapY :: (Double -> Double) -> (Shape -> Shape)
mapX f s = case s of
    Rule {}        -> s { shapex1 = f (shapex1 s)
                        , shapex2 = f (shapex2 s)
                        }
    Arrow {}       -> s { shapex1 = f (shapex1 s)
                        , shapex2 = f (shapex2 s)
                        }
    SignalArrow {} -> s { shapex1 = f (shapex1 s)
                        , epicentre = f (epicentre s)
                        , shapex2 = f (shapex2 s)
                        }
    Arc {}         -> s { topx = f (topx s)
                        , bottomx = f (bottomx s)
                        }
    _              -> s { shapex = f (shapex s) }

mapY f s = case s of
    Arc {}        -> s { topy = f (topy s)
                       , bottomy = f (bottomy s)
                       }
    ClientLine {} -> s { shapey1 = f (shapey1 s)
                       , shapey2 = f (shapey2 s)
                       }
    _             -> s { shapey = f (shapey s) }

--
-- Constants
--
eventHeight :: Double
eventHeight = 30

timestampx, timestampWidth :: Double
timestampx = 0 + timestampWidth / 2
timestampWidth = 60

memberx, memberWidth :: Double
memberx = timestampWidth + memberWidth / 2
memberWidth = 340

timestampAndMemberWidth :: Double
timestampAndMemberWidth = timestampWidth + memberWidth

columnWidth :: Double
columnWidth = 90

-- Method return arcs can go outside the first column. Empirically, 20 is
-- enough to stop the arc (or the duration text) overlapping the object path
-- etc.
firstColumnOffset :: Double
firstColumnOffset = 20 + columnWidth / 2

--
-- Calculating bounds of shapes
--
minMax :: Ord a => (a, a) -> (a, a)
minMax = uncurry min &&& uncurry max

xMinMax :: Shape -> (Double, Double)
xMinMax = minMax . (shapex1 &&& shapex2)

fromCentre :: Double -> Double -> Double -> Rect
fromCentre x y width =
    (x - width / 2, y - height / 2,
     x + width / 2, y + height / 2)
  where height = eventHeight

headerHeight :: [String] -> Double
headerHeight = fromIntegral . (10 *) . length

bounds :: Shape -> Rect
bounds s = case s of
  ClientLine {} -> (shapex s, shapey1 s, shapex s, shapey2 s)
  Rule {} -> (shapex1 s, shapey s, shapex2 s, shapey s)
  Arrow {} ->
    let (x1, x2) = xMinMax s
        y1 = shapey s - (if above (arrowhead s) then 5 else 0)
        y2 = shapey s + (if below (arrowhead s) then 5 else 0)
    in (x1, y1, x2, y2)
  SignalArrow {} ->
    let (x1, x2) = xMinMax s
        (y1, y2) = (subtract 5) &&& (+5) $ shapey s
    in (x1, y1, x2, y2)
  Arc { topx=x1, bottomx=x2, topy=y1, bottomy=y2 } ->
    let ((cx, _), (dx, _)) = arcControlPoints s
       -- FIXME: magic 5 makes the bounding box include the text
    in (min x1 cx, y1, max x2 dx, y2 + 5)
  TimestampLabel { shapex=x, shapey=y } -> fromCentre x y timestampWidth
  MemberLabel _ _ x y -> fromCentre x y memberWidth
  Header { strs = ss, shapex = x, shapey = y} ->
    let width = columnWidth
        height = headerHeight ss
    in (x - width / 2, y,
        x + width / 2, y + height)

intersects :: Rect -> Rect -> Bool
intersects (x,y,w,z) (x', y', w', z') =
  not $ or [x > w', w < x', y > z', z < y']

-- Constructs a series of headers of various-sized lists of names,
-- bottom-justified.
headers :: [(Double, [String])]  -- list of (x-coordinate, names)
        -> Double                -- y-coordinate of top of headers
        -> (Double, [Shape])     -- the headers' combined height, and shapes
headers []  _ = (0, [])
headers xss y = (height, shapes)
  where heights = map (headerHeight . snd) xss
        height  = maximum heights
        adjs    = map (height -) heights
        shapes  = zipWith (\(x, ss) adj -> Header ss x (y + adj)) xss adjs

--
-- Drawing
--

diagramBounds :: Diagram -> ((Double, Double), (Double, Double))
diagramBounds shapes = ((minimum (0:x1s), minimum (0:y1s))
                       ,(maximum (0:x2s), maximum (0:y2s))
                       )
  where
    (x1s, y1s, x2s, y2s) = unzip4 $ map bounds shapes

diagramDimensions :: Diagram -> (Double, Double)
diagramDimensions shapes = (x2 - x1, y2 - y1)
  where
    ((x1, y1), (x2, y2)) = diagramBounds shapes

topLeftJustifyDiagram :: Diagram -- ^ the original diagram
                      -> (Double, Diagram) -- ^ the diagram transformed to be
                                           --   in positive space, and the
                                           --   x-axis shift necessary to do so
topLeftJustifyDiagram shapes = (negate x1, shapes')
  where
    shapes'       = transformDiagram (negate x1, negate y1) shapes
    ((x1, y1), _) = diagramBounds shapes

transformDiagram :: (Double, Double) -> (Diagram -> Diagram)
transformDiagram (x, y) = map (mapX (+ x) . mapY (+ y))

drawDiagramInternal :: (Shape -> Bool) -- ^ A filter for the shapes
                    -> Bool -- ^ True to draw canvas items' bounding boxes
                            --   (for debugging)
                    -> Diagram   -- ^ A diagram to render
                    -> Render ()
drawDiagramInternal f drawBounds shapes = do
    clearCanvas

    forM_ (filter f shapes) $ \x -> do
        when drawBounds (drawBoundingBox x)
        draw x

drawDiagram :: Bool      -- ^ True to draw canvas items' bounding boxes (for
                         --   debugging)
            -> Diagram   -- ^ A diagram to render
            -> Render ()
drawDiagram = drawDiagramInternal (const True)

drawRegion :: Rect -> Bool -> Diagram -> Render ()
drawRegion r = drawDiagramInternal isVisible
    where isVisible = intersects r . bounds

saved :: Render () -> Render ()
saved act = save >> act >> restore

clearCanvas :: Render ()
clearCanvas = saved $ do
    setSourceRGB 1 1 1
    setOperator OperatorSource
    paint

drawBoundingBox :: Shape -> Render ()
drawBoundingBox s = saved $ do
    let (x,y,w,z) = bounds s
    setSourceRGB 0 0 1
    rectangle x y (w - x) (z - y)
    stroke

draw :: Shape -> Render ()
draw s = draw' s
  where draw' = case s of
          Arc {} -> let ((cx, cy), (dx, dy)) = arcControlPoints s
                    in drawArc cx cy dx dy <$>
                          topx <*> topy <*> bottomx <*> bottomy <*> caption
          SignalArrow {} -> drawSignalArrow <$> epicentre <*> shapex1 <*>
                              shapex2 <*> shapey
          Arrow {} -> drawArrow <$> shapecolour <*> arrowhead <*> shapex1 <*>
                        shapex2 <*> shapey
          Header {} -> drawHeader <$> strs <*> shapex <*> shapey
          MemberLabel {} -> drawMember <$> labelPath
                                       <*> labelMember
                                       <*> shapex
                                       <*> shapey
          TimestampLabel {} -> drawTimestamp <$> str
                                             <*> shapex
                                             <*> shapey
          ClientLine {} -> drawClientLine <$> shapex <*> shapey1 <*> shapey2
          Rule {} -> drawRule <$> shapex1
                              <*> shapex2
                              <*> shapey


halfArrowHead :: Arrowhead -> Bool -> Render ()
halfArrowHead a left = do
    (x,y) <- getCurrentPoint
    let x' = if left then x - 10 else x + 10
    let y' = voffset a y 5
    if left -- work around weird artifacts
      then moveTo x' y' >> lineTo x y
      else lineTo x' y' >> moveTo x y

arrowHead :: Bool -> Render ()
arrowHead left = halfArrowHead Above left >> halfArrowHead Below left

drawArrow :: Maybe Colour -> Arrowhead -> Double -> Double -> Double
          -> Render ()
drawArrow c a from to y = saved $ do
    maybe (return ()) (\(Colour r g b) -> setSourceRGB r g b) c

    moveTo from y
    lineTo to y
    halfArrowHead a (from < to)
    stroke

drawSignalArrow :: Double -> Double -> Double -> Double -> Render ()
drawSignalArrow e left right y = do
    newPath
    arc e y 5 0 (2 * pi)
    stroke

    moveTo left y
    arrowHead False
    lineTo (e - 5) y
    stroke

    moveTo (e + 5) y
    lineTo right y
    arrowHead True
    stroke

drawArcCurve :: Double -> Double -> Double -> Double
             -> Double -> Double -> Double -> Double
             -> Render ()
drawArcCurve cx cy dx dy x1 y1 x2 y2 = saved $ do
    setSourceRGB 0.4 0.7 0.4
    setDash [3, 3] 0

    moveTo x1 y1
    curveTo cx cy dx dy x2 y2
    stroke

drawArc :: Double -> Double -> Double -> Double
        -> Double -> Double -> Double -> Double
        -> String
        -> Render ()
drawArc cx cy dx dy x1 y1 x2 y2 cap = saved $ do
    drawArcCurve cx cy dx dy x1 y1 x2 y2

    setSourceRGB 0 0 0
    l <- mkLayout cap EllipsizeNone AlignLeft
    (PangoRectangle _ _ textWidth _, _) <- liftIO $ layoutGetExtents l
    let tx = min x2 dx + abs (x2 - dx) / 2
    moveTo (if x1 > cx then tx - textWidth else tx) (y2 - 5)
    showLayout l

font :: IO FontDescription
font = do
    fd <- fontDescriptionNew
    fontDescriptionSetSize fd 7
    fontDescriptionSetFamily fd "Sans"
    return fd

mkLayout :: (MonadIO m)
         => String -> EllipsizeMode -> LayoutAlignment
         -> m PangoLayout
mkLayout s e a = liftIO $ do
    ctx <- cairoCreateContext Nothing
    layout <- layoutEmpty ctx
    layoutSetMarkup layout s
    layoutSetFontDescription layout . Just =<< font
    layoutSetEllipsize layout e
    layoutSetAlignment layout a
    return layout

withWidth :: MonadIO m => m PangoLayout -> Double -> m PangoLayout
withWidth m w = do
    l <- m
    liftIO $ layoutSetWidth l (Just w)
    return l

drawHeader :: [String] -> Double -> Double -> Render ()
drawHeader names x y = forM_ (zip [0..] names) $ \(i, name) -> do
    l <- mkLayout name EllipsizeEnd AlignCenter `withWidth` columnWidth
    moveTo (x - (columnWidth / 2)) (y + i * h)
    showLayout l
  where h = 10

drawMember :: String -> String -> Double -> Double -> Render ()
drawMember s1 s2 x y = dm s1 (y - 10) >> dm s2 y
  where
    dm s y' = do
      l <- mkLayout s EllipsizeStart AlignLeft `withWidth` memberWidth
      moveTo (x - memberWidth / 2) y'
      showLayout l

drawTimestamp :: String -> Double -> Double -> Render ()
drawTimestamp ts x y = do
    moveTo (x - timestampWidth / 2) (y - 10)
    showLayout =<< mkLayout ts EllipsizeNone AlignLeft `withWidth` timestampWidth

drawClientLine :: Double -> Double -> Double -> Render ()
drawClientLine x y1 y2 = saved $ do
    setSourceRGB 0.7 0.7 0.7
    moveTo x y1
    lineTo x y2
    stroke

drawRule :: Double -> Double -> Double -> Render ()
drawRule x1 x2 y = saved $ do
    setSourceRGB 0.9 0.9 0.9
    setLineWidth 0.5

    moveTo x1 y
    lineTo x2 y
    stroke

-- vim: sw=2 sts=2
