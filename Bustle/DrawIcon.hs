module Main where

import Graphics.Rendering.Cairo
import Bustle.Diagram

width, height :: Num a => a
width = 64
height = width

halfwayAcross = width / 2
halfwayDown = height / 2

scaleFactor = 4

main = do
    withImageSurface FormatARGB32 (width * scaleFactor) (height * scaleFactor) $ \surface -> do
      renderWith surface $ do
        scale 3.8 3.8

        --setSourceRGBA 1 1 1 0
        --rectangle 0 0 width height
        --Graphics.Rendering.Cairo.fill

        drawDiagram False [SignalArrow 0 halfwayAcross width halfwayDown]
        drawArcCurve 0 0 width 0 0 halfwayDown width halfwayDown

        -- setSourceRGB 0 0 0
        -- shape $ SignalArrow (x1 - 20) x (x2 + 20) t
        -- signalArrow 25 22 29 33

        -- setSourceRGB 0.4 0.7 0.4
        -- setDash [6, 3] 0

        -- moveTo 2 33
        -- curveTo 2 2 49 2 49 33
        -- stroke

        -- setSourceRGB 0 0 0
        -- setDash [] 0

      surfaceWriteToPNG surface "bustle-256.png"
