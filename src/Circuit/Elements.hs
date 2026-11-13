-- | Circuit elements: the data model and drawing of the first components
-- (battery, resistor, capacitor, inductor, diode), ported from Paul Falstad's
-- Java circuit simulator.
--
-- Coordinates are in the same "point" space as the drawing area; the origin is
-- the top-left corner and the Y axis grows downwards, matching libui and the
-- original applet.
module Circuit.Elements
  ( Point,
    ComponentKind (..),
    componentName,
    defaultValue,
    valueUnit,
    Element (..),
    gridSize,
    snapGrid,
    distanceToSegment,
    interpPoint,
    compColor,
    voltageColor,
    drawElement,
  )
where

import UI

-- | A point in the circuit canvas, in points.
type Point = (Double, Double)

-- | The kinds of components implemented so far.
data ComponentKind
  = Wire
  | Battery
  | Resistor
  | Capacitor
  | Inductor
  | Diode
  deriving (Eq, Show)

componentName :: ComponentKind -> String
componentName Wire = "Wire"
componentName Battery = "Voltage Source"
componentName Resistor = "Resistor"
componentName Capacitor = "Capacitor"
componentName Inductor = "Inductor"
componentName Diode = "Diode"

-- | Default value of a component's primary parameter (Ω, F, H or V).
defaultValue :: ComponentKind -> Double
defaultValue Wire = 1e-6
defaultValue Battery = 5.0
defaultValue Resistor = 100.0
defaultValue Capacitor = 1e-5
defaultValue Inductor = 1.0
defaultValue Diode = 0.805904783

-- | A human-readable unit suffix for a component's value.
valueUnit :: ComponentKind -> String
valueUnit Wire = "Ω"
valueUnit Battery = "V"
valueUnit Resistor = "Ω"
valueUnit Capacitor = "F"
valueUnit Inductor = "H"
valueUnit Diode = "V"

-- | A placed element: a kind, its parameter value and its two endpoints.
data Element = Element
  { elemKind :: ComponentKind,
    elemValue :: Double,
    elemP1 :: Point,
    elemP2 :: Point
  }
  deriving (Eq, Show)

-- | Grid spacing in points.
gridSize :: Int
gridSize = 16

-- | Snaps a coordinate to the grid (rounds to the nearest multiple of
-- 'gridSize', matching the original's @snapGrid@).
snapGrid :: Double -> Double
snapGrid x = fromIntegral (((round x + 7) `div` gridSize) * gridSize)

-- Colors ----------------------------------------------------------------

-- | Body/lead color for components.
compColor :: RGBA
compColor = (0.85, 0.85, 0.85, 1.0)

-- | Post (connection dot) color.
postColor :: RGBA
postColor = (1.0, 1.0, 1.0, 1.0)

-- Geometry helpers ------------------------------------------------------

distance :: Point -> Point -> Double
distance (ax, ay) (bx, by) = sqrt ((bx - ax) ** 2 + (by - ay) ** 2)

-- | Point at fraction @f@ along @a -> b@, offset @g@ points perpendicular.
interpPoint :: Point -> Point -> Double -> Double -> Point
interpPoint (ax, ay) (bx, by) f g =
  let gx = by - ay
      gy = ax - bx
      glen = sqrt (gx * gx + gy * gy)
      (nx, ny) = if glen > 0 then (gx / glen, gy / glen) else (0, 0)
   in (ax * (1 - f) + bx * f + g * nx, ay * (1 - f) + by * f + g * ny)

-- | The two points at fraction @f@ along @a -> b@, offset @±g@ perpendicular.
interpPoint2 :: Point -> Point -> Double -> Double -> (Point, Point)
interpPoint2 a b f g = (interpPoint a b f g, interpPoint a b f (-g))

-- | Two "lead" points spaced @len@ apart and centered on the midpoint of the
-- segment. Falls back to the endpoints when the segment is too short.
calcLeads :: Point -> Point -> Double -> (Point, Point)
calcLeads p1 p2 len
  | d < len || len == 0 = (p1, p2)
  | otherwise =
      ( interpPoint p1 p2 ((d - len) / (2 * d)) 0,
        interpPoint p1 p2 ((d + len) / (2 * d)) 0
      )
  where
    d = distance p1 p2

-- | Distance from point @p@ to the segment @a-b@.
distanceToSegment :: Point -> Point -> Point -> Double
distanceToSegment (px, py) (ax, ay) (bx, by) =
  let dx = bx - ax
      dy = by - ay
      len2 = dx * dx + dy * dy
      t = if len2 == 0 then 0 else max 0 (min 1 (((px - ax) * dx + (py - ay) * dy) / len2))
      cx = ax + t * dx
      cy = ay + t * dy
   in sqrt ((px - cx) ** 2 + (py - cy) ** 2)

-- Drawing ---------------------------------------------------------------

-- | Draws an element with the given body color. Degenerate (zero-length)
-- elements draw only their posts.
drawElement :: DrawContext -> RGBA -> ComponentKind -> Point -> Point -> IO ()
drawElement ctx color kind p1 p2
  | distance p1 p2 < 1.0 = drawPosts ctx p1 p2
  | otherwise =
      case kind of
        Wire -> drawWire ctx color p1 p2
        Battery -> drawBattery ctx color p1 p2
        Resistor -> drawResistor ctx color p1 p2
        Capacitor -> drawCapacitor ctx color p1 p2
        Inductor -> drawInductor ctx color p1 p2
        Diode -> drawDiode ctx color p1 p2

-- Wire: a plain thick line between the posts.
drawWire :: DrawContext -> RGBA -> Point -> Point -> IO ()
drawWire ctx color p1 p2 = do
  drawLine ctx p1 p2 color 2
  drawPosts ctx p1 p2

-- | Draws the two connection dots.
drawPosts :: DrawContext -> Point -> Point -> IO ()
drawPosts ctx p1 p2 = do
  fillCircle ctx p1 3.5 postColor
  fillCircle ctx p2 3.5 postColor

-- Battery: two parallel plates (short = +, long = -) with leads.
drawBattery :: DrawContext -> RGBA -> Point -> Point -> IO ()
drawBattery ctx color p1 p2 = do
  let (lead1, lead2) = calcLeads p1 p2 8
      (pos1, pos2) = interpPoint2 lead1 lead2 0 10
      (neg1, neg2) = interpPoint2 lead1 lead2 1 16
  drawLine ctx p1 lead1 color 2
  drawLine ctx lead2 p2 color 2
  drawLine ctx pos1 pos2 color 2
  drawLine ctx neg1 neg2 color 2
  drawPosts ctx p1 p2

-- Resistor: a zigzag between the two leads.
drawResistor :: DrawContext -> RGBA -> Point -> Point -> IO ()
drawResistor ctx color p1 p2 = do
  let (lead1, lead2) = calcLeads p1 p2 32
  drawLine ctx p1 lead1 color 2
  drawLine ctx lead2 p2 color 2
  zigzag lead1 lead2 0 0
  drawPosts ctx p1 p2
  where
    hs :: Double
    hs = 8.0
    segments :: Int
    segments = 16
    segf :: Double
    segf = 1.0 / 16.0
    zigzag :: Point -> Point -> Int -> Int -> IO ()
    zigzag a b ox i
      | i >= segments = pure ()
      | otherwise = do
          let nx = case i `mod` 4 of
                0 -> 1
                2 -> -1
                _ -> 0
              pA = interpPoint a b (fromIntegral i * segf) (fromIntegral ox * hs)
              pB = interpPoint a b (fromIntegral (i + 1) * segf) (fromIntegral nx * hs)
          drawLine ctx pA pB color 2
          zigzag a b nx (i + 1)

-- Capacitor: two parallel plates with leads.
drawCapacitor :: DrawContext -> RGBA -> Point -> Point -> IO ()
drawCapacitor ctx color p1 p2 = do
  let d = distance p1 p2
      f = (d / 2 - 4) / d
      lead1 = interpPoint p1 p2 f 0
      lead2 = interpPoint p1 p2 (1 - f) 0
      (pa1, pa2) = interpPoint2 p1 p2 f 12
      (pb1, pb2) = interpPoint2 p1 p2 (1 - f) 12
  drawLine ctx p1 lead1 color 2
  drawLine ctx p2 lead2 color 2
  drawLine ctx pa1 pa2 color 2
  drawLine ctx pb1 pb2 color 2
  drawPosts ctx p1 p2

-- Inductor: a coil between the two leads.
drawInductor :: DrawContext -> RGBA -> Point -> Point -> IO ()
drawInductor ctx color p1 p2 = do
  let hs = 8.0
      (lead1, lead2) = calcLeads p1 p2 32
  drawLine ctx p1 lead1 color 2
  drawLine ctx lead2 p2 color 2
  drawCoil ctx color lead1 lead2 hs
  drawPosts ctx p1 p2

-- | Draws a coil (series of semicircular arcs) between @a@ and @b@.
drawCoil :: DrawContext -> RGBA -> Point -> Point -> Double -> IO ()
drawCoil ctx color a b hs = go a (0 :: Int)
  where
    segments :: Int
    segments = 30
    segf :: Double
    segf = 1.0 / 30.0
    go :: Point -> Int -> IO ()
    go prev i
      | i >= segments = pure ()
      | otherwise = do
          let t = fromIntegral (i + 1) * 6.0 * segf
              cx = javaMod t 2.0 - 1.0
              hsx = sqrt (max 0 (1 - cx * cx))
              pt = interpPoint a b (fromIntegral i * segf) (hsx * hs)
          drawLine ctx prev pt color 2
          go pt (i + 1)

-- | Java's @%@ operator (remainder) for non-negative doubles.
javaMod :: Double -> Double -> Double
javaMod x m = x - fromIntegral (floor (x / m) :: Int) * m

-- Diode: filled triangle (arrow) plus the cathode bar.
drawDiode :: DrawContext -> RGBA -> Point -> Point -> IO ()
drawDiode ctx color p1 p2 = do
  let hs = 8.0
      (lead1, lead2) = calcLeads p1 p2 16
      (a1, a2) = interpPoint2 lead1 lead2 0 hs
      (c1, c2) = interpPoint2 lead1 lead2 1 hs
  drawLine ctx p1 lead1 color 2
  drawLine ctx lead2 p2 color 2
  fillTriangle ctx a1 a2 lead2 color
  drawLine ctx c1 c2 color 2
  drawPosts ctx p1 p2

-- | Maps a node voltage to the classic red/green voltage color scale.
voltageColor :: Double -> RGBA
voltageColor volts =
  let v = clamp (-1) 1 (volts / 5.0)
      (r, g) = if v < 0
        then (128 * (-v) + 127, 127 * (1 + v))
        else (127 * (1 - v), 128 * v + 127)
   in (r / 255, g / 255, g / 255, 1.0)
  where
    clamp lo hi x = max lo (min hi x)
