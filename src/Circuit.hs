-- | Circuit simulator interface, ported from the Java applet by Paul Falstad
-- (https://github.com/kojix2/circuit-simulator).
--
-- Implements the window layout, drawing and placement of components, a
-- select/drag mode, and a live simulation backed by ngspice: the circuit is
-- turned into a SPICE netlist, ngspice streams a transient in a background
-- thread, and the canvas is colored/labelled from the latest values.
module Circuit (run) where

import Control.Monad (forM_, when)
import Data.IORef
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Text.Printf (printf)
import Text.Read (readMaybe)
import UI
import Circuit.Elements
import Circuit.Spice

-- | Information about an in-progress drag of an existing element.
data DragInfo = DragInfo
  { dragIndex :: Int, -- ^ index of the dragged element in 'placed'
    dragAnchor :: Point, -- ^ snapped mouse-down position
    dragOriginal :: Element -- ^ element before dragging
  }

-- | Mutable state of the editor.
data CircuitState = CircuitState
  { placed :: [Element], -- ^ committed elements (newest first)
    pending :: Maybe Element, -- ^ element being dragged into place
    selectedKind :: Maybe ComponentKind, -- ^ component selected in the menu
    dragInfo :: Maybe DragInfo -- ^ element being moved in select mode
  }

-- | Mutable state of the running ngspice simulation.
data SpiceState = SpiceState
  { spElements :: [SpiceElement], -- ^ parallel to 'placed'
    spFrameCount :: Int, -- ^ number of time steps in the trajectory
    spFrame :: Int, -- ^ current playback frame
    spNodeVoltages :: [[Double]], -- ^ @!! frame !! node@
    spCurrents :: [[Double]] -- ^ @!! frame !! element@
  }

-- | Code of the Escape key in libui's @uiExtKey@ enumeration.
escapeKey :: Int
escapeKey = 1 -- uiExtKeyEscape

-- | A series RLC loop (battery, resistor, inductor, capacitor) used as the
-- built-in example.
rlcCircuit :: [Element]
rlcCircuit =
  [ Element Battery 5 (240, 144) (240, 416), -- left side (post 1 = negative, ground)
    Element Resistor 100 (240, 144) (560, 144), -- top
    Element Inductor 1 (560, 144) (560, 416), -- right side
    Element Capacitor 1e-5 (560, 416) (240, 416) -- bottom
  ]

-- | Launches the circuit simulator interface.
run :: IO ()
run = do
  hSetBuffering stdout LineBuffering
  initUI
  initSpice

  stateRef <- newIORef (CircuitState [] Nothing Nothing Nothing)
  spiceRef <- newIORef Nothing
  stoppedRef <- newIORef False
  showValuesRef <- newIORef False
  titleLabel <- label "Select/Drag mode"
  canvasRef <- newIORef Nothing

  -- Menus must be created before any window: on macOS libui finalizes the
  -- menu bar when the first window is created.
  buildMenus stateRef titleLabel canvasRef spiceRef showValuesRef

  w <- window "Circuit Simulator v1.6.1a" 860 640
  setMargined w True

  canvas <-
    area
      (drawCircuit stateRef spiceRef showValuesRef)
      (handleMouse stateRef canvasRef spiceRef)
      (handleKey stateRef titleLabel canvasRef)
  writeIORef canvasRef (Just canvas)

  toolbar <- buildToolbar titleLabel stoppedRef

  root <- vbox
  setPadded root True
  append root toolbar
  appendStretchy root canvas True
  setChild w root

  onClose w quit
  showWidget w

  -- The simulation timer runs forever; it idles while "Stopped" is checked.
  timer 16 (timerTick spiceRef stoppedRef canvasRef)

  uiMain
  uninitUI

-- Menu bar --------------------------------------------------------------

-- | A declarative description of a menu entry.
data Entry
  = Item String -- ^ plain item that logs its name when clicked
  | ItemAct String (IO ()) -- ^ item with a custom action
  | Check String -- ^ checkable item
  | Header String -- ^ disabled (grayed) section header
  | Sep -- ^ separator

buildMenus :: IORef CircuitState -> Control -> IORef (Maybe Area) -> IORef (Maybe SpiceState) -> IORef Bool -> IO ()
buildMenus stateRef titleLabel canvasRef spiceRef showValuesRef = do
  buildMenu "File"
    [ Item "Import",
      Item "Export",
      Sep,
      ItemAct "Exit" quit
    ]
  buildMenu "Edit"
    [ Item "Undo",
      Item "Redo",
      Sep,
      Item "Cut",
      Item "Copy",
      Item "Paste",
      Sep,
      Item "Select All"
    ]
  buildMenu "Scope"
    [ Item "Stack All",
      Item "Unstack All"
    ]
  buildOptionsMenu showValuesRef
  buildCircuitsMenu stateRef titleLabel canvasRef spiceRef
  buildComponentsMenu stateRef titleLabel

-- The Options menu. "Show Values" toggles the on-canvas value labels.
buildOptionsMenu :: IORef Bool -> IO ()
buildOptionsMenu showValuesRef = do
  m <- menu "Options"
  plain m "Show Current"
  plain m "Show Voltage"
  plain m "Show Power"
  values <- checkMenuItem m "Show Values"
  onMenuItemClicked values (modifyIORef' showValuesRef not)
  plain m "Small Grid"
  plain m "European Resistors"
  plain m "White Background"
  plain m "Conventional Current Motion"
  plain m "Ideal Wires"
  menuSeparator m
  other <- menuItem m "Other Options..."
  onMenuItemClicked other (logClick "Other Options...")
  where
    plain mm name = do
      it <- checkMenuItem mm name
      onMenuItemClicked it (logClick name)

-- The components implemented so far, keyed by their menu label.
supportedComponents :: [(String, ComponentKind)]
supportedComponents =
  [ ("Add Wire", Wire),
    ("Add Resistor", Resistor),
    ("Add Capacitor", Capacitor),
    ("Add Inductor", Inductor),
    ("Add Diode", Diode),
    ("Add Voltage Source (2-terminal)", Battery)
  ]

-- The "Circuits" menu of example circuits.
buildCircuitsMenu :: IORef CircuitState -> Control -> IORef (Maybe Area) -> IORef (Maybe SpiceState) -> IO ()
buildCircuitsMenu stateRef titleLabel canvasRef spiceRef = do
  m <- menu "Circuits"
  add m "Blank Circuit" (loadExample stateRef titleLabel canvasRef spiceRef [])
  add m "RLC Circuit" (loadExample stateRef titleLabel canvasRef spiceRef rlcCircuit)
  add m "RC Filter" (notImplemented "RC Filter")
  add m "Inverting Amplifier" (notImplemented "Inverting Amplifier")
  add m "555 Timer" (notImplemented "555 Timer")
  where
    add mm name act = do
      it <- menuItem mm name
      onMenuItemClicked it act

    notImplemented name = putStrLn ("not implemented: " ++ name)

-- Replaces the current circuit with the given example elements.
loadExample :: IORef CircuitState -> Control -> IORef (Maybe Area) -> IORef (Maybe SpiceState) -> [Element] -> IO ()
loadExample stateRef titleLabel canvasRef spiceRef els = do
  modifyIORef' stateRef $ \s ->
    s
      { placed = els,
        pending = Nothing,
        selectedKind = Nothing,
        dragInfo = Nothing
      }
  setLabelText titleLabel (if null els then "Blank circuit" else "RLC Circuit")
  runSpice stateRef spiceRef
  redraw canvasRef
  putStrLn ("loaded circuit: " ++ if null els then "blank" else "RLC")

-- libui-ng has no nested submenus, so the component palette is a single flat
-- menu organized with disabled headers and separators.
buildComponentsMenu :: IORef CircuitState -> Control -> IO ()
buildComponentsMenu stateRef titleLabel = do
  m <- menu "Components"
  add m "Add Wire"
  add m "Add Resistor"
  sep m

  header m "Passive Components"
  mapM_ (add m)
    [ "Add Capacitor",
      "Add Inductor",
      "Add Switch",
      "Add Push Switch",
      "Add SPDT Switch",
      "Add Potentiometer",
      "Add Transformer",
      "Add Tapped Transformer",
      "Add Transmission Line",
      "Add Relay",
      "Add Memristor",
      "Add Spark Gap"
    ]
  sep m

  header m "Inputs/Outputs"
  mapM_ (add m)
    [ "Add Ground",
      "Add Voltage Source (2-terminal)",
      "Add A/C Source (2-terminal)",
      "Add Voltage Source (1-terminal)",
      "Add A/C Source (1-terminal)",
      "Add Square Wave (1-terminal)",
      "Add Analog Output",
      "Add Logic Input",
      "Add Logic Output",
      "Add Clock",
      "Add A/C Sweep",
      "Add Var. Voltage",
      "Add Antenna",
      "Add Current Source",
      "Add LED",
      "Add Lamp (beta)"
    ]
  sep m

  header m "Active Components"
  mapM_ (add m)
    [ "Add Diode",
      "Add Zener Diode",
      "Add Transistor (bipolar, NPN)",
      "Add Transistor (bipolar, PNP)",
      "Add Op Amp (- on top)",
      "Add Op Amp (+ on top)",
      "Add MOSFET (n-channel)",
      "Add MOSFET (p-channel)",
      "Add JFET (n-channel)",
      "Add JFET (p-channel)",
      "Add Analog Switch (SPST)",
      "Add Analog Switch (SPDT)",
      "Add SCR",
      "Add Tunnel Diode",
      "Add Triode",
      "Add CCII+",
      "Add CCII-"
    ]
  sep m

  header m "Logic Gates"
  mapM_ (add m)
    [ "Add Inverter",
      "Add NAND Gate",
      "Add NOR Gate",
      "Add AND Gate",
      "Add OR Gate",
      "Add XOR Gate"
    ]
  sep m

  header m "Chips"
  mapM_ (add m)
    [ "Add D Flip-Flop",
      "Add JK Flip-Flop",
      "Add 7 Segment LED",
      "Add VCO",
      "Add Phase Comparator",
      "Add Counter",
      "Add Decade Counter",
      "Add 555 Timer",
      "Add DAC",
      "Add ADC",
      "Add Latch"
    ]
  sep m

  header m "Other"
  mapM_ (add m)
    [ "Add Text",
      "Add Box",
      "Add Scope Probe",
      "Drag All (Alt-drag)",
      "Drag Row",
      "Drag Column",
      "Drag Selected",
      "Drag Post"
    ]
  sep m

  add m "Select/Drag Selected"
  where
    add mm name = do
      it <- menuItem mm name
      onMenuItemClicked it (selectComponent name)

    header mm name = do
      it <- menuItem mm name
      disableMenuItem it

    sep mm = menuSeparator mm

    -- Choosing a component selects it for placement; anything else just logs.
    selectComponent name =
      case lookup name supportedComponents of
        Just kind -> do
          modifyIORef' stateRef $ \s -> s {selectedKind = Just kind}
          setLabelText titleLabel name
          putStrLn ("selected: " ++ name)
        Nothing -> do
          modifyIORef' stateRef $ \s -> s {selectedKind = Nothing}
          setLabelText titleLabel (name ++ " (not implemented)")
          putStrLn ("not implemented: " ++ name)

buildMenu :: String -> [Entry] -> IO ()
buildMenu name entries = do
  m <- menu name
  forM_ entries (buildEntry m)

buildEntry :: Menu -> Entry -> IO ()
buildEntry m (Item name) = do
  it <- menuItem m name
  onMenuItemClicked it (logClick name)
buildEntry m (ItemAct name act) = do
  it <- menuItem m name
  onMenuItemClicked it act
buildEntry m (Check name) = do
  it <- checkMenuItem m name
  onMenuItemClicked it (logClick name)
buildEntry m (Header name) = do
  it <- menuItem m name
  disableMenuItem it
buildEntry m Sep = menuSeparator m

logClick :: String -> IO ()
logClick name = putStrLn ("clicked: " ++ name)

-- Toolbar ---------------------------------------------------------------

buildToolbar :: Control -> IORef Bool -> IO Box
buildToolbar titleLabel stoppedRef = do
  tb <- hbox
  setPadded tb True

  resetBtn <- button "Reset" (putStrLn "Reset clicked")
  append tb resetBtn

  stopped <- checkbox "Stopped" (modifyIORef' stoppedRef not)
  append tb stopped

  append tb =<< label "Simulation Speed"
  speed <- slider 0 260 (pure ())
  setSliderValue speed 3
  append tb speed

  append tb =<< label "Current Speed"
  current <- slider 1 100 (pure ())
  setSliderValue current 50
  append tb current

  append tb =<< label "Current Circuit:"
  append tb titleLabel

  pure tb

-- Canvas ----------------------------------------------------------------

drawCircuit :: IORef CircuitState -> IORef (Maybe SpiceState) -> IORef Bool -> DrawContext -> Double -> Double -> IO ()
drawCircuit stateRef spiceRef showValuesRef ctx w h = do
  -- Dark background, like the original circuit canvas.
  fillRect ctx 0 0 w h (0.08, 0.08, 0.08, 1.0)

  -- Grid: minor lines every 16 points, major lines every 5th line.
  let grid = 16.0 :: Double
      minor = (0.16, 0.16, 0.16, 1.0) :: RGBA
      major = (0.26, 0.26, 0.26, 1.0) :: RGBA
      cols = ceiling (w / grid) :: Int
      rows = ceiling (h / grid) :: Int

  forM_ [0 .. cols] $ \i -> do
    let x = fromIntegral i * grid
        color = if i `mod` 5 == 0 then major else minor
    drawLine ctx (x, 0) (x, h) color 1.0

  forM_ [0 .. rows] $ \i -> do
    let y = fromIntegral i * grid
        color = if i `mod` 5 == 0 then major else minor
    drawLine ctx (0, y) (w, y) color 1.0

  -- Committed elements, colored by node voltage from ngspice, then the
  -- element being dragged.
  s <- readIORef stateRef
  spiceM <- readIORef spiceRef
  showValues <- readIORef showValuesRef
  case spiceM of
    Just sp
      | spFrameCount sp > 0 && length (spElements sp) == length (placed s) ->
          let vn = spNodeVoltages sp !! spFrame sp
              curr = spCurrents sp !! spFrame sp
           in forM_ (zip3 (placed s) (spElements sp) [0 ..]) $ \(el, se, idx) -> do
                let n0 = seNode0 se
                    n1 = seNode1 se
                    color = voltageColor ((vn !! n0 + vn !! n1) / 2)
                drawElement ctx color (elemKind el) (elemP1 el) (elemP2 el)
                when showValues $ do
                  let vd = vn !! n0 - vn !! n1
                      (vx, vy) = interpPoint (elemP1 el) (elemP2 el) 0.5 14
                      (ix, iy) = interpPoint (elemP1 el) (elemP2 el) 0.5 (-14)
                  drawText ctx (vx, vy) 12 (1, 1, 1, 0.9) (formatVoltage vd)
                  drawText ctx (ix, iy) 12 (1, 1, 1, 0.9) (formatCurrent (curr !! idx))
    _ ->
      forM_ (placed s) $ \el ->
        drawElement ctx compColor (elemKind el) (elemP1 el) (elemP2 el)

  forM_ (pending s) $ \el ->
    drawElement ctx compColor (elemKind el) (elemP1 el) (elemP2 el)

-- Mouse handling --------------------------------------------------------

-- In add mode, placement is press-drag-release. In select mode, dragging an
-- existing element moves it around the grid.
handleMouse :: IORef CircuitState -> IORef (Maybe Area) -> IORef (Maybe SpiceState) -> MouseEvent -> IO ()
handleMouse stateRef canvasRef spiceRef me = do
  canvasM <- readIORef canvasRef
  forM_ canvasM $ \canvas -> do
    s <- readIORef stateRef
    case selectedKind s of
      Just _ -> handleAdd canvas
      Nothing -> handleSelect canvas
  where
    pos = (mouseX me, mouseY me)

    handleAdd canvas = do
      if mouseDown me == 1
        then do
          s <- readIORef stateRef
          case selectedKind s of
            Just k -> do
              let p = snapPoint pos
              modifyIORef' stateRef $ \s' -> s' {pending = Just (Element k (defaultValue k) p p)}
              queueRedraw canvas
            Nothing -> pure ()
        else
          if mouseUp me == 1
            then do
              s <- readIORef stateRef
              case pending s of
                Just el@(Element _ _ p1 p2)
                  | p1 /= p2 -> do
                      modifyIORef' stateRef $ \s' ->
                        s' {placed = el : placed s', pending = Nothing}
                      runSpice stateRef spiceRef
                _ -> modifyIORef' stateRef $ \s' -> s' {pending = Nothing}
              queueRedraw canvas
            else
              when (odd (mouseHeld me)) $ do
                s <- readIORef stateRef
                case pending s of
                  Just (Element k _ p1 _) -> do
                    let p2' = snapPoint pos
                    modifyIORef' stateRef $ \s' -> s' {pending = Just (Element k (defaultValue k) p1 p2')}
                    queueRedraw canvas
                  Nothing -> pure ()

    handleSelect canvas = do
      if mouseDown me == 1
        then do
          s <- readIORef stateRef
          case findElementAt pos (placed s) of
            Just (i, elm)
              | mouseCount me == 2 -> editElement stateRef spiceRef canvasRef i elm
              | otherwise ->
                  modifyIORef' stateRef $ \s' ->
                    s' {dragInfo = Just (DragInfo i (snapPoint pos) elm)}
            Nothing -> pure ()
        else
          if mouseUp me == 1
            then do
              s <- readIORef stateRef
              when (isJust (dragInfo s)) $ runSpice stateRef spiceRef
              modifyIORef' stateRef $ \s' -> s' {dragInfo = Nothing}
              queueRedraw canvas
            else
              when (odd (mouseHeld me)) $ do
                s <- readIORef stateRef
                case dragInfo s of
                  Just DragInfo {dragIndex = i, dragAnchor = anchor, dragOriginal = orig} -> do
                    let delta = subPoint (snapPoint pos) anchor
                        elm' = moveElement orig delta
                    modifyIORef' stateRef $ \s' -> s' {placed = replaceAt i elm' (placed s')}
                    queueRedraw canvas
                  Nothing -> pure ()

-- Key handling ----------------------------------------------------------

-- Escape returns to select/drag mode, cancelling any in-progress placement.
handleKey :: IORef CircuitState -> Control -> IORef (Maybe Area) -> KeyEvent -> IO Bool
handleKey stateRef titleLabel canvasRef ke
  | keyExt ke == escapeKey && not (keyUp ke) = do
      modifyIORef' stateRef $ \s ->
        s {selectedKind = Nothing, pending = Nothing, dragInfo = Nothing}
      setLabelText titleLabel "Select/Drag mode"
      redraw canvasRef
      putStrLn "escape: back to select/drag mode"
      pure True
  | otherwise = pure False

-- Simulation ------------------------------------------------------------

-- | Regenerates the netlist from the current circuit and restarts ngspice.
runSpice :: IORef CircuitState -> IORef (Maybe SpiceState) -> IO ()
runSpice stateRef spiceRef = do
  s <- readIORef stateRef
  let els = placed s
  if null els
    then writeIORef spiceRef Nothing
    else do
      let (netlist, elems, nodeCount) = generateNetlist els
      loadNetlist netlist
      -- `tran` runs the analysis synchronously (and quickly, since there is
      -- no per-point delay now); afterwards we read the full waveform and
      -- animate it by playback.
      spiceCommand "tran 20u 50m uic"
      nodeVecs <- mapM (readVector . nodeVoltageName) [1 .. nodeCount - 1]
      currVecs <- mapM (readVector . seCurrentName) elems
      let frameCount = maximum (0 : map length (currVecs ++ nodeVecs))
          nodeVoltsAt = [0 : [v !! j | v <- nodeVecs] | j <- [0 .. frameCount - 1]]
          currentsAt = [[c !! j | c <- currVecs] | j <- [0 .. frameCount - 1]]
      writeIORef spiceRef (Just (SpiceState elems frameCount 0 nodeVoltsAt currentsAt))

-- | Opens a small dialog to edit a component's value.
editElement :: IORef CircuitState -> IORef (Maybe SpiceState) -> IORef (Maybe Area) -> Int -> Element -> IO ()
editElement _ _ _ _ elm | elemKind elm == Wire = pure () -- wires are fixed
editElement stateRef spiceRef canvasRef idx elm = do
  let kind = elemKind elm
  dlg <- window ("Edit " ++ componentName kind) 320 140
  setMargined dlg True

  f <- form
  valueEntry <- entry
  setEntryText valueEntry (formatValue (elemValue elm))
  formAppend f (valueUnit kind) valueEntry False

  btns <- hbox
  setPadded btns True
  okBtn <- button "OK" $ do
    txt <- entryText valueEntry
    case readMaybe txt :: Maybe Double of
      Just v
        | v > 0 -> do
            modifyIORef' stateRef $ \s ->
              s {placed = replaceAt idx (elm {elemValue = v}) (placed s)}
            runSpice stateRef spiceRef
            redraw canvasRef
            destroyWindow dlg
      _ -> pure () -- invalid input; keep the dialog open
  cancelBtn <- button "Cancel" (destroyWindow dlg)
  append btns okBtn
  append btns cancelBtn

  root <- vbox
  setPadded root True
  append root f
  append root btns
  setChild dlg root
  showWidget dlg

formatValue :: Double -> String
formatValue v = printf "%.6g" v

-- | Timer tick: advances the playback frame and repaints.
timerTick :: IORef (Maybe SpiceState) -> IORef Bool -> IORef (Maybe Area) -> IO Bool
timerTick spiceRef stoppedRef canvasRef = do
  stopped <- readIORef stoppedRef
  if stopped
    then pure True
    else do
      spiceM <- readIORef spiceRef
      forM_ spiceM $ \sp -> do
        let next = (spFrame sp + 8) `mod` max 1 (spFrameCount sp)
        writeIORef spiceRef (Just sp {spFrame = next})
        redraw canvasRef
      pure True

-- Helpers ---------------------------------------------------------------

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

snapPoint :: Point -> Point
snapPoint (x, y) = (snapGrid x, snapGrid y)

addPoint :: Point -> Point -> Point
addPoint (ax, ay) (bx, by) = (ax + bx, ay + by)

subPoint :: Point -> Point -> Point
subPoint (ax, ay) (bx, by) = (ax - bx, ay - by)

moveElement :: Element -> Point -> Element
moveElement (Element k v p1 p2) d = Element k v (addPoint p1 d) (addPoint p2 d)

replaceAt :: Int -> a -> [a] -> [a]
replaceAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

-- | Finds the topmost (most recently added) element whose body is near @p@.
findElementAt :: Point -> [Element] -> Maybe (Int, Element)
findElementAt p = go 0
  where
    go _ [] = Nothing
    go i (e : es)
      | distanceToSegment p (elemP1 e) (elemP2 e) <= 12 = Just (i, e)
      | otherwise = go (i + 1) es

redraw :: IORef (Maybe Area) -> IO ()
redraw canvasRef = readIORef canvasRef >>= mapM_ queueRedraw

formatVoltage :: Double -> String
formatVoltage v = printf "%.2f V" v

formatCurrent :: Double -> String
formatCurrent i
  | abs i < 1 = printf "%.2f mA" (i * 1000)
  | otherwise = printf "%.2f A" i
