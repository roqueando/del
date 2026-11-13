{-# LANGUAGE ForeignFunctionInterface #-}

-- | A small, high-level Haskell binding to libui-ng.
--
-- The binding talks to libui-ng through a thin C bridge
-- (@cbits/libui_bridge.c@) that exposes only opaque @void *@ handles,
-- @const char *@ strings and plain C function pointers, which map cleanly
-- onto Haskell's FFI.
module UI
  ( -- * Handles
    Window,
    Box,
    Control,
    Form,
    Area,
    DrawContext,
    Menu,
    MenuItem,
    IsControl (toControl),
    Showable (showWidget),

    -- * Lifecycle
    initUI,
    uiMain,
    quit,
    uninitUI,
    timer,

    -- * Widgets
    window,
    vbox,
    hbox,
    setPadded,
    setMargined,
    label,
    setLabelText,
    button,
    entry,
    entryText,
    setEntryText,
    form,
    formAppend,
    destroyControl,
    destroyWindow,
    checkbox,
    checkboxChecked,
    setCheckboxChecked,
    slider,
    sliderValue,
    setSliderValue,

    -- * Menus
    menu,
    menuItem,
    checkMenuItem,
    quitItem,
    menuSeparator,
    onMenuItemClicked,
    setMenuItemChecked,
    menuItemChecked,
    disableMenuItem,

    -- * Custom drawing area
    area,
    queueRedraw,
    setAreaSize,
    fillRect,
    drawLine,
    fillTriangle,
    fillCircle,
    drawText,
    RGBA,
    MouseEvent (..),
    KeyEvent (..),

    -- * Composition
    setChild,
    append,
    appendStretchy,
    onClose,
  )
where

import Foreign
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CDouble (..), CInt (..))

-- | Opaque handle to a top-level window.
newtype Window = Window (Ptr ())

-- | Opaque handle to a box container.
newtype Box = Box (Ptr ())

-- | Opaque handle to a generic control (label, button, checkbox, slider, ...).
newtype Control = Control (Ptr ())

-- | Opaque handle to a form layout.
newtype Form = Form (Ptr ())

-- | Opaque handle to a custom drawing area.
newtype Area = Area (Ptr ())

-- | Drawing context passed to an area's redraw callback.
newtype DrawContext = DrawContext (Ptr ())

-- | Opaque handle to a menu.
newtype Menu = Menu (Ptr ())

-- | Opaque handle to a menu item.
newtype MenuItem = MenuItem (Ptr ())

-- | An RGBA color with components in the range @0..1@.
type RGBA = (Double, Double, Double, Double)

-- | A mouse event on a custom drawing area.
data MouseEvent = MouseEvent
  { mouseX :: Double, -- ^ x coordinate in the area's coordinate space
    mouseY :: Double, -- ^ y coordinate in the area's coordinate space
    mouseDown :: Int, -- ^ button number on press (1 = left), 0 otherwise
    mouseUp :: Int, -- ^ button number on release (1 = left), 0 otherwise
    mouseHeld :: Int, -- ^ bitmask of held buttons (bit 0 = left)
    mouseCount :: Int -- ^ click count (1 = single, 2 = double, ...)
  }

-- | A key event on a custom drawing area.
data KeyEvent = KeyEvent
  { keyChar :: Char, -- ^ printable character, @'\0'@ if none
    keyExt :: Int, -- ^ extended key code (1 = Escape, ...)
    keyUp :: Bool -- ^ 'True' for key release, 'False' for press
  }

-- | Things that can be shown with 'showWidget'.
class Showable a where
  showWidget :: a -> IO ()

instance Showable Control where
  showWidget (Control c) = c_uiControlShow c

instance Showable Window where
  showWidget (Window w) = c_uiControlShow w

instance Showable Area where
  showWidget (Area a) = c_uiControlShow a

-- | Anything that can be used where libui expects a control.
class IsControl a where
  toControl :: a -> Control

instance IsControl Control where
  toControl = id

instance IsControl Box where
  toControl (Box p) = Control p

instance IsControl Area where
  toControl (Area p) = Control p

instance IsControl Form where
  toControl (Form p) = Control p

-- | Unwrap a 'Control' handle to its underlying pointer.
controlPtr :: Control -> Ptr ()
controlPtr (Control p) = p

-- Raw FFI imports -------------------------------------------------------

foreign import ccall "bridge_uiInit" c_uiInit :: IO CString
foreign import ccall unsafe "bridge_uiFreeInitError" c_uiFreeInitError :: CString -> IO ()
foreign import ccall safe "bridge_uiMain" c_uiMain :: IO ()
foreign import ccall unsafe "bridge_uiQuit" c_uiQuit :: IO ()
foreign import ccall unsafe "bridge_uiUninit" c_uiUninit :: IO ()
foreign import ccall unsafe "bridge_uiTimer" c_uiTimer
  :: CInt -> FunPtr (Ptr () -> IO CInt) -> Ptr () -> IO ()

foreign import ccall unsafe "bridge_uiNewWindow" c_uiNewWindow
  :: CString -> CInt -> CInt -> IO (Ptr ())

foreign import ccall unsafe "bridge_uiNewVerticalBox" c_uiNewVerticalBox :: IO (Ptr ())
foreign import ccall unsafe "bridge_uiNewHorizontalBox" c_uiNewHorizontalBox :: IO (Ptr ())
foreign import ccall unsafe "bridge_uiBoxSetPadded" c_uiBoxSetPadded :: Ptr () -> CInt -> IO ()

foreign import ccall unsafe "bridge_uiNewLabel" c_uiNewLabel :: CString -> IO (Ptr ())
foreign import ccall unsafe "bridge_uiLabelSetText" c_uiLabelSetText :: Ptr () -> CString -> IO ()

foreign import ccall unsafe "bridge_uiNewEntry" c_uiNewEntry :: IO (Ptr ())
foreign import ccall unsafe "bridge_uiEntryText" c_uiEntryText :: Ptr () -> IO CString
foreign import ccall unsafe "bridge_uiEntrySetText" c_uiEntrySetText :: Ptr () -> CString -> IO ()
foreign import ccall unsafe "bridge_uiFreeText" c_uiFreeText :: CString -> IO ()

foreign import ccall unsafe "bridge_uiNewForm" c_uiNewForm :: IO (Ptr ())
foreign import ccall unsafe "bridge_uiFormAppend" c_uiFormAppend :: Ptr () -> CString -> Ptr () -> CInt -> IO ()

foreign import ccall unsafe "bridge_uiControlDestroy" c_uiControlDestroy :: Ptr () -> IO ()

foreign import ccall unsafe "bridge_uiNewButton" c_uiNewButton
  :: CString -> FunPtr (Ptr () -> IO ()) -> Ptr () -> IO (Ptr ())

foreign import ccall unsafe "bridge_uiNewCheckbox" c_uiNewCheckbox :: CString -> IO (Ptr ())
foreign import ccall unsafe "bridge_uiCheckboxSetChecked" c_uiCheckboxSetChecked :: Ptr () -> CInt -> IO ()
foreign import ccall unsafe "bridge_uiCheckboxChecked" c_uiCheckboxChecked :: Ptr () -> IO CInt
foreign import ccall unsafe "bridge_uiCheckboxOnToggled" c_uiCheckboxOnToggled
  :: Ptr () -> FunPtr (Ptr () -> IO ()) -> Ptr () -> IO ()

foreign import ccall unsafe "bridge_uiNewSlider" c_uiNewSlider :: CInt -> CInt -> IO (Ptr ())
foreign import ccall unsafe "bridge_uiSliderSetValue" c_uiSliderSetValue :: Ptr () -> CInt -> IO ()
foreign import ccall unsafe "bridge_uiSliderValue" c_uiSliderValue :: Ptr () -> IO CInt
foreign import ccall unsafe "bridge_uiSliderOnChanged" c_uiSliderOnChanged
  :: Ptr () -> FunPtr (Ptr () -> IO ()) -> Ptr () -> IO ()

foreign import ccall unsafe "bridge_uiWindowOnClosing" c_uiWindowOnClosing
  :: Ptr () -> FunPtr (Ptr () -> IO ()) -> Ptr () -> IO ()

foreign import ccall unsafe "bridge_uiWindowSetMargined" c_uiWindowSetMargined :: Ptr () -> CInt -> IO ()

foreign import ccall unsafe "bridge_uiWindowSetChild" c_uiWindowSetChild
  :: Ptr () -> Ptr () -> IO ()

foreign import ccall unsafe "bridge_uiBoxAppend" c_uiBoxAppend
  :: Ptr () -> Ptr () -> CInt -> IO ()

foreign import ccall unsafe "bridge_uiControlShow" c_uiControlShow :: Ptr () -> IO ()

foreign import ccall unsafe "bridge_uiNewMenu" c_uiNewMenu :: CString -> IO (Ptr ())
foreign import ccall unsafe "bridge_uiMenuAppendItem" c_uiMenuAppendItem :: Ptr () -> CString -> IO (Ptr ())
foreign import ccall unsafe "bridge_uiMenuAppendCheckItem" c_uiMenuAppendCheckItem :: Ptr () -> CString -> IO (Ptr ())
foreign import ccall unsafe "bridge_uiMenuAppendQuitItem" c_uiMenuAppendQuitItem :: Ptr () -> IO (Ptr ())
foreign import ccall unsafe "bridge_uiMenuAppendSeparator" c_uiMenuAppendSeparator :: Ptr () -> IO ()

foreign import ccall unsafe "bridge_uiMenuItemOnClicked" c_uiMenuItemOnClicked
  :: Ptr () -> FunPtr (Ptr () -> IO ()) -> Ptr () -> IO ()

foreign import ccall unsafe "bridge_uiMenuItemSetChecked" c_uiMenuItemSetChecked :: Ptr () -> CInt -> IO ()
foreign import ccall unsafe "bridge_uiMenuItemChecked" c_uiMenuItemChecked :: Ptr () -> IO CInt
foreign import ccall unsafe "bridge_uiMenuItemDisable" c_uiMenuItemDisable :: Ptr () -> IO ()

foreign import ccall unsafe "bridge_uiNewArea" c_uiNewArea
  :: FunPtr (Ptr () -> CDouble -> CDouble -> Ptr () -> IO ())
  -> FunPtr (CDouble -> CDouble -> CInt -> CInt -> CInt -> CInt -> Ptr () -> IO ())
  -> FunPtr (CInt -> CInt -> CInt -> Ptr () -> IO CInt)
  -> Ptr ()
  -> IO (Ptr ())

foreign import ccall unsafe "bridge_uiAreaQueueRedrawAll" c_uiAreaQueueRedrawAll :: Ptr () -> IO ()
foreign import ccall unsafe "bridge_uiAreaSetSize" c_uiAreaSetSize :: Ptr () -> CInt -> CInt -> IO ()

foreign import ccall unsafe "bridge_drawFillRect" c_drawFillRect
  :: Ptr () -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall unsafe "bridge_drawLine" c_drawLine
  :: Ptr () -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall unsafe "bridge_fillTriangle" c_fillTriangle
  :: Ptr () -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall unsafe "bridge_fillCircle" c_fillCircle
  :: Ptr () -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

foreign import ccall unsafe "bridge_drawText" c_drawText
  :: Ptr () -> CDouble -> CDouble -> CString -> CDouble -> CDouble -> CDouble -> CDouble -> CDouble -> IO ()

-- Turn a Haskell function into a C function pointer stored by libui and
-- invoked from its event loop. `uiMain` is imported @safe@ so the runtime can
-- call back into Haskell.
foreign import ccall "wrapper"
  mkCallback :: (Ptr () -> IO ()) -> IO (FunPtr (Ptr () -> IO ()))

foreign import ccall "wrapper"
  mkAreaDraw :: (Ptr () -> CDouble -> CDouble -> Ptr () -> IO ())
  -> IO (FunPtr (Ptr () -> CDouble -> CDouble -> Ptr () -> IO ()))

foreign import ccall "wrapper"
  mkMouse :: (CDouble -> CDouble -> CInt -> CInt -> CInt -> CInt -> Ptr () -> IO ())
  -> IO (FunPtr (CDouble -> CDouble -> CInt -> CInt -> CInt -> CInt -> Ptr () -> IO ()))

foreign import ccall "wrapper"
  mkKey :: (CInt -> CInt -> CInt -> Ptr () -> IO CInt)
  -> IO (FunPtr (CInt -> CInt -> CInt -> Ptr () -> IO CInt))

foreign import ccall "wrapper"
  mkTimer :: (Ptr () -> IO CInt) -> IO (FunPtr (Ptr () -> IO CInt))

-- High-level API ---------------------------------------------------------

-- | Initializes libui. Throws an 'IOError' if initialization fails.
initUI :: IO ()
initUI = do
  err <- c_uiInit
  if err == nullPtr
    then pure ()
    else do
      msg <- peekCString err
      c_uiFreeInitError err
      ioError (userError ("libui initialization failed: " ++ msg))

-- | Runs the main event loop until 'quit' is called.
uiMain :: IO ()
uiMain = c_uiMain

-- | Stops the main event loop. Safe to call from callbacks.
quit :: IO ()
quit = c_uiQuit

-- | Tears libui down. Call after 'uiMain' returns.
uninitUI :: IO ()
uninitUI = c_uiUninit

-- | Schedules a repeating timer. The action returns 'True' to keep the timer
-- running, or 'False' to stop it.
timer :: Int -> IO Bool -> IO ()
timer ms action = do
  stable <- newStablePtr action
  fn <- mkTimer timerTrampoline
  c_uiTimer (fromIntegral ms) fn (castStablePtrToPtr stable)

-- | Creates a window with the given title and content size in pixels.
window :: String -> Int -> Int -> IO Window
window title w h =
  withCString title $ \t ->
    Window <$> c_uiNewWindow t (fromIntegral w) (fromIntegral h)

-- | Creates an empty vertical box container.
vbox :: IO Box
vbox = Box <$> c_uiNewVerticalBox

-- | Creates an empty horizontal box container.
hbox :: IO Box
hbox = Box <$> c_uiNewHorizontalBox

-- | Sets whether a box lays out its children with padding.
setPadded :: Box -> Bool -> IO ()
setPadded (Box b) padded = c_uiBoxSetPadded b (if padded then 1 else 0)

-- | Sets whether a window's content has a margin.
setMargined :: Window -> Bool -> IO ()
setMargined (Window w) margined = c_uiWindowSetMargined w (if margined then 1 else 0)

-- | Creates a label displaying the given text.
label :: String -> IO Control
label text = withCString text $ \t -> Control <$> c_uiNewLabel t

-- | Changes the text of a label.
setLabelText :: Control -> String -> IO ()
setLabelText (Control l) text = withCString text $ c_uiLabelSetText l

-- | Creates a button. The given action runs every time the button is clicked.
button :: String -> IO () -> IO Control
button text onClick = do
  stable <- newStablePtr onClick
  fn <- mkCallback clickedHandler
  ptr <- withCString text $ \t -> c_uiNewButton t fn (castStablePtrToPtr stable)
  pure (Control ptr)

-- | Creates a single-line text entry.
entry :: IO Control
entry = Control <$> c_uiNewEntry

-- | Reads the current text of an entry.
entryText :: Control -> IO String
entryText (Control e) = do
  cs <- c_uiEntryText e
  s <- peekCString cs
  c_uiFreeText cs
  pure s

-- | Sets the text of an entry.
setEntryText :: Control -> String -> IO ()
setEntryText (Control e) text = withCString text (c_uiEntrySetText e)

-- | Creates a form layout (label + control rows).
form :: IO Form
form = Form <$> c_uiNewForm

-- | Appends a labelled control to a form.
formAppend :: Form -> String -> Control -> Bool -> IO ()
formAppend (Form f) caption (Control c) stretchy =
  withCString caption $ \l -> c_uiFormAppend f l c (if stretchy then 1 else 0)

-- | Destroys a control.
destroyControl :: Control -> IO ()
destroyControl (Control c) = c_uiControlDestroy c

-- | Destroys a window.
destroyWindow :: Window -> IO ()
destroyWindow (Window w) = c_uiControlDestroy w

-- | Creates a checkbox. The action runs whenever the user toggles it.
checkbox :: String -> IO () -> IO Control
checkbox text onToggle = do
  stable <- newStablePtr onToggle
  fn <- mkCallback clickedHandler
  ptr <- withCString text $ \t -> c_uiNewCheckbox t
  c_uiCheckboxOnToggled ptr fn (castStablePtrToPtr stable)
  pure (Control ptr)

-- | Reads the checked state of a checkbox.
checkboxChecked :: Control -> IO Bool
checkboxChecked (Control c) = (/= 0) <$> c_uiCheckboxChecked c

-- | Sets the checked state of a checkbox (does not fire the callback).
setCheckboxChecked :: Control -> Bool -> IO ()
setCheckboxChecked (Control c) checked = c_uiCheckboxSetChecked c (if checked then 1 else 0)

-- | Creates a slider with the given range. The action runs as it changes.
slider :: Int -> Int -> IO () -> IO Control
slider lo hi onChanged = do
  stable <- newStablePtr onChanged
  fn <- mkCallback clickedHandler
  ptr <- c_uiNewSlider (fromIntegral lo) (fromIntegral hi)
  c_uiSliderOnChanged ptr fn (castStablePtrToPtr stable)
  pure (Control ptr)

-- | Reads the current value of a slider.
sliderValue :: Control -> IO Int
sliderValue (Control s) = fromIntegral <$> c_uiSliderValue s

-- | Sets the value of a slider (does not fire the callback).
setSliderValue :: Control -> Int -> IO ()
setSliderValue (Control s) v = c_uiSliderSetValue s (fromIntegral v)

-- | Creates a menu with the given name. Menus are placed in the application
-- menu bar automatically.
menu :: String -> IO Menu
menu name = withCString name $ \n -> Menu <$> c_uiNewMenu n

-- | Appends a plain menu item.
menuItem :: Menu -> String -> IO MenuItem
menuItem (Menu m) name = withCString name $ \n -> MenuItem <$> c_uiMenuAppendItem m n

-- | Appends a checkable menu item.
checkMenuItem :: Menu -> String -> IO MenuItem
checkMenuItem (Menu m) name = withCString name $ \n -> MenuItem <$> c_uiMenuAppendCheckItem m n

-- | Appends the platform-standard "Quit" menu item.
quitItem :: Menu -> IO MenuItem
quitItem (Menu m) = MenuItem <$> c_uiMenuAppendQuitItem m

-- | Appends a separator to a menu.
menuSeparator :: Menu -> IO ()
menuSeparator (Menu m) = c_uiMenuAppendSeparator m

-- | Registers the action to run when a menu item is clicked.
onMenuItemClicked :: MenuItem -> IO () -> IO ()
onMenuItemClicked (MenuItem item) action = do
  stable <- newStablePtr action
  fn <- mkCallback clickedHandler
  c_uiMenuItemOnClicked item fn (castStablePtrToPtr stable)

-- | Sets the checked state of a checkable menu item.
setMenuItemChecked :: MenuItem -> Bool -> IO ()
setMenuItemChecked (MenuItem item) checked =
  c_uiMenuItemSetChecked item (if checked then 1 else 0)

-- | Reads the checked state of a checkable menu item.
menuItemChecked :: MenuItem -> IO Bool
menuItemChecked (MenuItem item) = (/= 0) <$> c_uiMenuItemChecked item

-- | Disables (grays out) a menu item.
disableMenuItem :: MenuItem -> IO ()
disableMenuItem (MenuItem item) = c_uiMenuItemDisable item

-- | Creates a custom drawing area. The draw callback receives the context
-- and the current size (in points); the mouse callback receives mouse events;
-- the key callback receives key events and returns 'True' if handled.
area ::
  (DrawContext -> Double -> Double -> IO ()) ->
  (MouseEvent -> IO ()) ->
  (KeyEvent -> IO Bool) ->
  IO Area
area drawFn mouseFn keyFn = do
  stable <- newStablePtr (AreaCallbacks drawFn mouseFn keyFn)
  dfn <- mkAreaDraw areaDrawTrampoline
  mfn <- mkMouse mouseTrampoline
  kfn <- mkKey keyTrampoline
  ptr <- c_uiNewArea dfn mfn kfn (castStablePtrToPtr stable)
  pure (Area ptr)

-- | Requests a full repaint of an area.
queueRedraw :: Area -> IO ()
queueRedraw (Area a) = c_uiAreaQueueRedrawAll a

-- | Sets the requested size of an area.
setAreaSize :: Area -> Int -> Int -> IO ()
setAreaSize (Area a) w h = c_uiAreaSetSize a (fromIntegral w) (fromIntegral h)

-- | Fills an axis-aligned rectangle with an 'RGBA' color.
fillRect :: DrawContext -> Double -> Double -> Double -> Double -> RGBA -> IO ()
fillRect (DrawContext c) x y w h (r, g, b, a) =
  c_drawFillRect c (d x) (d y) (d w) (d h) (d r) (d g) (d b) (d a)

-- | Strokes a line between two points with an 'RGBA' color and a thickness.
drawLine :: DrawContext -> (Double, Double) -> (Double, Double) -> RGBA -> Double -> IO ()
drawLine (DrawContext c) (x1, y1) (x2, y2) (r, g, b, a) thickness =
  c_drawLine c (d x1) (d y1) (d x2) (d y2) (d r) (d g) (d b) (d a) (d thickness)

-- | Fills a triangle with an 'RGBA' color.
fillTriangle :: DrawContext -> (Double, Double) -> (Double, Double) -> (Double, Double) -> RGBA -> IO ()
fillTriangle (DrawContext c) (x1, y1) (x2, y2) (x3, y3) (r, g, b, a) =
  c_fillTriangle c (d x1) (d y1) (d x2) (d y2) (d x3) (d y3) (d r) (d g) (d b) (d a)

-- | Fills a circle with an 'RGBA' color.
fillCircle :: DrawContext -> (Double, Double) -> Double -> RGBA -> IO ()
fillCircle (DrawContext c) (cx, cy) radius (r, g, b, a) =
  c_fillCircle c (d cx) (d cy) (d radius) (d r) (d g) (d b) (d a)

-- | Draws a text label with its top-left corner at the given point.
drawText :: DrawContext -> (Double, Double) -> Double -> RGBA -> String -> IO ()
drawText (DrawContext c) (x, y) size (r, g, b, a) text =
  withCString text $ \t ->
    c_drawText c (d x) (d y) t (d size) (d r) (d g) (d b) (d a)

-- | Runs the given action when the window is closed, then destroys it.
onClose :: Window -> IO () -> IO ()
onClose (Window w) action = do
  stable <- newStablePtr action
  fn <- mkCallback clickedHandler
  c_uiWindowOnClosing w fn (castStablePtrToPtr stable)

-- | Sets the single child of a window.
setChild :: IsControl c => Window -> c -> IO ()
setChild (Window w) child = c_uiWindowSetChild w (controlPtr (toControl child))

-- | Appends a control to a box.
append :: IsControl c => Box -> c -> IO ()
append (Box b) child = c_uiBoxAppend b (controlPtr (toControl child)) 0

-- | Appends a control to a box, optionally stretching it to fill free space.
appendStretchy :: IsControl c => Box -> c -> Bool -> IO ()
appendStretchy (Box b) child stretchy =
  c_uiBoxAppend b (controlPtr (toControl child)) (if stretchy then 1 else 0)

-- Callback trampolines ---------------------------------------------------

-- The @Ptr ()@ passed in by the C bridge is a 'StablePtr' to the Haskell
-- action created in 'button' / 'checkbox' / 'slider' / 'menuItem' / 'onClose'.
clickedHandler :: Ptr () -> IO ()
clickedHandler ptr = do
  action <- deRefStablePtr (castPtrToStablePtr ptr)
  action

areaDrawTrampoline :: Ptr () -> CDouble -> CDouble -> Ptr () -> IO ()
areaDrawTrampoline ctx w h ptr = do
  AreaCallbacks{areaDraw = drawFn} <- deRefStablePtr (castPtrToStablePtr ptr)
  let wd = realToFrac w :: Double
      hd = realToFrac h :: Double
  drawFn (DrawContext ctx) wd hd

mouseTrampoline :: CDouble -> CDouble -> CInt -> CInt -> CInt -> CInt -> Ptr () -> IO ()
mouseTrampoline x y down up held count ptr = do
  AreaCallbacks{areaMouse = mouseFn} <- deRefStablePtr (castPtrToStablePtr ptr)
  mouseFn
    MouseEvent
      { mouseX = realToFrac x :: Double,
        mouseY = realToFrac y :: Double,
        mouseDown = fromIntegral down,
        mouseUp = fromIntegral up,
        mouseHeld = fromIntegral held,
        mouseCount = fromIntegral count
      }

keyTrampoline :: CInt -> CInt -> CInt -> Ptr () -> IO CInt
keyTrampoline key ext up ptr = do
  AreaCallbacks{areaKey = keyFn} <- deRefStablePtr (castPtrToStablePtr ptr)
  handled <-
    keyFn
      KeyEvent
        { keyChar = toEnum (fromIntegral key),
          keyExt = fromIntegral ext,
          keyUp = up /= 0
        }
  pure (if handled then 1 else 0)

timerTrampoline :: Ptr () -> IO CInt
timerTrampoline ptr = do
  action <- deRefStablePtr (castPtrToStablePtr ptr)
  keep <- action
  pure (if keep then 1 else 0)

-- | Internal record bundling the callbacks kept alive for an area.
data AreaCallbacks = AreaCallbacks
  { areaDraw :: DrawContext -> Double -> Double -> IO (),
    areaMouse :: MouseEvent -> IO (),
    areaKey :: KeyEvent -> IO Bool
  }

-- Helpers ----------------------------------------------------------------

d :: Double -> CDouble
d = realToFrac
