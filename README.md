# del

A circuit simulator being rebuilt in Haskell, starting from a port of Paul
Falstad's Java applet (this repo is a fork of
<https://github.com/kojix2/circuit-simulator>). The GUI is implemented on top of
[libui-ng](https://github.com/libui-ng/libui) through a small C binding.

## Status

This first cut reproduces the **basic interface** of the Java simulator and
runs a live simulation of linear circuits:

- window titled `Circuit Simulator v1.6.1a`
- menu bar: `File`, `Edit`, `Scope`, `Options`, `Circuits`, `Components`
  (the component palette is a flat menu — libui-ng has no nested submenus)
- toolbar: `Reset` button, `Stopped` checkbox, `Simulation Speed` and
  `Current Speed` sliders, and the `Current Circuit:` label
- a custom drawing canvas that renders the dark background and grid
- components **wire**, **battery**, **resistor**, **capacitor**, **inductor**
  and **diode**, placed by choosing one in the `Components` menu and
  press-drag-releasing on the canvas
- a **select/drag mode**: press `Escape` after placing to leave add mode and
  drag existing components around the grid (press-drag on a component)
- an example **series RLC circuit** (`Circuits > RLC Circuit`)
- a **modified nodal analysis** engine (`Circuit.Analysis`) that stamps the
  elements, solves the system with Gaussian elimination and advances the
  transient with a `uiTimer`; components are colored red/green by node
  voltage, and **the diode is solved with Newton-Raphson**
- a **Show Values** option (`Options > Show Values`) that draws each
  component's voltage and current on the canvas
- **editable component values**: double-click a component to open a small
  dialog and change its resistance/capacitance/inductance/voltage (the
  simulation re-runs immediately)
- **ngspice integration** (`Circuit.Spice`): the circuit is translated to a
  SPICE netlist, ngspice runs the transient (KLU sparse solver, adaptive
  timestep, full device models), and the waveform is played back and looped;
  the hand-rolled solver is kept for tests but the GUI uses ngspice

The matrix-simplification pass is not implemented yet.

## Layout

```
cbits/
  libui_bridge.c/.h   C bridge: a small, Haskell-friendly API over libui-ng
  include/ui.h        vendored libui-ng public header
src/
  UI.hs               Haskell FFI bindings + high-level libui API
  Circuit.hs          the circuit simulator interface (menus, toolbar, canvas,
                      mouse placement, select/drag mode, simulation playback)
  Circuit/Elements.hs component model + drawing of the components
  Circuit/Analysis.hs hand-rolled MNA engine (kept for unit tests)
  Circuit/Spice.hs    ngspice binding + netlist generation
app/
  Main.hs             entry point (runs Circuit)
vendor/lib/
  libui.dylib         vendored libui-ng shared library (macOS universal)
Setup.hs              makes the relative include/lib dirs absolute at configure time
```

## Build and run

```sh
cabal run del
```

Close the window or choose `File > Exit` to quit.

## The C bridge

`cbits/libui_bridge.c` maps libui-ng's object handles to opaque `void *` and
its `(sender, data)` callbacks to plain `(data)` callbacks. It exposes:

| Area       | Functions                                                              |
| ---------- | ---------------------------------------------------------------------- |
| Lifecycle  | `bridge_uiInit`, `bridge_uiMain`, `bridge_uiQuit`, `bridge_uiUninit`   |
| Widgets    | window, vbox/hbox, label, button, checkbox, slider                     |
| Menus      | `bridge_uiNewMenu`, append item/check/separator/quit, click + checked  |
| Drawing    | `bridge_uiNewArea`, `bridge_uiAreaQueueRedrawAll`, fillRect, drawLine  |
| Composition| `bridge_uiWindowSetChild`, `bridge_uiBoxAppend`, `bridge_uiControlShow`|

> **macOS gotcha:** menus must be created *before* the first window. libui
> finalizes the menu bar inside `uiNewWindow`, and creating menus afterwards
> aborts with `uiNewMenu(): You can't create a new menu after menus have been
> finalized.` `Circuit.run` therefore builds all menus first.

## The Haskell API

```haskell
import UI

main :: IO ()
main = do
  initUI
  w   <- window "Hello" 320 200
  box <- vbox
  l   <- label "Hello, world!"
  btn <- button "Click me" (putStrLn "clicked!")
  append box l
  append box btn
  setChild w box
  onClose w quit
  showWidget w
  uiMain
  uninitUI
```

Types: `Window`, `Box`, `Control`, `Area`, `DrawContext`, `Menu`, `MenuItem`.
Callbacks are plain `IO ()` actions kept alive with `StablePtr`s and C `FunPtr`s.
Custom drawing is done with `area :: (DrawContext -> Double -> Double -> IO ()) -> IO Area`
plus `fillRect` / `drawLine`.

## Rebuilding the vendored libui-ng

The header and library under `cbits/include/` and `vendor/lib/` were built from
`https://github.com/kojix2/libui-ng` with:

```sh
meson setup build --buildtype=release --default-library=shared -Dtests=false -Dexamples=false
ninja -C build
# then copy:
#   ui.h                     -> cbits/include/ui.h
#   build/meson-out/libui.*  -> vendor/lib/libui.*
```

On non-macOS platforms, replace `vendor/lib/libui.dylib` with the platform's
build artifact (`libui.so` / `libui.dll` / `libui.a`) and adjust
`extra-libraries` accordingly.

## License

`del` is MIT (see `LICENSE`). libui-ng is MIT. The circuit-simulator source this
is based on is © Paul Falstad / Rodrigo Hausen — see its `README.md` for terms.
