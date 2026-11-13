/*
 * libui_bridge.h — thin, Haskell-friendly C API on top of libui-ng.
 *
 * The goal of this bridge is to expose a tiny, stable C ABI that is trivial
 * to call from Haskell's FFI: no opaque struct types, only `void *` handles,
 * `const char *` strings and plain C function pointers for callbacks.
 */

#ifndef LIBUI_BRIDGE_H
#define LIBUI_BRIDGE_H

#ifdef __cplusplus
extern "C" {
#endif

/* Lifecycle ----------------------------------------------------------- */

/* Initializes libui. Returns NULL on success, or a UTF-8 error string. */
const char *bridge_uiInit(void);

/* Frees the string returned by bridge_uiInit(). */
void bridge_uiFreeInitError(const char *err);

/* Runs the main event loop until bridge_uiQuit() is called. */
void bridge_uiMain(void);

/* Asks the main event loop to stop. Safe to call from any callback. */
void bridge_uiQuit(void);

/* Tears libui down. */
void bridge_uiUninit(void);

/* Schedules a repeating timer. The callback returns 1 to keep the timer
 * running, or 0 to stop it. */
void bridge_uiTimer(int milliseconds, int (*fire)(void *data), void *data);

/* Widgets ------------------------------------------------------------- */

/* Creates a new window with the given title and content size (px). */
void *bridge_uiNewWindow(const char *title, int width, int height);

/* Creates a vertical or horizontal box container. */
void *bridge_uiNewVerticalBox(void);
void *bridge_uiNewHorizontalBox(void);

/* Sets the padding of a box (0 or 1). */
void bridge_uiBoxSetPadded(void *box, int padded);

/* Creates a text label and changes its text. */
void *bridge_uiNewLabel(const char *text);
void bridge_uiLabelSetText(void *label, const char *text);

/* Creates a button. `onClicked` may be NULL; `data` is passed through to it. */
void *bridge_uiNewButton(const char *text, void (*onClicked)(void *data), void *data);

/* Creates a checkbox and sets/gets its checked state. */
void *bridge_uiNewCheckbox(const char *text);
void bridge_uiCheckboxSetChecked(void *checkbox, int checked);
int bridge_uiCheckboxChecked(void *checkbox);
void bridge_uiCheckboxOnToggled(void *checkbox, void (*onToggled)(void *data), void *data);

/* Creates a slider, sets/gets its value and registers a change callback. */
void *bridge_uiNewSlider(int min, int max);
void bridge_uiSliderSetValue(void *slider, int value);
int bridge_uiSliderValue(void *slider);
void bridge_uiSliderOnChanged(void *slider, void (*onChanged)(void *data), void *data);

/* Registers a callback fired when the window is asked to close.
 * The bridge always allows the window to be destroyed after the callback. */
void bridge_uiWindowOnClosing(void *window, void (*onClosing)(void *data), void *data);

/* Sets whether the window's content has a margin. */
void bridge_uiWindowSetMargined(void *window, int margined);

/* Composition --------------------------------------------------------- */

/* Sets the single child of a window. */
void bridge_uiWindowSetChild(void *window, void *child);

/* Appends a control to a box. `stretchy` is 0 or 1. */
void bridge_uiBoxAppend(void *box, void *control, int stretchy);

/* Shows a control (or a window, which is also a control). */
void bridge_uiControlShow(void *control);

/* Destroys a control (e.g. closes a window). */
void bridge_uiControlDestroy(void *control);

/* Single-line text entry. */
void *bridge_uiNewEntry(void);
char *bridge_uiEntryText(void *entry);
void bridge_uiEntrySetText(void *entry, const char *text);
void bridge_uiFreeText(char *text);

/* Form layout (label + control rows). */
void *bridge_uiNewForm(void);
void bridge_uiFormAppend(void *form, const char *label, void *control, int stretchy);

/* Menus --------------------------------------------------------------- */

/* Creates a menu and appends items / separators. */
void *bridge_uiNewMenu(const char *name);
void *bridge_uiMenuAppendItem(void *menu, const char *name);
void *bridge_uiMenuAppendCheckItem(void *menu, const char *name);
void *bridge_uiMenuAppendQuitItem(void *menu);
void bridge_uiMenuAppendSeparator(void *menu);

/* Registers a click callback and reads/writes the checked state of an item. */
void bridge_uiMenuItemOnClicked(void *item, void (*onClicked)(void *data), void *data);
void bridge_uiMenuItemSetChecked(void *item, int checked);
int bridge_uiMenuItemChecked(void *item);
void bridge_uiMenuItemDisable(void *item);

/* Custom drawing area ------------------------------------------------- */

/* Creates an area. The draw callback receives the context, size and `data`;
 * the mouse callback receives coordinates, event flags and `data`; the key
 * callback receives a key/event code, the up flag and `data`, and returns
 * 1 if the key was handled.
 *   down/up : button number (1=left) for press/release, 0 otherwise
 *   held    : bitmask of buttons currently held (bit 0 = left) */
void *bridge_uiNewArea(
	void (*draw)(void *context, double width, double height, void *data),
	void (*mouse)(double x, double y, int down, int up, int held, int count, void *data),
	int (*key)(int key, int extKey, int up, void *data),
	void *data);

/* Requests a full redraw of the area. */
void bridge_uiAreaQueueRedrawAll(void *area);

/* Sets the requested size of the area. */
void bridge_uiAreaSetSize(void *area, int width, int height);

/* Minimal draw helpers (used from the area redraw callback). */

/* Fills an axis-aligned rectangle with the given RGBA color (0..1). */
void bridge_drawFillRect(void *context, double x, double y, double width, double height,
                         double r, double g, double b, double a);

/* Strokes a line from (x1, y1) to (x2, y2) with the given RGBA color and thickness. */
void bridge_drawLine(void *context, double x1, double y1, double x2, double y2,
                     double r, double g, double b, double a, double thickness);

/* Fills a triangle with the given RGBA color. */
void bridge_fillTriangle(void *context, double x1, double y1, double x2, double y2,
                         double x3, double y3, double r, double g, double b, double a);

/* Fills a circle centered at (cx, cy) with the given radius and RGBA color. */
void bridge_fillCircle(void *context, double cx, double cy, double radius,
                       double r, double g, double b, double a);

/* Draws a text label with its top-left corner at (x, y), using the given
 * font size and RGBA color. */
void bridge_drawText(void *context, double x, double y, const char *text, double size,
                     double r, double g, double b, double a);

#ifdef __cplusplus
}
#endif

#endif /* LIBUI_BRIDGE_H */
