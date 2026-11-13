/*
 * libui_bridge.c — implementation of the Haskell-friendly C API.
 *
 * Every handle is passed around as `void *` so Haskell only ever sees opaque
 * pointers. Callbacks are adapted from libui's `(sender, data)` convention to
 * a plain `(data)` convention.
 */

#include <stdlib.h>
#include <string.h>

#include <ui.h>

#include "libui_bridge.h"

/* Generic callback adapter --------------------------------------------- */

typedef struct bridge_callback {
	void (*fn)(void *data);
	void *data;
} bridge_callback;

static bridge_callback *bridge_callback_new(void (*fn)(void *data), void *data)
{
	bridge_callback *cb = (bridge_callback *) malloc(sizeof(bridge_callback));
	cb->fn = fn;
	cb->data = data;
	/* Kept alive for the lifetime of the owning widget. */
	return cb;
}

/* Lifecycle ----------------------------------------------------------- */

const char *bridge_uiInit(void)
{
	uiInitOptions options;

	memset(&options, 0, sizeof(options));
	return uiInit(&options);
}

void bridge_uiFreeInitError(const char *err)
{
	uiFreeInitError(err);
}

void bridge_uiMain(void)
{
	uiMain();
}

void bridge_uiQuit(void)
{
	uiQuit();
}

void bridge_uiUninit(void)
{
	uiUninit();
}

/* Timer callback that returns an int (1 = keep going). */
typedef struct bridge_int_callback {
	int (*fn)(void *data);
	void *data;
} bridge_int_callback;

static int bridge_timer_fire(void *data)
{
	bridge_int_callback *cb = (bridge_int_callback *) data;
	if (cb != NULL && cb->fn != NULL)
		return cb->fn(cb->data);
	return 0;
}

void bridge_uiTimer(int milliseconds, int (*fire)(void *data), void *data)
{
	bridge_int_callback *cb =
		(bridge_int_callback *) malloc(sizeof(bridge_int_callback));
	cb->fn = fire;
	cb->data = data;
	uiTimer(milliseconds, bridge_timer_fire, cb);
}

/* Widgets ------------------------------------------------------------- */

void *bridge_uiNewWindow(const char *title, int width, int height)
{
	return (void *) uiNewWindow(title, width, height, 1);
}

void *bridge_uiNewVerticalBox(void)
{
	return (void *) uiNewVerticalBox();
}

void *bridge_uiNewHorizontalBox(void)
{
	return (void *) uiNewHorizontalBox();
}

void bridge_uiBoxSetPadded(void *box, int padded)
{
	uiBoxSetPadded((uiBox *) box, padded);
}

void *bridge_uiNewLabel(const char *text)
{
	return (void *) uiNewLabel(text);
}

void bridge_uiLabelSetText(void *label, const char *text)
{
	uiLabelSetText((uiLabel *) label, text);
}

/* libui button callbacks receive (uiButton *sender, void *data); the bridge
 * collapses that to a single (void *data) call for the Haskell side. */
static void bridge_button_clicked(uiButton *sender, void *data)
{
	bridge_callback *cb = (bridge_callback *) data;
	(void) sender;
	if (cb != NULL && cb->fn != NULL)
		cb->fn(cb->data);
}

void *bridge_uiNewButton(const char *text, void (*onClicked)(void *data), void *data)
{
	uiButton *button = uiNewButton(text);

	if (onClicked != NULL) {
		bridge_callback *cb = bridge_callback_new(onClicked, data);
		uiButtonOnClicked(button, bridge_button_clicked, cb);
	}

	return (void *) button;
}

static void bridge_checkbox_toggled(uiCheckbox *sender, void *data)
{
	bridge_callback *cb = (bridge_callback *) data;
	(void) sender;
	if (cb != NULL && cb->fn != NULL)
		cb->fn(cb->data);
}

void *bridge_uiNewCheckbox(const char *text)
{
	return (void *) uiNewCheckbox(text);
}

void bridge_uiCheckboxSetChecked(void *checkbox, int checked)
{
	uiCheckboxSetChecked((uiCheckbox *) checkbox, checked);
}

int bridge_uiCheckboxChecked(void *checkbox)
{
	return uiCheckboxChecked((uiCheckbox *) checkbox);
}

void bridge_uiCheckboxOnToggled(void *checkbox, void (*onToggled)(void *data), void *data)
{
	bridge_callback *cb = bridge_callback_new(onToggled, data);
	uiCheckboxOnToggled((uiCheckbox *) checkbox, bridge_checkbox_toggled, cb);
}

static void bridge_slider_changed(uiSlider *sender, void *data)
{
	bridge_callback *cb = (bridge_callback *) data;
	(void) sender;
	if (cb != NULL && cb->fn != NULL)
		cb->fn(cb->data);
}

void *bridge_uiNewSlider(int min, int max)
{
	return (void *) uiNewSlider(min, max);
}

void bridge_uiSliderSetValue(void *slider, int value)
{
	uiSliderSetValue((uiSlider *) slider, value);
}

int bridge_uiSliderValue(void *slider)
{
	return uiSliderValue((uiSlider *) slider);
}

void bridge_uiSliderOnChanged(void *slider, void (*onChanged)(void *data), void *data)
{
	bridge_callback *cb = bridge_callback_new(onChanged, data);
	uiSliderOnChanged((uiSlider *) slider, bridge_slider_changed, cb);
}

static int bridge_window_closing(uiWindow *sender, void *data)
{
	bridge_callback *cb = (bridge_callback *) data;
	(void) sender;
	if (cb != NULL && cb->fn != NULL)
		cb->fn(cb->data);
	return 1; /* TRUE: allow the window to be destroyed. */
}

void bridge_uiWindowOnClosing(void *window, void (*onClosing)(void *data), void *data)
{
	bridge_callback *cb = bridge_callback_new(onClosing, data);
	uiWindowOnClosing((uiWindow *) window, bridge_window_closing, cb);
}

void bridge_uiWindowSetMargined(void *window, int margined)
{
	uiWindowSetMargined((uiWindow *) window, margined);
}

/* Composition --------------------------------------------------------- */

void bridge_uiWindowSetChild(void *window, void *child)
{
	uiWindowSetChild((uiWindow *) window, (uiControl *) child);
}

void bridge_uiBoxAppend(void *box, void *control, int stretchy)
{
	uiBoxAppend((uiBox *) box, (uiControl *) control, stretchy);
}

void bridge_uiControlShow(void *control)
{
	uiControlShow((uiControl *) control);
}

void bridge_uiControlDestroy(void *control)
{
	uiControlDestroy((uiControl *) control);
}

void *bridge_uiNewEntry(void)
{
	return (void *) uiNewEntry();
}

char *bridge_uiEntryText(void *entry)
{
	return uiEntryText((uiEntry *) entry);
}

void bridge_uiEntrySetText(void *entry, const char *text)
{
	uiEntrySetText((uiEntry *) entry, text);
}

void bridge_uiFreeText(char *text)
{
	uiFreeText(text);
}

void *bridge_uiNewForm(void)
{
	return (void *) uiNewForm();
}

void bridge_uiFormAppend(void *form, const char *label, void *control, int stretchy)
{
	uiFormAppend((uiForm *) form, label, (uiControl *) control, stretchy);
}

/* Menus --------------------------------------------------------------- */

void *bridge_uiNewMenu(const char *name)
{
	return (void *) uiNewMenu(name);
}

void *bridge_uiMenuAppendItem(void *menu, const char *name)
{
	return (void *) uiMenuAppendItem((uiMenu *) menu, name);
}

void *bridge_uiMenuAppendCheckItem(void *menu, const char *name)
{
	return (void *) uiMenuAppendCheckItem((uiMenu *) menu, name);
}

void *bridge_uiMenuAppendQuitItem(void *menu)
{
	return (void *) uiMenuAppendQuitItem((uiMenu *) menu);
}

void bridge_uiMenuAppendSeparator(void *menu)
{
	uiMenuAppendSeparator((uiMenu *) menu);
}

static void bridge_menu_item_clicked(uiMenuItem *sender, uiWindow *window, void *data)
{
	bridge_callback *cb = (bridge_callback *) data;
	(void) sender;
	(void) window;
	if (cb != NULL && cb->fn != NULL)
		cb->fn(cb->data);
}

void bridge_uiMenuItemOnClicked(void *item, void (*onClicked)(void *data), void *data)
{
	bridge_callback *cb = bridge_callback_new(onClicked, data);
	uiMenuItemOnClicked((uiMenuItem *) item, bridge_menu_item_clicked, cb);
}

void bridge_uiMenuItemSetChecked(void *item, int checked)
{
	uiMenuItemSetChecked((uiMenuItem *) item, checked);
}

int bridge_uiMenuItemChecked(void *item)
{
	return uiMenuItemChecked((uiMenuItem *) item);
}

void bridge_uiMenuItemDisable(void *item)
{
	uiMenuItemDisable((uiMenuItem *) item);
}

/* Custom drawing area ------------------------------------------------- */

#define BRIDGE_AREA_MAX 32

typedef struct bridge_area_callback {
	void (*draw)(void *context, double width, double height, void *data);
	void (*mouse)(double x, double y, int down, int up, int held, int count, void *data);
	int (*key)(int key, int extKey, int up, void *data);
	void *data;
} bridge_area_callback;

typedef struct bridge_area_entry {
	uiArea *area;
	bridge_area_callback cb;
	int used;
} bridge_area_entry;

static bridge_area_entry g_areas[BRIDGE_AREA_MAX];

static bridge_area_entry *bridge_area_find(uiArea *area)
{
	int i;
	for (i = 0; i < BRIDGE_AREA_MAX; i++)
		if (g_areas[i].used && g_areas[i].area == area)
			return &g_areas[i];
	return NULL;
}

static bridge_area_entry *bridge_area_alloc(uiArea *area)
{
	int i;
	for (i = 0; i < BRIDGE_AREA_MAX; i++)
		if (!g_areas[i].used) {
			g_areas[i].used = 1;
			g_areas[i].area = area;
			g_areas[i].cb.draw = NULL;
			g_areas[i].cb.mouse = NULL;
			g_areas[i].cb.key = NULL;
			g_areas[i].cb.data = NULL;
			return &g_areas[i];
		}
	return NULL;
}

static void bridge_area_draw(uiAreaHandler *ah, uiArea *a, uiAreaDrawParams *p)
{
	bridge_area_entry *e = bridge_area_find(a);
	(void) ah;
	if (e != NULL && e->cb.draw != NULL)
		e->cb.draw((void *) p->Context, p->AreaWidth, p->AreaHeight, e->cb.data);
}

static void bridge_area_mouse(uiAreaHandler *ah, uiArea *a, uiAreaMouseEvent *e)
{
	bridge_area_entry *ent = bridge_area_find(a);
	(void) ah;
	if (ent != NULL && ent->cb.mouse != NULL)
		ent->cb.mouse(e->X, e->Y, e->Down, e->Up, (int) (e->Held1To64 & 0xFF), e->Count, ent->cb.data);
}

static void bridge_area_crossed(uiAreaHandler *ah, uiArea *a, int left)
{
	(void) ah;
	(void) a;
	(void) left;
}

static void bridge_area_drag_broken(uiAreaHandler *ah, uiArea *a)
{
	(void) ah;
	(void) a;
}

static int bridge_area_key(uiAreaHandler *ah, uiArea *a, uiAreaKeyEvent *e)
{
	bridge_area_entry *ent = bridge_area_find(a);
	(void) ah;
	if (ent != NULL && ent->cb.key != NULL)
		return ent->cb.key((int) e->Key, (int) e->ExtKey, e->Up, ent->cb.data);
	return 0;
}

static uiAreaHandler bridge_area_handler = {
	bridge_area_draw,
	bridge_area_mouse,
	bridge_area_crossed,
	bridge_area_drag_broken,
	bridge_area_key,
};

void *bridge_uiNewArea(
	void (*draw)(void *context, double width, double height, void *data),
	void (*mouse)(double x, double y, int down, int up, int held, int count, void *data),
	int (*key)(int key, int extKey, int up, void *data),
	void *data)
{
	uiArea *area = uiNewArea(&bridge_area_handler);
	bridge_area_entry *e = bridge_area_alloc(area);

	if (e != NULL) {
		e->cb.draw = draw;
		e->cb.mouse = mouse;
		e->cb.key = key;
		e->cb.data = data;
	}

	return (void *) area;
}

void bridge_uiAreaQueueRedrawAll(void *area)
{
	uiAreaQueueRedrawAll((uiArea *) area);
}

void bridge_uiAreaSetSize(void *area, int width, int height)
{
	uiAreaSetSize((uiArea *) area, width, height);
}

/* Minimal draw helpers ------------------------------------------------- */

static void bridge_solid_brush(uiDrawBrush *brush, double r, double g, double b, double a)
{
	brush->Type = uiDrawBrushTypeSolid;
	brush->R = r;
	brush->G = g;
	brush->B = b;
	brush->A = a;
}

void bridge_drawFillRect(void *context, double x, double y, double width, double height,
                         double r, double g, double b, double a)
{
	uiDrawContext *c = (uiDrawContext *) context;
	uiDrawPath *path = uiDrawNewPath(uiDrawFillModeWinding);
	uiDrawBrush brush;

	bridge_solid_brush(&brush, r, g, b, a);
	uiDrawPathAddRectangle(path, x, y, width, height);
	uiDrawPathEnd(path);
	uiDrawFill(c, path, &brush);
	uiDrawFreePath(path);
}

void bridge_drawLine(void *context, double x1, double y1, double x2, double y2,
                     double r, double g, double b, double a, double thickness)
{
	uiDrawContext *c = (uiDrawContext *) context;
	uiDrawPath *path = uiDrawNewPath(uiDrawFillModeWinding);
	uiDrawBrush brush;
	uiDrawStrokeParams sp;

	bridge_solid_brush(&brush, r, g, b, a);

	memset(&sp, 0, sizeof(sp));
	sp.Cap = uiDrawLineCapFlat;
	sp.Join = uiDrawLineJoinMiter;
	sp.Thickness = thickness;
	sp.MiterLimit = uiDrawDefaultMiterLimit;

	uiDrawPathNewFigure(path, x1, y1);
	uiDrawPathLineTo(path, x2, y2);
	uiDrawPathEnd(path);
	uiDrawStroke(c, path, &brush, &sp);
	uiDrawFreePath(path);
}

static uiDrawPath *bridge_polygon_path(double x1, double y1, double x2, double y2, double x3, double y3)
{
	uiDrawPath *path = uiDrawNewPath(uiDrawFillModeWinding);
	uiDrawPathNewFigure(path, x1, y1);
	uiDrawPathLineTo(path, x2, y2);
	uiDrawPathLineTo(path, x3, y3);
	uiDrawPathCloseFigure(path);
	uiDrawPathEnd(path);
	return path;
}

void bridge_fillTriangle(void *context, double x1, double y1, double x2, double y2,
                         double x3, double y3, double r, double g, double b, double a)
{
	uiDrawContext *c = (uiDrawContext *) context;
	uiDrawPath *path = bridge_polygon_path(x1, y1, x2, y2, x3, y3);
	uiDrawBrush brush;

	bridge_solid_brush(&brush, r, g, b, a);
	uiDrawFill(c, path, &brush);
	uiDrawFreePath(path);
}

void bridge_fillCircle(void *context, double cx, double cy, double radius,
                       double r, double g, double b, double a)
{
	uiDrawContext *c = (uiDrawContext *) context;
	uiDrawPath *path = uiDrawNewPath(uiDrawFillModeWinding);
	uiDrawBrush brush;

	bridge_solid_brush(&brush, r, g, b, a);
	uiDrawPathNewFigureWithArc(path, cx, cy, radius, 0.0, 6.283185307179586, 0);
	uiDrawPathEnd(path);
	uiDrawFill(c, path, &brush);
	uiDrawFreePath(path);
}

void bridge_drawText(void *context, double x, double y, const char *text, double size,
                     double r, double g, double b, double a)
{
	uiDrawContext *c = (uiDrawContext *) context;
	uiFontDescriptor font;
	uiAttributedString *s;
	uiAttribute *color;
	uiDrawTextLayoutParams params;
	uiDrawTextLayout *layout;
	size_t len = strlen(text);

	if (len == 0)
		return;

	uiLoadControlFont(&font);
	font.Size = size;

	s = uiNewAttributedString(text);
	color = uiNewColorAttribute(r, g, b, a);
	uiAttributedStringSetAttribute(s, color, 0, len);

	params.String = s;
	params.DefaultFont = &font;
	params.Width = 1000.0;
	params.Align = uiDrawTextAlignLeft;

	layout = uiDrawNewTextLayout(&params);
	uiDrawText(c, layout, x, y);
	uiDrawFreeTextLayout(layout);

	uiFreeAttributedString(s);
	uiFreeFontDescriptor(&font);
}
