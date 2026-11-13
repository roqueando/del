/*
 * ngspice_bridge.c — implementation of the Haskell-friendly ngspice API.
 *
 * ngspice streams simulation results through the SendData callback (one call
 * per accepted data point, containing every vector's value). The bridge stores
 * the latest values in a mutex-protected table that Haskell polls.
 */

#include <errno.h>
#include <pthread.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/time.h>
#include <time.h>
#include <unistd.h>

#include <ngspice/sharedspice.h>

#include "ngspice_bridge.h"

#define BRIDGE_MAX_VECS 512

typedef struct bridge_vec_entry {
	char name[64];
	double value;
} bridge_vec_entry;

static bridge_vec_entry g_vecs[BRIDGE_MAX_VECS];
static int g_vec_count = 0;
static pthread_mutex_t g_vec_mutex = PTHREAD_MUTEX_INITIALIZER;
static volatile int g_running = 0;
static volatile int g_delay_us = 0;

/* Signal-robust sleep: ngspice may install timers that interrupt usleep. */
static void bridge_sleep_us(long us)
{
	struct timespec ts;
	ts.tv_sec = us / 1000000L;
	ts.tv_nsec = (us % 1000000L) * 1000L;
	while (nanosleep(&ts, &ts) != 0 && errno == EINTR)
		;
}

/* --- ngspice callbacks ----------------------------------------------- */

static int bridge_send_char(char *s, int ident, void *user_data)
{
	/* Discard ngspice's console output. */
	(void) s;
	(void) ident;
	(void) user_data;
	return 0;
}

static int bridge_send_stat(char *s, int ident, void *user_data)
{
	(void) s;
	(void) ident;
	(void) user_data;
	return 0;
}

static int bridge_controlled_exit(int status, NG_BOOL immediate, NG_BOOL quit, int ident, void *user_data)
{
	(void) status;
	(void) immediate;
	(void) quit;
	(void) ident;
	(void) user_data;
	g_running = 0;
	return 0;
}

static int bridge_bg_running(NG_BOOL running, int ident, void *user_data)
{
	(void) ident;
	(void) user_data;
	g_running = running ? 1 : 0;
	return 0;
}

static int bridge_send_data(pvecvaluesall v, int num_structs, int ident, void *user_data)
{
	int i;
	(void) num_structs;
	(void) ident;
	(void) user_data;

	pthread_mutex_lock(&g_vec_mutex);
	g_vec_count = 0;
	for (i = 0; i < v->veccount && i < BRIDGE_MAX_VECS; i++) {
		pvecvalues vv = v->vecsa[i];
		strncpy(g_vecs[g_vec_count].name, vv->name, sizeof(g_vecs[0].name) - 1);
		g_vecs[g_vec_count].name[sizeof(g_vecs[0].name) - 1] = '\0';
		g_vecs[g_vec_count].value = vv->creal;
		g_vec_count++;
	}
	pthread_mutex_unlock(&g_vec_mutex);

	return 0;
}

static int bridge_send_init_data(pvecinfoall v, int ident, void *user_data)
{
	(void) v;
	(void) ident;
	(void) user_data;
	return 0;
}

/* --- public API ------------------------------------------------------- */

void bridge_ngspice_init(void)
{
	ngSpice_Init(bridge_send_char, bridge_send_stat, bridge_controlled_exit,
	             bridge_send_data, bridge_send_init_data, bridge_bg_running,
	             NULL);
}

void bridge_ngspice_command(const char *cmd)
{
	ngSpice_Command((char *) cmd);
}

void bridge_ngspice_load_netlist(const char *netlist)
{
	char *copy;
	char *p;
	char **arr;
	int i;
	int line_count = 1;

	copy = strdup(netlist);
	if (copy == NULL)
		return;

	for (p = copy; *p; p++)
		if (*p == '\n')
			line_count++;

	arr = (char **) malloc((line_count + 2) * sizeof(char *));
	if (arr == NULL) {
		free(copy);
		return;
	}

	i = 0;
	arr[i++] = copy;
	for (p = copy; *p; p++) {
		if (*p == '\n') {
			*p = '\0';
			arr[i++] = p + 1;
		}
	}
	arr[i] = NULL;

	ngSpice_Circ(arr);

	free(arr);
	free(copy);
}

double bridge_ngspice_get_value(const char *name)
{
	double result = 0.0;
	int i;

	pthread_mutex_lock(&g_vec_mutex);
	for (i = 0; i < g_vec_count; i++) {
		if (strcmp(g_vecs[i].name, name) == 0) {
			result = g_vecs[i].value;
			break;
		}
	}
	pthread_mutex_unlock(&g_vec_mutex);

	return result;
}

void bridge_ngspice_set_delay(int microseconds)
{
	g_delay_us = microseconds;
}

int bridge_ngspice_running(void)
{
	return g_running;
}

int bridge_ngspice_get_vector_length(const char *name)
{
	pvector_info vi = ngGet_Vec_Info((char *) name);
	return (vi != NULL) ? vi->v_length : 0;
}

double bridge_ngspice_get_vector_value(const char *name, int index)
{
	pvector_info vi = ngGet_Vec_Info((char *) name);
	if (vi != NULL && index >= 0 && index < vi->v_length)
		return vi->v_realdata[index];
	return 0.0;
}
