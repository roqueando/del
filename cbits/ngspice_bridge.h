/*
 * ngspice_bridge.h — thin, Haskell-friendly C API on top of libngspice.
 *
 * The bridge runs ngspice in a background thread and caches the latest values
 * of every output vector (node voltages, branch currents, time) so that the
 * Haskell side can poll them cheaply with get_value().
 */

#ifndef NGSPICE_BRIDGE_H
#define NGSPICE_BRIDGE_H

#ifdef __cplusplus
extern "C" {
#endif

/* Initializes ngspice with the bridge's callback functions. */
void bridge_ngspice_init(void);

/* Sends an interactive command (e.g. "tran 1u 10m", "bg_run", "bg_halt"). */
void bridge_ngspice_command(const char *cmd);

/* Loads a netlist given as a single newline-separated string. */
void bridge_ngspice_load_netlist(const char *netlist);

/* Returns the latest value of the named vector (0.0 if unknown). */
double bridge_ngspice_get_value(const char *name);

/* Sets a per-data-point delay (microseconds) applied inside SendData, used to
 * throttle the streaming/animation rate. */
void bridge_ngspice_set_delay(int microseconds);

/* Returns 1 while ngspice's background simulation thread is running. */
int bridge_ngspice_running(void);

/* After a synchronous run, reads the full vector data. */
int bridge_ngspice_get_vector_length(const char *name);
double bridge_ngspice_get_vector_value(const char *name, int index);

#ifdef __cplusplus
}
#endif

#endif /* NGSPICE_BRIDGE_H */
