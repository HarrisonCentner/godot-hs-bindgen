#include "HsFFI.h"
#include "Rts.h"

// Haskell code cannot start its own runtime, so the GHC RTS is started when
// Godot loads this shared library and stopped when it unloads it.

static void hs_example_rts_init(void) __attribute__((constructor));
static void hs_example_rts_init(void)
{
  static char *argv[] = {"hs_example", 0}, **argv_ = argv;
  static int argc = 1;
  RtsConfig conf = defaultRtsConfig;
  conf.rts_opts_enabled = RtsOptsAll;
  // Signal handling stays with Godot. By default the RTS installs its own
  // SIGINT handler, which swallows the first Ctrl-C.
  conf.rts_opts = "-N2 --install-signal-handlers=no";
  hs_init_ghc(&argc, &argv_, conf);
}

static void hs_example_rts_fini(void) __attribute__((destructor));
static void hs_example_rts_fini(void)
{
  hs_exit();
}
