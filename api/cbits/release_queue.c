// A queue of reference-counted Godot objects whose Haskell handles have been
// garbage collected.
//
// The finalizer of a handle may run on any thread, at any time, but giving a
// reference back can destroy the object, which is only safe on Godot's main
// thread. So the finalizer only records the object here, and the main thread
// releases the recorded objects once per frame (Godot.Api.Runtime.releasePending).

#include <stdatomic.h>
#include <stdlib.h>

struct pending {
  void *object;
  struct pending *next;
};

// Pushed to by finalizers, on any thread.
static _Atomic(struct pending *) queued = NULL;

// Taken from `queued` in one step; only touched by the releasing thread.
static struct pending *taken = NULL;

// The finalizer: called by the GHC runtime with the object pointer.
void godot_hs_release_later(void *object)
{
  struct pending *entry = malloc(sizeof *entry);
  if (entry == NULL) {
    return; // Out of memory: leak the reference rather than crash.
  }
  entry->object = object;
  entry->next = atomic_load(&queued);
  while (!atomic_compare_exchange_weak(&queued, &entry->next, entry)) {
  }
}

// The next object to release, or NULL when there are none.
void *godot_hs_release_pop(void)
{
  if (taken == NULL) {
    taken = atomic_exchange(&queued, NULL);
  }
  if (taken == NULL) {
    return NULL;
  }
  struct pending *entry = taken;
  void *object = entry->object;
  taken = entry->next;
  free(entry);
  return object;
}
