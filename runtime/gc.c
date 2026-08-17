/*s: gc.c */
/*s: gc.c  */
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stdint.h>
#include <qc--runtime.h>

/* claude: heap cells, the allocation header word, and every pointer this
 * file casts to/from an integer are widened from "unsigned" (always 32
 * bits) to uintptr_t (32 bits on a 32-bit target, 64 on a 64-bit one like
 * -alpha/-riscv64) - this is what "fork-tiger's runtime hardcoding
 * bits32 pointers" meant. uintptr_t equals plain "unsigned" on every
 * existing 32-bit backend, so this is a no-op there; the mechanical
 * counterpart on the qc-- .c-- side (bits32 -> bits64, the "+4" header
 * offset -> "+8", alignment masks widened) lives in fork-c--'s
 * tests/tiger64/regenerate-riscv64.sh, applied at generation time rather
 * than checked in here, so this file itself stays target-generic.
 *
 * gc_data (the GC descriptor table Cmm_GetDescriptor hands back) is the
 * one exception, deliberately NOT widened: those tables are qc---emitted
 * "bits32[] { ... }" counts and 0/1 pointer flags regardless of target
 * (see fork-tiger's frontend/frame.ml claude: comment on output_footer),
 * so gc_data stays "unsigned*" throughout. */

/* global private state of GC */
static unsigned    heap_size  = 0;
static uintptr_t*  heap       = NULL;
static uintptr_t*  from_space = NULL;
static uintptr_t*  to_space   = NULL;
static uintptr_t*  alloc_ptr  = NULL;

/* This one is visible externally */
uintptr_t* space_end = NULL;

#define FORWARDED ((uintptr_t)1 << (sizeof(uintptr_t) * 8 - 1))
#define SIZE_MASK (~FORWARDED)

#define gc_bits(x)         (*(uintptr_t*)(((uintptr_t)x) - sizeof(uintptr_t)))
#define forwarded(x)       (gc_bits(x) & FORWARDED)
#define forward_address(x) (*(uintptr_t*)(x))
#define size(x)            (gc_bits(x) & SIZE_MASK)

void set_forward_address(void* p, void* fp) {
  gc_bits(p) |= FORWARDED;
  *(uintptr_t*)p = (uintptr_t)fp;
}

/* flip also works for initialization */
void flip() {
  if (from_space == heap) {
    to_space = heap;
    from_space = (uintptr_t*)((uintptr_t)heap + heap_size);
  } else {
    from_space = heap;
    to_space = (uintptr_t*)((uintptr_t)heap + heap_size);
  }
  space_end = (uintptr_t*)((uintptr_t)from_space + heap_size);
}

void* gc_init(int size) {
  heap_size = size;
  heap = malloc(heap_size * 2);
  if (heap == NULL) {
    perror("could not create heap");
    exit(1);
  }
  bzero(heap, heap_size * 2);
  flip();
  alloc_ptr = from_space;
  return alloc_ptr;
}

void* internal_alloc(int size) {
  void *p = alloc_ptr + 1;  /* skip the header word */
  assert(size > 0);
  /* claude: same "+2*align-1, clear low align-1 bits" slop this had at
   * align=4 (originally hardcoded as "+7) & 0xFFFFFFFC"), generalized to
   * whatever align = sizeof(uintptr_t) is on this target. */
  size = (size + 2 * (int)sizeof(uintptr_t) - 1) & ~((int)sizeof(uintptr_t) - 1);
  assert(size % (int)sizeof(uintptr_t) == 0);
  (*(uintptr_t*)alloc_ptr) = (uintptr_t)size & SIZE_MASK;
  alloc_ptr = (uintptr_t*)((uintptr_t)alloc_ptr + size);
  return p;
}

int is_pointer(uintptr_t p) {
  if (p < (uintptr_t)from_space ||
      p > (uintptr_t)from_space + heap_size) {
    return 0;
  }
  return 1;
}

uintptr_t* gc_forward(uintptr_t *p, int ptr) {
  void* addr;
  if (ptr == 0 || !is_pointer((uintptr_t)p)) return p;
  if (forwarded(p)) return (void*)forward_address(p);

  addr = internal_alloc(size(p) - (int)sizeof(uintptr_t));
  memcpy(addr, p, size(p) - (int)sizeof(uintptr_t));
  set_forward_address(p, addr);
  return addr;
}

void gc_copy(void) {
  uintptr_t* scan;
  for (scan = to_space; scan < alloc_ptr; scan++)
    *scan = (uintptr_t)gc_forward((uintptr_t*)*scan, -1);
}
/*x: gc.c  */
void* tig_gc(Cmm_Cont* k) {
  Cmm_Activation a;
  alloc_ptr = to_space;
  space_end = (uintptr_t*)((uintptr_t)to_space + heap_size);

  a = Cmm_YoungestActivation(k);   // ignore call_gc activation
  while (Cmm_ChangeActivation(&a))
  {
    int i;
    unsigned  var_count = Cmm_LocalVarCount(&a);
    unsigned* gc_data   = Cmm_GetDescriptor(&a, 1);

    assert(!gc_data || gc_data[gc_data[0]+1] == var_count);

    /* If we have gc_data and stack vars, then we are in a proper
       tiger function. The first stack var will be the pfp and we can
       safely skip it. The assertion checks that the first stack var is a
       pointer -- it should be the parent frame pointer.
     */
    if (gc_data && gc_data[0] > 0) {
      uintptr_t* tig_fp          = Cmm_FindStackLabel(&a, 0);
      unsigned   stack_var_count = gc_data[0];

      assert(tig_fp);
      assert(gc_data[1] == 1);
      for (i = 1; i < stack_var_count; ++i)
        tig_fp[i] = (uintptr_t)gc_forward((void*)tig_fp[i], gc_data[i+1]);
    }

    /* The first local will be the pfp in a tiger procedure, but the
       forward function will ignore it. For stdlib functions we may
       need to collect the first argument.
     */
    for (i = 0; i < var_count; ++i) {
      int ptr_flg;
      uintptr_t** rootp = (uintptr_t **) Cmm_FindLocalVar(&a, i);
      if (rootp != NULL) {
        if (gc_data) ptr_flg = gc_data[gc_data[0] + 2 + i];
        else         ptr_flg = -1;
        *rootp = gc_forward(*rootp, ptr_flg);
      }
    }
  }
  gc_copy();
  bzero(from_space, heap_size);
  flip();
  return alloc_ptr;
}
/*e: gc.c  */
/*e: gc.c */
