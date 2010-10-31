# 79 "gc.nw"
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <qc--runtime.h>

/* global private state of GC */
static unsigned  heap_size  = 0;
static unsigned* heap       = NULL;
static unsigned* from_space = NULL;
static unsigned* to_space   = NULL;
static unsigned* alloc_ptr  = NULL;

/* This one is visible externally */
unsigned* space_end = NULL;

#define FORWARDED 0x80000000
#define SIZE_MASK 0x7FFFFFFF

#define gc_bits(x)         (*(unsigned*)(((unsigned)x) - sizeof(unsigned)))
#define forwarded(x)       (gc_bits(x) & FORWARDED)
#define forward_address(x) (*(unsigned*)(x))
#define size(x)            (gc_bits(x) & SIZE_MASK)

void set_forward_address(void* p, void* fp) {
  gc_bits(p) |= FORWARDED;
  *(unsigned*)p = (unsigned)fp;
}

/* flip also works for initialization */
void flip() {
  if (from_space == heap) {
    to_space = heap;
    from_space = (unsigned*)((unsigned)heap + heap_size);
  } else {
    from_space = heap;
    to_space = (unsigned*)((unsigned)heap + heap_size);
  }
  space_end = (unsigned*)((unsigned)from_space + heap_size);
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
  void *p = alloc_ptr + 1;
  assert(size > 0);
  size = (size + 7) & 0xFFFFFFFC;
  assert(size % 4 == 0);
  (*(unsigned*)alloc_ptr) = size & SIZE_MASK;
  alloc_ptr = (unsigned*)((unsigned)alloc_ptr + size);
  return p;
}

int is_pointer(unsigned p) {
  if (p < (unsigned)from_space || 
      p > (unsigned)from_space + heap_size) {
    return 0;
  }
  return 1;
}

unsigned* gc_forward(unsigned *p, int ptr) {
  void* addr;
  if (ptr == 0 || !is_pointer((unsigned)p)) return p;
  if (forwarded(p)) return (void*)forward_address(p);

  addr = internal_alloc(size(p) - 4);
  memcpy(addr, p, size(p) - 4);
  set_forward_address(p, addr);
  return addr;
}

void gc_copy(void) {
  unsigned* scan;
  for (scan = to_space; scan < alloc_ptr; scan++)
    *scan = (unsigned)gc_forward((unsigned*)*scan, -1);
}
# 179 "gc.nw"
void* tig_gc(Cmm_Cont* k) {
  Cmm_Activation a;
  alloc_ptr = to_space;
  space_end = (unsigned*)((unsigned)to_space + heap_size);

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
      unsigned* tig_fp          = Cmm_FindStackLabel(&a, 0);
      unsigned  stack_var_count = gc_data[0];

      assert(tig_fp);
      assert(gc_data[1] == 1);
      for (i = 1; i < stack_var_count; ++i)
        tig_fp[i] = (unsigned)gc_forward((void*)tig_fp[i], gc_data[i+1]);
    }

    /* The first local will be the pfp in a tiger procedure, but the
       forward function will ignore it. For stdlib functions we may
       need to collect the first argument.
     */
    for (i = 0; i < var_count; ++i) {
      int ptr_flg;
      unsigned** rootp = (unsigned **) Cmm_FindLocalVar(&a, i);
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
