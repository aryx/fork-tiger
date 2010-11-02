/*s: client.c */
#include <assert.h>
#include <qc--interp.h>

#include "stdlib.h"
#include "gc.h"

#define BUFF_SIZE     256
#define VALSTACK_SIZE 256
#define ARGSPACE_SIZE 256
#define HEAP_SIZE     4096

typedef struct {
  Cmm_Cont cont;
  void     *stack_space;
  unsigned  stack_space_size;
  void     *limit_cookie;
} Cmm_TCB;

extern int      verbosity;
static unsigned stack_size = 65536;

static void* globals_backup = NULL;

Cmm_TCB *TCB_new(void) {
  Cmm_Dataptr  data;
  Cmm_TCB     *tcb = (Cmm_TCB *) malloc(sizeof(*tcb));
  assert(tcb != NULL);

  tcb->stack_space      = (Cmm_Dataptr) malloc(stack_size * sizeof(*data));
  mem_assert(tcb->stack_space);
  tcb->stack_space_size = stack_size;

  tcb->limit_cookie     = NULL;

  return tcb;
}

void TCB_free(Cmm_TCB *tcb) {
  free(tcb->stack_space);

  /* FIX make sure that 'cont' is freed? */
  free(tcb);
}

//extern void gc_set_thread(Cmm_Cont* t);

int main(int argc, char *argv[])
{
  verbosity = 0;

  if (Cmm_open(VALSTACK_SIZE, ARGSPACE_SIZE) != 0) {
    exit(1);
  }

  /* standard library functions */
  register_c_func("tig_print",     (void*)tig_print,     "pointer:void");
  register_c_func("tig_printi",    (void*)tig_printi,    "int:void");
  register_c_func("tig_flush",     (void*)tig_flush,     "void:void");
  register_c_func("tig_getchar",   (void*)tig_getchar,   "void:pointer");
  register_c_func("tig_ord",       (void*)tig_ord,       "pointer:int");
  register_c_func("tig_chr" ,      (void*)tig_chr,       "unsigned:pointer");
  register_c_func("tig_size" ,     (void*)tig_size,      "pointer:unsigned");
  register_c_func("tig_sizea" ,    (void*)tig_sizea,     "pointer:unsigned");
  register_c_func("tig_substring", (void*)tig_substring, "pointer,unsigned,unsigned:pointer");
  register_c_func("tig_concat",    (void*)tig_concat,    "pointer,pointer:pointer");
  register_c_func("tig_not",       (void*)tig_not,       "int:int");
  register_c_func("tig_exit",      (void*)tig_exit,      "int:void");

  /* GC functions  */
  register_c_func("tig_gc",    (void*)tig_gc,    "void:void");
  register_c_func("tig_alloc", (void*)tig_alloc, "unsigned:pointer");
  register_c_func("tig_compare_str", (void*)tig_compare_str,
				  "pointer,pointer:int");
  register_c_func("tig_bounds_check", (void*)tig_bounds_check,
				  "pointer,int,int:void");

  if (!load_assembly_unit(argv[1],SRC_FILE))
  {
    Cmm_Codeptr loc = cmm_find_export("tiger_main");
    if (loc == NULL) {
      fprintf(stderr, "error: cannot find procedure \"tiger_main\"\n");
    } else {
      Cmm_TCB     *tcb = TCB_new();
      globals_backup = malloc(Cmm_GlobalSize());
      assert(globals_backup);
      tcb->cont = Cmm_CreateThread(loc,
                                   (void*)&globals_backup,
                                   tcb->stack_space,
                                   tcb->stack_space_size,
                                   &(tcb->limit_cookie));
      //gc_set_thread(&(tcb->cont));
      gc_init(atoi(getenv("TIGER_HEAPSIZE")) || HEAP_SIZE);
      tcb->cont = Cmm_RunThread(&(tcb->cont));
      gc_finish();
      free(globals_backup);
      TCB_free(tcb);      
    }
  }
  Cmm_close();
  return 0;
}
/*e: client.c */
