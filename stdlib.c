# 44 "stdlib.nw"
#include <qc--runtime.h>
#include "stdlib.h"
#include "gc.h"
#include <string.h>
#include <assert.h>
# 76 "stdlib.nw"
unsigned tig_sizea(void* array) { return *((int*)array); }
unsigned tig_size(string *s)    { return s->length;      }
int      tig_not(int i)         { return !i;             }
void     tig_exit(int status)   { exit(status);          }
void     tig_flush()            { fflush(stdout);        }
void     tig_printi(int n)      { printf("%d", n);       }
void     tig_print(string *s)   { printf("%s", s->chars);}
# 123 "stdlib.nw"
void tig_bounds_check(void *array, int index, int line) {
  int size = tig_sizea(array);
  if (index < 0 || index >= size) {
    fprintf(stderr, "Runtime Error line(%d): Attempt to access "
            "array index %d for array of size %d\n",
            line, index, size);
    exit(1);
  }
}
# 136 "stdlib.nw"
int tig_ord(string *s) {
  if (s->length != 1) {
    fprintf(stderr, "Tiger program took ord of string of length %d\n",
            s->length);
    exit(1);
  }
  return s->chars[0];
}
# 148 "stdlib.nw"
int tig_compare_str(string *s, string *t) {
  int i;
  assert(t);
  assert(s);
  if (s == t) return 0;

  if (s->length == t->length)
    return strncmp(s->chars, t->chars, s->length);

  i = strncmp(s->chars, t->chars,
              (s->length < t->length ? s->length : t->length));

  if (i != 0) return i;
  if (s->length < t->length) return -1;
  return 1;
}
# 237 "stdlib.nw"
void unwinder(Cmm_Cont* k, unsigned exn_id) {
  Cmm_Activation a = Cmm_YoungestActivation(k); 
  do {
    if ((unsigned)Cmm_GetDescriptor(&a, 2) == 1) {
      Cmm_Cont* exn = Cmm_MakeUnwindCont(&a, 0, exn_id);
      Cmm_CutTo(exn);
      return;
    }
  } while(Cmm_ChangeActivation(&a));
  assert(0);
}
