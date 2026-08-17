/*s: stdlib.h */
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

/* Internal representation of strings. claude: length is uintptr_t, not
 * unsigned, so chars[] starts at exactly sizeof(uintptr_t) - matching the
 * same "+4"/"+8" offset fork-c--'s tests/tiger64/regenerate-riscv64.sh
 * generates for stdlibcmm.c--'s own bits32/bits64 header word; a fixed
 * 32-bit length field here would desync from a bits64 one there under a
 * 64-bit backend. On every existing 32-bit backend uintptr_t is 32 bits,
 * so this is unchanged. */
typedef struct _string {
  uintptr_t length;
  unsigned char chars[1];
} string;

/* standard library funcitons */
void      tig_print(string *s);
void      tig_printi(int n);
void      tig_flush(void);
string*   tig_getchar(void);
int       tig_ord(string *s);
string*   tig_chr(unsigned i);
uintptr_t tig_size(string* s);
uintptr_t tig_sizea(void* array);
string*  tig_substring(string*, unsigned first, unsigned n);
string*  tig_concat(string *a, string *b);
int      tig_not(int i);
void     tig_exit(int status);
/*e: stdlib.h */
