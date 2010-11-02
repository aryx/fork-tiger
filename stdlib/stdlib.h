/*s: stdlib.h */
#include <stdio.h>
#include <stdlib.h>

/* Internal representation of strings */
typedef struct _string {
  unsigned length;
  unsigned char chars[1];
} string;

/* standard library funcitons */
void     tig_print(string *s);
void     tig_printi(int n);
void     tig_flush(void);
string*  tig_getchar(void);
int      tig_ord(string *s);
string*  tig_chr(unsigned i);
unsigned tig_size(string* s);
unsigned tig_sizea(void* array);
string*  tig_substring(string*, unsigned first, unsigned n);
string*  tig_concat(string *a, string *b);
int      tig_not(int i);
void     tig_exit(int status);
/*e: stdlib.h */
