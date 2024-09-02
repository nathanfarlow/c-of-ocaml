#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uintptr_t value;
typedef uintptr_t unatint;
typedef intptr_t natint;

#define Is_int(v) (((v) & 1) != 0)
#define Is_block(v) (((v) & 1) == 0)

#define Val_int(x) (((value)(x) << 1) | 1)
#define Int_val(v) ((natint)(v) >> 1)

#define Tag_tuple 0
#define Tag_closure 1
#define Tag_string 2

#define Val_unit Val_int(0)

typedef unsigned char uchar;

typedef struct {
  unatint size;
  uchar tag;
  value data[];
} block;

typedef struct {
  value (*fun)(value *);
  unatint env_size;
  value env[];
} closure_t;

block *caml_alloc_block(unatint size, uchar tag) {
  block *b = malloc(sizeof(block) + size * sizeof(value));
  b->size = size;
  b->tag = tag;
  return b;
}

value caml_alloc_closure(value (*fun)(value *), unatint env_size) {
  block *b = caml_alloc_block(env_size + 1, Tag_closure);
  closure_t *c = (closure_t *)b->data;
  c->fun = fun;
  c->env_size = env_size;
  return (value)b;
}

value caml_copy_string(const char *s) {
  unatint len = strlen(s);
  unatint num_values_for_string = ((len + 1) / sizeof(value)) + 1;
  block *b = caml_alloc_block(num_values_for_string + 1, Tag_string);
  b->data[0] = len;
  memcpy(b->data + 1, s, len + 1);
  return (value)b;
}

value caml_alloc(natint size, uchar tag, ...) {
  block *b = caml_alloc_block(size, tag);
  va_list args;
  va_start(args, tag);
  natint i;
  for (i = 0; i < size; i++) {
    b->data[i] = va_arg(args, value);
  }
  va_end(args);
  return (value)b;
}

#define Field(v, i) (((block *)(v))->data[i])
#define Str_val(v) ((char *)&Field(v, 1))

value caml_putc(value c) {
  putchar(Int_val(c));
  return Val_unit;
}

value caml_getc(value _) { return Val_int(getchar()); }

#define Val_bool(x) ((x) ? Val_int(1) : Val_int(0))
#define Bool_val(v) (Int_val(v) != 0)

value caml_string_unsafe_get(value s, value i) {
  char c = Str_val(s)[Int_val(i)];
  return Val_int(c);
}
// free: 0, params: 0
value closure_0(value *env) {

block_0: //

block_61: //

block_173: //

block_213: //

block_236: //

block_199: //

block_234: //

block_116: //
  value a = Val_int(0);
  value i = a;
  value b = i;

block_128: // b
  value c = caml_string_unsafe_get(caml_copy_string("hello world!"), b);

block_238: //

block_194: //
  value d = caml_putc(c);

block_237: //
  value e = Val_int(Int_val(b) + Int_val(Val_int(1)));
  value f = Val_bool(Val_int(11) != b);
  if (Bool_val(f)) {
    value g = e;
    b = e;

    goto block_128;
  } else {
    goto block_144;
  }

block_144: //

block_233: //
  value h = caml_putc(Val_int(10));

block_235: //
  return Val_unit;
}

int main() {
  closure_0(NULL);
  return 0;
}
