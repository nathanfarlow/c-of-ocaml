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

value caml_getc(value) { return Val_int(getchar()); }

#define Val_bool(x) ((x) ? Val_int(1) : Val_int(0))
#define Bool_val(v) (Int_val(v) != 0)

value caml_string_unsafe_get(value s, value i) {
  char c = Str_val(s)[Int_val(i)];
  return Val_int(c);
}
// free: 0, params: 0
value closure_0(value *env) {

block_0: //
  value a = Val_unit;
  value b = caml_copy_string("hello");
  value c = caml_alloc(2, 0, caml_alloc(1, 0, caml_copy_string("world")),
                       caml_alloc(2, 0, Val_int(0), Val_int(0)));

  goto block_61;
block_61: //

  goto block_173;
block_173: //

  goto block_247;
block_247: //
  value d = caml_alloc_closure(closure_233, 1)

                d->env[0] = a;
  value e = caml_alloc(1, 0, b);
  value f = caml_alloc(2, 0, e, c);

  goto block_287;
block_287: //
  value g = f;
  h = g;

  goto block_197;
block_197: // h
  if (Bool_val(h)) {
    goto block_200;
  } else {
    goto block_212;
  }
block_200: //
  value i = Field(h, 1);
  value j = Field(h, 0);

  goto block_291;
block_291: //

  goto block_215;
block_215: //
  if (Bool_val(j)) {
    goto block_218;
  } else {
    goto block_224;
  }
block_218: //
  value k = Field(j, 0);
  value l = caml_call1(d, k) // exact;

      goto block_290;
block_290: //
  value m = i;
  h = m;

  goto block_197;

block_224:                   //
  value n = caml_call1(d, b) // exact;

      goto block_290;

block_212: //

  goto block_286;
block_286: //
  return Val_unit;
}

// free: 1, params: 1
value closure_233(value *env) {
  value a = env[0];
  value o = env[1];

block_233: //

  goto block_285;
block_285: //

  goto block_116;
block_116: //
  value p = caml_ml_string_length(o);
  value q = Val_int(0);
  value r = Val_int(Int_val(p) + Int_val(Val_int(-1)));
  value s = Val_bool(Int_val(r) < Int_val(Val_int(0)));
  if (Bool_val(s)) {
    goto block_144;
  } else {
    value t = q;
    u = t;

    goto block_128;
  }
block_144: //

  goto block_284;
block_284: //
  value v = caml_putc(Val_int(10));
  return a;
block_128: // u
  value w = caml_string_unsafe_get(o, u);

  goto block_289;
block_289: //

  goto block_228;
block_228: //
  value x = caml_putc(w);

  goto block_288;
block_288: //
  value y = Val_int(Int_val(u) + Int_val(Val_int(1)));
  value z = Val_bool(r != u);
  if (Bool_val(z)) {
    value A = y;
    u = A;

    goto block_128;
  } else {
    goto block_144;
  }
}

int main() {
  closure_0(NULL);
  return 0;
}
