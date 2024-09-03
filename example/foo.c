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
  unatint args_idx;
  unatint total_args;
  value args[];
} closure_t;

block *caml_alloc_block(unatint size, uchar tag) {
  block *b = malloc(sizeof(block) + size * sizeof(value));
  b->size = size;
  b->tag = tag;
  return b;
}

value caml_alloc_closure(value (*fun)(value *), unatint num_args,
                         unatint num_env) {
  block *b = caml_alloc_block(num_args + num_env + 3, Tag_closure);
  closure_t *c = (closure_t *)b->data;
  c->fun = fun;
  c->args_idx = 0;
  c->total_args = num_args + num_env;
  return (value)b;
}

void add_arg(value closure, value arg) {
  closure_t *c = (closure_t *)(((block *)closure)->data);
  c->args[c->args_idx++] = arg;
}

value caml_call(value closure, unatint num_args, ...) {
  closure_t *c = (closure_t *)(((block *)closure)->data);

  // Copy existing args
  value *new_args = malloc((c->total_args) * sizeof(value));
  memcpy(new_args, c->args, c->args_idx * sizeof(value));

  va_list args;
  va_start(args, num_args);

  for (unatint i = 0; i < num_args && c->args_idx + i < c->total_args; i++) {
    new_args[c->args_idx + i] = va_arg(args, value);
  }

  va_end(args);

  unatint total_provided = c->args_idx + num_args;

  if (total_provided == c->total_args) {
    // Exact number of args provided
    value result = c->fun(new_args);
    free(new_args);
    return result;
  } else if (total_provided < c->total_args) {
    // Too few args, return new partial closure
    value new_closure = caml_alloc_closure(
        c->fun, c->total_args - total_provided, total_provided);
    closure_t *new_c = (closure_t *)(((block *)new_closure)->data);
    memcpy(new_c->args, new_args, total_provided * sizeof(value));
    free(new_args);
    return new_closure;
  } else {
    // Too many args, call function and recurse
    value result = c->fun(new_args);
    free(new_args);

    // Prepare args for recursive call
    unatint excess_args = num_args - (c->total_args - c->args_idx);
    va_list excess_va_list;
    va_start(excess_va_list, num_args);

    // Skip the args we've already used
    for (unatint i = 0; i < c->total_args - c->args_idx; i++) {
      va_arg(excess_va_list, value);
    }

    // Recursive call with remaining args
    value final_result = caml_call(result, excess_args, excess_va_list);
    va_end(excess_va_list);
    return final_result;
  }
}

value caml_copy_string(const char *s) {
  unatint len = strlen(s);
  unatint num_values_for_string = ((len + 1) / sizeof(value)) + 1;
  block *b = caml_alloc_block(num_values_for_string + 1, Tag_string);
  memcpy(b->data + 1, s, len + 1);
  b->data[0] = Val_int(len);
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

value caml_ml_string_length(value s) { return Field(s, 0); }

value caml_getc(value _) { return Val_int(getchar()); }

#define Val_bool(x) ((x) ? Val_int(1) : Val_int(0))
#define Bool_val(v) (Int_val(v) != 0)

value caml_string_unsafe_get(value s, value i) {
  char c = Str_val(s)[Int_val(i)];
  return Val_int(c);
}
value closure_0(value *env);
value closure_238(value *env);
value closure_258(value *env);
// free: 0, params: 0
value closure_0(value *env) {
  value b;
  value a;

block_0: //
  value c = Val_unit /* aka undefined */;

block_111: //

block_217: //

block_334: //
  value d = caml_alloc_closure(closure_238, 1, 1);
  add_arg(d, d);

block_401: //

block_320: //

block_392: //

block_178: //
  value e = Val_int(0);
  a = e;

block_190: // a
  value f = caml_string_unsafe_get(caml_copy_string("Fibonacci numbers:"), a);

block_403: //

block_315: //
  value g = caml_putc(f);

block_402: //
  value h = Val_int(Int_val(a) + Int_val(Val_int(1)));
  value i = Val_bool(Val_int(17) != a);
  if (Bool_val(i)) {
    a = h;

    goto block_190;
  } else {
    goto block_206;
  }

block_206: //

block_391: //
  value j = caml_putc(Val_int(10));

block_400: //
  value k = Val_int(0);
  b = k;

block_358: // b
  value l = caml_call(d, 1, b) /* exact */;

block_399: //

block_291: //
  value m = caml_alloc_closure(closure_258, 1, 2);
  add_arg(m, c);
  add_arg(m, m);
  value n = Val_bool(Int_val(Val_int(0)) <= Int_val(l));
  if (Bool_val(n)) {
    goto block_311;
  } else {
    goto block_302;
  }
block_311: //
  value o = caml_call(m, 1, l) /* exact */;

block_398: //
  value p = caml_putc(Val_int(10));
  value q = Val_int(Int_val(b) + Int_val(Val_int(1)));
  value r = Val_bool(Val_int(40) != b);
  if (Bool_val(r)) {
    b = q;

    goto block_358;
  } else {
    goto block_378;
  }

block_378: //
  return Val_unit;
block_302: //
  value s = caml_putc(Val_int(45));
  value t = Val_int(-Int_val(l));
  value u = caml_call(m, 1, t) /* exact */;

  goto block_398;
}

// free: 1, params: 1
value closure_238(value *env) {

  value d = env[0];
  value v = env[1];

block_238: //
  value w = Val_bool(Int_val(Val_int(2)) <= Int_val(v));
  if (Bool_val(w)) {
    goto block_245;
  } else {
    goto block_242;
  }
block_245: //
  value x = Val_int(Int_val(v) + Int_val(Val_int(-2)));
  value y = caml_call(d, 1, x) /* exact */;
  value z = Val_int(Int_val(v) + Int_val(Val_int(-1)));
  value A = caml_call(d, 1, z) /* exact */;
  value B = Val_int(Int_val(A) + Int_val(y));
  return B;
block_242: //
  return v;
}

// free: 2, params: 1
value closure_258(value *env) {

  value c = env[0];
  value m = env[1];
  value C = env[2];

block_258: //
  value D = Val_bool(Int_val(Val_int(10)) <= Int_val(C));
  if (Bool_val(D)) {
    goto block_272;
  } else {
    goto block_262;
  }
block_272: //
  value E = Val_int(Int_val(C) / Int_val(Val_int(10)));
  value F = caml_call(m, 1, E) /* exact */;
  value G = Val_int(Int_val(C) % Int_val(Val_int(10)));

block_395: //

block_393: //
  value H = caml_string_unsafe_get(caml_copy_string("0123456789"), G);

block_394: //
  value I = caml_putc(H);
  return c;
block_262: //

block_397: //

block_212: //
  value J = caml_string_unsafe_get(caml_copy_string("0123456789"), C);

block_396: //
  value K = caml_putc(J);
  return c;
}

int main() {
  closure_0(NULL);
  return 0;
}
