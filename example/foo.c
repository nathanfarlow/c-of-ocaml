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
                         unatint num_env, ...) {
  block *b = caml_alloc_block(num_args + num_env + 3, Tag_closure);
  closure_t *c = (closure_t *)b->data;
  c->fun = fun;
  c->args_idx = num_env;
  c->total_args = num_args + num_env;

  va_list args;
  va_start(args, num_env);
  for (unatint i = 0; i < num_env; i++) {
    c->args[i] = va_arg(args, value);
  }
  va_end(args);

  return (value)b;
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
value closure_233(value *env);
// free: 0, params: 0
value closure_0(value *env) {

block_0: //
  value a = Val_unit;
  value b = caml_copy_string("hello");
  value c = caml_alloc(2, 0, caml_alloc(1, 0, caml_copy_string("world")),
                       caml_alloc(2, 0, Val_int(0), Val_int(0)));

block_61: //

block_173: //

block_247: //
  value d = caml_alloc_closure(closure_233, 1, 1, a);

  value e = caml_alloc(1, 0, b);
  value f = caml_alloc(2, 0, e, c);

block_287: //
  value n = f;
  value g = n;

block_197: // g
  if (Bool_val(g)) {
    goto block_200;
  } else {
    goto block_212;
  }
block_200: //
  value h = Field(g, 1);
  value i = Field(g, 0);

block_291: //

block_215: //
  if (Bool_val(i)) {
    goto block_218;
  } else {
    goto block_224;
  }
block_218: //
  value j = Field(i, 0);
  value k = caml_call(d, 1, j) /* exact */;

block_290: //
  value l = h;
  g = h;

  goto block_197;

block_224: //
  value m = caml_call(d, 1, b) /* exact */;

  goto block_290;

block_212: //

block_286: //
  return Val_unit;
}

// free: 1, params: 1
value closure_233(value *env) {
  value a = env[0];
  value o = env[1];

block_233: //

block_285: //

block_116: //
  value p = caml_ml_string_length(o);
  value q = Val_int(0);
  value r = Val_int(Int_val(p) + Int_val(Val_int(-1)));
  value s = Val_bool(Int_val(r) < Int_val(Val_int(0)));
  value u;
  if (Bool_val(s)) {
    goto block_144;
  } else {
    value t = q;
    u = q;

    goto block_128;
  }
block_144: //

block_284: //
  value v = caml_putc(Val_int(10));
  return a;
block_128: // u
  value w = caml_string_unsafe_get(o, u);

block_289: //

block_228: //
  value x = caml_putc(w);

block_288: //
  value y = Val_int(Int_val(u) + Int_val(Val_int(1)));
  value z = Val_bool(r != u);
  if (Bool_val(z)) {
    value A = y;
    u = y;

    goto block_128;
  } else {
    goto block_144;
  }
}

int main() {
  closure_0(NULL);
  return 0;
}
