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

value caml_raise(value v) {
  fprintf(stderr, "Exception raised: %ld\n", Int_val(v));
  exit(1);
}

value caml_create_bytes(value value_len) {
  unatint len = Int_val(value_len);
  unatint num_values_for_string = len / sizeof(value) + 1;
  block *b = caml_alloc_block(num_values_for_string + 1, Tag_string);
  b->data[0] = Val_int(len);
  Str_val((value)b)[len] = '\0';
  return (value)b;
}

value caml_bytes_unsafe_set(value s, value i, value c) {
  Str_val(s)[Int_val(i)] = Int_val(c);
  return Val_unit;
}

value caml_string_of_bytes(value s) { return s; }

value caml_string_concat(value s1, value s2) {
  unatint len1 = Int_val(Field(s1, 0));
  unatint len2 = Int_val(Field(s2, 0));
  unatint total_len = len1 + len2;
  value new_string = caml_create_bytes(Val_int(total_len));
  memcpy(Str_val(new_string), Str_val(s1), len1);
  memcpy(Str_val(new_string) + len1, Str_val(s2), len2);
  return new_string;
}

value caml_bytes_of_string(value s) { return s; }

value caml_ml_bytes_length(value s) { return Field(s, 0); }

value caml_blit_bytes(value src, value src_pos, value dst, value dst_pos,
                      value len) {
  unatint src_len = Field(src, 0);
  unatint dst_len = Field(dst, 0);
  unatint src_pos_val = Int_val(src_pos);
  unatint dst_pos_val = Int_val(dst_pos);
  unatint len_val = Int_val(len);

  if (src_pos_val + len_val > src_len || dst_pos_val + len_val > dst_len) {
    caml_raise(len);
  }

  memcpy(Str_val(dst) + dst_pos_val, Str_val(src) + src_pos_val, len_val);
  return Val_unit;
}
value closure_1875(value *env);

value closure_2222(value *env);

value closure_1898(value *env);

value closure_2034(value *env);

value closure_2249(value *env);

value closure_0(value *env);

value closure_1915(value *env);

value closure_1860(value *env);

value closure_2050(value *env);

// free: 0, params: 2
value closure_1875(value *env) {

  value a = env[0];
  value b = env[1];

block_1875:; //
  value c = Val_int(Int_val(a) * Int_val(b));
  value d = Val_int((unsigned long)Int_val(c) >> Int_val(Val_int(8)));
  return d;
}

// free: 4, params: 1
value closure_2222(value *env) {

  value e = env[0];
  value f = env[1];
  value g = env[2];
  value h = env[3];
  value i = env[4];

block_2222:; //
  value j = Val_bool(Val_int(0) == i);
  if (Bool_val(j)) {
    goto block_2226;
  } else {
    goto block_2229;
  }
block_2226:; //
  return e;
block_2229:; //
  value k = Val_int(Int_val(i) % Int_val(Val_int(10)));
  value l = Val_int(Int_val(i) / Int_val(Val_int(10)));
  value m = caml_call(h, 1, l) /* exact */;

block_2375:; //

block_1730:; //
  value n = Val_bool(Int_val(Val_int(0)) <= Int_val(k));
  if (Bool_val(n)) {
    goto block_1734;
  } else {
    goto block_1741;
  }
block_1734:; //
  value o = Val_bool(Int_val(Val_int(10)) <= Int_val(k));
  if (Bool_val(o)) {
    goto block_1741;
  } else {
    goto block_1748;
  }
block_1741:; //
  value p = caml_alloc(2, 0, f, g);
  caml_raise(p);
block_1748:; //
  value q = caml_string_unsafe_get(caml_copy_string("0123456789"), k);

block_2374:; //
  value r = caml_putc(q);
  return e;
}

// free: 0, params: 1
value closure_1898(value *env) {

  value s = env[0];

block_1898:; //
  value t = Val_int(Int_val(s) + Int_val(Val_int(128)));
  value u = Val_int((unsigned long)Int_val(t) >> Int_val(Val_int(8)));
  return u;
}

// free: 1, params: 1
value closure_2034(value *env) {

  value v = env[0];
  value w = env[1];

block_2034:; //
  value x = Val_bool(Val_int(0) == w);
  if (Bool_val(x)) {
    goto block_2038;
  } else {
    goto block_2041;
  }
block_2038:; //
  value y = Val_int(1);
  return y;
block_2041:; //
  value z = Val_int(Int_val(w) + Int_val(Val_int(-1)));
  value A = caml_call(v, 1, z) /* exact */;
  value B = Val_int(Int_val(w) * Int_val(A));
  return B;
}

// free: 3, params: 1
value closure_2249(value *env) {

  value e = env[0];
  value f = env[1];
  value g = env[2];
  value C = env[3];

block_2249:; //
  value h = caml_alloc_closure(closure_2222, 1, 4);
  add_arg(h, e);
  add_arg(h, f);
  add_arg(h, g);
  add_arg(h, h);
  value D = Val_bool(Int_val(Val_int(0)) <= Int_val(C));
  if (Bool_val(D)) {
    goto block_2267;
  } else {
    goto block_2258;
  }
block_2267:; //
  value E = Val_bool(Val_int(0) == C);
  if (Bool_val(E)) {
    goto block_2271;
  } else {
    goto block_2277;
  }
block_2271:; //
  value F = caml_putc(Val_int(48));
  return e;
block_2277:; //
  value G = caml_call(h, 1, C) /* exact */;
  return G;
block_2258:; //
  value H = caml_putc(Val_int(45));
  value I = Val_int(-Int_val(C));
  value J = caml_call(h, 1, I) /* exact */;
  return J;
}

// free: 0, params: 0
value closure_0(value *env) {

block_0:; //
  value e = Val_unit /* aka undefined */;
  value f =
      caml_alloc(2, 248, caml_copy_string("Invalid_argument"), Val_int(-4));
  value g = caml_copy_string("String.get");

block_218:; //

block_300:; //

block_1433:; //

block_1753:; //

block_1801:; //

block_2106:; //
  value K = caml_alloc_closure(closure_1898, 1, 0);

  value L = caml_alloc_closure(closure_1875, 2, 0);

  value M = caml_alloc_closure(closure_1860, 2, 0);

block_2380:; //

block_2060:; //
  value N = caml_alloc_closure(closure_2050, 1, 0);

  value O = caml_alloc_closure(closure_1915, 2, 2);
  add_arg(O, O);
  add_arg(O, L);

block_2379:; //

block_2281:; //
  value P = caml_alloc_closure(closure_2249, 1, 3);
  add_arg(P, e);
  add_arg(P, f);
  add_arg(P, g);

block_2416:; //

block_1907:; //
  value Q = Val_int(512);

block_2415:; //

block_2414:; //

block_2412:; //
  value R = Val_int(256);

block_2413:; //
  value S = caml_call(M, 2, R, Q) /* exact */;

block_2411:; //

block_2409:; //
  value T = Val_int(256000);

block_2410:; //

block_2408:; //

block_1986:; //
  value U = Val_int(7);
  value V = caml_call(N, 1, U) /* exact */;
  value W = Val_int(7);
  value X = caml_call(O, 2, S, W) /* exact */;
  value Y = caml_call(M, 2, X, V) /* exact */;
  value Z = Val_int(5);
  value _ = caml_call(N, 1, Z) /* exact */;
  value $ = Val_int(5);
  value aa = caml_call(O, 2, S, $) /* exact */;
  value ab = caml_call(M, 2, aa, _) /* exact */;
  value ac = Val_int(3);
  value ad = caml_call(N, 1, ac) /* exact */;
  value ae = Val_int(3);
  value af = caml_call(O, 2, S, ae) /* exact */;
  value ag = caml_call(M, 2, af, ad) /* exact */;

block_2392:; //

block_2390:; //
  value ah = Val_int(Int_val(S) - Int_val(ag));

block_2391:; //

block_2389:; //

block_2387:; //
  value ai = Val_int(Int_val(ah) + Int_val(ab));

block_2388:; //

block_2386:; //

block_2385:; //
  value aj = Val_int(Int_val(ai) - Int_val(Y));

block_2407:; //
  value ak = caml_call(L, 2, aj, T) /* exact */;

block_2406:; //

block_2404:; //
  value al = Val_int(256000);

block_2405:; //

block_2403:; //

block_1934:; //
  value am = Val_int(6);
  value an = caml_call(N, 1, am) /* exact */;
  value ao = Val_int(6);
  value ap = caml_call(O, 2, S, ao) /* exact */;
  value aq = caml_call(M, 2, ap, an) /* exact */;
  value ar = Val_int(4);
  value as = caml_call(N, 1, ar) /* exact */;
  value at = Val_int(4);
  value au = caml_call(O, 2, S, at) /* exact */;
  value av = caml_call(M, 2, au, as) /* exact */;
  value aw = Val_int(2);
  value ax = caml_call(N, 1, aw) /* exact */;
  value ay = Val_int(2);
  value az = caml_call(O, 2, S, ay) /* exact */;
  value aA = caml_call(M, 2, az, ax) /* exact */;

block_2401:; //

block_2399:; //

block_2400:; //

block_2398:; //

block_1885:; //
  value aB = Val_int(Int_val(Val_int(256)) - Int_val(aA));

block_2397:; //

block_2396:; //

block_1893:; //
  value aC = Val_int(Int_val(aB) + Int_val(av));

block_2395:; //

block_2394:; //

block_2393:; //
  value aD = Val_int(Int_val(aC) - Int_val(aq));

block_2402:; //
  value aE = caml_call(L, 2, aD, al) /* exact */;
  value aF = caml_call(K, 1, ak) /* exact */;
  value aG = caml_call(P, 1, aF) /* exact */;
  value aH = caml_putc(Val_int(32));
  value aI = caml_call(K, 1, aE) /* exact */;
  value aJ = caml_call(P, 1, aI) /* exact */;
  return Val_unit;
}

// free: 2, params: 2
value closure_1915(value *env) {

  value O = env[0];
  value L = env[1];
  value aK = env[2];
  value aL = env[3];

block_1915:; //
  value aM = Val_bool(Val_int(0) == aL);
  if (Bool_val(aM)) {
    goto block_1919;
  } else {
    goto block_1923;
  }
block_1919:; //

block_2384:; //

block_2383:; //
  value aN = Val_int(256);
  return aN;
block_1923:; //
  value aO = Val_int(Int_val(aL) + Int_val(Val_int(-1)));
  value aP = caml_call(O, 2, aK, aO) /* exact */;
  value aQ = caml_call(L, 2, aK, aP) /* exact */;
  return aQ;
}

// free: 0, params: 2
value closure_1860(value *env) {

  value aR = env[0];
  value aS = env[1];

block_1860:; //
  value aT = Val_int(Int_val(aS) / Int_val(Val_int(2)));
  value aU = Val_int(Int_val(aR) << Int_val(Val_int(8)));

block_2378:; //

block_2376:; //
  value aV = Val_int(Int_val(aU) + Int_val(aT));

block_2377:; //
  value aW = Val_int(Int_val(aV) / Int_val(aS));
  return aW;
}

// free: 0, params: 1
value closure_2050(value *env) {

  value aX = env[0];

block_2050:; //
  value v = caml_alloc_closure(closure_2034, 1, 1);
  add_arg(v, v);
  value aY = caml_call(v, 1, aX) /* exact */;

block_2382:; //

block_2381:; //
  value aZ = Val_int(Int_val(aY) << Int_val(Val_int(8)));
  return aZ;
}

int main() {
  closure_0(NULL);
  return 0;
}
