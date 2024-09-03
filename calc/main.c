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

value caml_create_bytes(value len) {
  unatint num_values_for_string = len / sizeof(value) + 1;
  block *b = caml_alloc_block(num_values_for_string + 1, Tag_string);
  b->data[0] = Val_int(len);
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
value closure_2190(value *env);
value closure_2217(value *env);
value closure_2018(value *env);
value closure_0(value *env);
value closure_1843(value *env);
value closure_1866(value *env);
value closure_1883(value *env);
value closure_2002(value *env);
value closure_1828(value *env);
// free: 4, params: 1
value closure_2190(value *env) {

  value a = env[0];
  value b = env[1];
  value c = env[2];
  value d = env[3];
  value e = env[4];

block_2190:; //
  value f = Val_bool(Val_int(0) == e);
  if (Bool_val(f)) {
    goto block_2194;
  } else {
    goto block_2197;
  }
block_2194:; //
  return a;
block_2197:; //
  value g = Val_int(Int_val(e) % Int_val(Val_int(10)));
  value h = Val_int(Int_val(e) / Int_val(Val_int(10)));
  value i = caml_call(d, 1, h) /* exact */;

block_2343:; //

block_1730:; //
  value j = Val_bool(Int_val(Val_int(0)) <= Int_val(g));
  if (Bool_val(j)) {
    goto block_1734;
  } else {
    goto block_1741;
  }
block_1734:; //
  value k = Val_bool(Int_val(Val_int(10)) <= Int_val(g));
  if (Bool_val(k)) {
    goto block_1741;
  } else {
    goto block_1748;
  }
block_1741:; //
  value l = caml_alloc(2, 0, b, c);
  caml_raise(l);
block_1748:; //
  value m = caml_string_unsafe_get(caml_copy_string("0123456789"), g);

block_2342:; //
  value n = caml_putc(m);
  return a;
}

// free: 3, params: 1
value closure_2217(value *env) {

  value a = env[0];
  value b = env[1];
  value c = env[2];
  value o = env[3];

block_2217:; //
  value d = caml_alloc_closure(closure_2190, 1, 4);
  add_arg(d, a);
  add_arg(d, b);
  add_arg(d, c);
  add_arg(d, d);
  value p = Val_bool(Int_val(Val_int(0)) <= Int_val(o));
  if (Bool_val(p)) {
    goto block_2235;
  } else {
    goto block_2226;
  }
block_2235:; //
  value q = Val_bool(Val_int(0) == o);
  if (Bool_val(q)) {
    goto block_2239;
  } else {
    goto block_2245;
  }
block_2239:; //
  value r = caml_putc(Val_int(48));
  return a;
block_2245:; //
  value s = caml_call(d, 1, o) /* exact */;
  return s;
block_2226:; //
  value t = caml_putc(Val_int(45));
  value u = Val_int(-Int_val(o));
  value v = caml_call(d, 1, u) /* exact */;
  return v;
}

// free: 0, params: 1
value closure_2018(value *env) {

  value w = env[0];

block_2018:; //
  value x = caml_alloc_closure(closure_2002, 1, 1);
  add_arg(x, x);
  value y = caml_call(x, 1, w) /* exact */;

block_2350:; //

block_2349:; //
  value z = Val_int(Int_val(y) << Int_val(Val_int(8)));
  return z;
}

// free: 0, params: 0
value closure_0(value *env) {

block_0:; //
  value a = Val_unit /* aka undefined */;
  value b =
      caml_alloc(2, 248, caml_copy_string("Invalid_argument"), Val_int(-4));
  value c = caml_copy_string("String.get");

block_218:; //

block_300:; //

block_1433:; //

block_1753:; //

block_2074:; //
  value A = caml_alloc_closure(closure_1866, 1, 0);

  value B = caml_alloc_closure(closure_1843, 2, 0);

  value C = caml_alloc_closure(closure_1828, 2, 0);

block_2348:; //

block_2028:; //
  value D = caml_alloc_closure(closure_2018, 1, 0);

  value E = caml_alloc_closure(closure_1883, 2, 2);
  add_arg(E, E);
  add_arg(E, B);

block_2347:; //

block_2249:; //
  value F = caml_alloc_closure(closure_2217, 1, 3);
  add_arg(F, a);
  add_arg(F, b);
  add_arg(F, c);

block_2384:; //

block_1875:; //
  value G = Val_int(512);

block_2383:; //

block_2382:; //

block_2380:; //
  value H = Val_int(256);

block_2381:; //
  value I = caml_call(C, 2, H, G) /* exact */;

block_2379:; //

block_2377:; //
  value J = Val_int(256000);

block_2378:; //

block_2376:; //

block_1954:; //
  value K = Val_int(7);
  value L = caml_call(D, 1, K) /* exact */;
  value M = Val_int(7);
  value N = caml_call(E, 2, I, M) /* exact */;
  value O = caml_call(C, 2, N, L) /* exact */;
  value P = Val_int(5);
  value Q = caml_call(D, 1, P) /* exact */;
  value R = Val_int(5);
  value S = caml_call(E, 2, I, R) /* exact */;
  value T = caml_call(C, 2, S, Q) /* exact */;
  value U = Val_int(3);
  value V = caml_call(D, 1, U) /* exact */;
  value W = Val_int(3);
  value X = caml_call(E, 2, I, W) /* exact */;
  value Y = caml_call(C, 2, X, V) /* exact */;

block_2360:; //

block_2358:; //
  value Z = Val_int(Int_val(I) - Int_val(Y));

block_2359:; //

block_2357:; //

block_2355:; //
  value _ = Val_int(Int_val(Z) + Int_val(T));

block_2356:; //

block_2354:; //

block_2353:; //
  value $ = Val_int(Int_val(_) - Int_val(O));

block_2375:; //
  value aa = caml_call(B, 2, $, J) /* exact */;

block_2374:; //

block_2372:; //
  value ab = Val_int(256000);

block_2373:; //

block_2371:; //

block_1902:; //
  value ac = Val_int(6);
  value ad = caml_call(D, 1, ac) /* exact */;
  value ae = Val_int(6);
  value af = caml_call(E, 2, I, ae) /* exact */;
  value ag = caml_call(C, 2, af, ad) /* exact */;
  value ah = Val_int(4);
  value ai = caml_call(D, 1, ah) /* exact */;
  value aj = Val_int(4);
  value ak = caml_call(E, 2, I, aj) /* exact */;
  value al = caml_call(C, 2, ak, ai) /* exact */;
  value am = Val_int(2);
  value an = caml_call(D, 1, am) /* exact */;
  value ao = Val_int(2);
  value ap = caml_call(E, 2, I, ao) /* exact */;
  value aq = caml_call(C, 2, ap, an) /* exact */;

block_2369:; //

block_2367:; //

block_2368:; //

block_2366:; //

block_1853:; //
  value ar = Val_int(Int_val(Val_int(256)) - Int_val(aq));

block_2365:; //

block_2364:; //

block_1861:; //
  value as = Val_int(Int_val(ar) + Int_val(al));

block_2363:; //

block_2362:; //

block_2361:; //
  value at = Val_int(Int_val(as) - Int_val(ag));

block_2370:; //
  value au = caml_call(B, 2, at, ab) /* exact */;
  value av = caml_call(A, 1, aa) /* exact */;
  value aw = caml_call(F, 1, av) /* exact */;
  value ax = caml_putc(Val_int(32));
  value ay = caml_call(A, 1, au) /* exact */;
  value az = caml_call(F, 1, ay) /* exact */;
  return Val_unit;
}

// free: 0, params: 2
value closure_1843(value *env) {

  value aA = env[0];
  value aB = env[1];

block_1843:; //
  value aC = Val_int(Int_val(aA) * Int_val(aB));
  value aD = Val_int((unsigned long)Int_val(aC) >> Int_val(Val_int(8)));
  return aD;
}

// free: 0, params: 1
value closure_1866(value *env) {

  value aE = env[0];

block_1866:; //
  value aF = Val_int(Int_val(aE) + Int_val(Val_int(128)));
  value aG = Val_int((unsigned long)Int_val(aF) >> Int_val(Val_int(8)));
  return aG;
}

// free: 2, params: 2
value closure_1883(value *env) {

  value E = env[0];
  value B = env[1];
  value aH = env[2];
  value aI = env[3];

block_1883:; //
  value aJ = Val_bool(Val_int(0) == aI);
  if (Bool_val(aJ)) {
    goto block_1887;
  } else {
    goto block_1891;
  }
block_1887:; //

block_2352:; //

block_2351:; //
  value aK = Val_int(256);
  return aK;
block_1891:; //
  value aL = Val_int(Int_val(aI) + Int_val(Val_int(-1)));
  value aM = caml_call(E, 2, aH, aL) /* exact */;
  value aN = caml_call(B, 2, aH, aM) /* exact */;
  return aN;
}

// free: 1, params: 1
value closure_2002(value *env) {

  value x = env[0];
  value aO = env[1];

block_2002:; //
  value aP = Val_bool(Val_int(0) == aO);
  if (Bool_val(aP)) {
    goto block_2006;
  } else {
    goto block_2009;
  }
block_2006:; //
  value aQ = Val_int(1);
  return aQ;
block_2009:; //
  value aR = Val_int(Int_val(aO) + Int_val(Val_int(-1)));
  value aS = caml_call(x, 1, aR) /* exact */;
  value aT = Val_int(Int_val(aO) * Int_val(aS));
  return aT;
}

// free: 0, params: 2
value closure_1828(value *env) {

  value aU = env[0];
  value aV = env[1];

block_1828:; //
  value aW = Val_int(Int_val(aV) / Int_val(Val_int(2)));
  value aX = Val_int(Int_val(aU) << Int_val(Val_int(8)));

block_2346:; //

block_2344:; //
  value aY = Val_int(Int_val(aX) + Int_val(aW));

block_2345:; //
  value aZ = Val_int(Int_val(aY) / Int_val(aV));
  return aZ;
}

int main() {
  closure_0(NULL);
  return 0;
}
