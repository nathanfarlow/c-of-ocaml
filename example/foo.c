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
value closure_0(value *env);
value closure_2236(value *env);
value closure_2334(value *env);
value closure_158(value *env);
// free: 0, params: 0
value closure_0(value *env) {
  value i;
  value g;
  value c;
  value j;
  value m;
  value b;
  value e;
  value f;
  value n;
  value l;
  value k;
  value d;
  value h;
  value a;

block_0:; //
  value o =
      caml_alloc(2, 248, caml_copy_string("Invalid_argument"), Val_int(-4));
  value p = caml_copy_string("Char.of_int");
  value q = caml_copy_string("");
  value r = caml_copy_string("0");
  value s = caml_copy_string("-");
  value t = caml_copy_string("");
  value u = caml_alloc(1, 0, caml_copy_string(""));

block_218:; //
  value v = caml_alloc_closure(closure_158, 2, 0);

block_300:; //

block_519:; //

block_1671:; //

block_1991:; //

block_2166:; //

block_2283:; //
  value w = caml_alloc_closure(closure_2236, 1, 1);
  add_arg(w, w);
  value x = Val_int(0);
  a = x;

block_2303:; // a
  value y = caml_call(w, 1, a) /* exact */;

block_2360:; //

block_2256:; //

block_2339:; //

block_2063:; //
  value z = Val_bool(Val_int(0) == y);
  if (Bool_val(z)) {
    goto block_2072;
  } else {
    goto block_2076;
  }
block_2072:; //
  b = r;

block_2338:; // b

block_2337:; //

block_2269:; //

block_2336:; //

block_1934:; //
  value A = caml_ml_string_length(b);
  value B = Val_int(0);
  value C = Val_int(Int_val(A) + Int_val(Val_int(-1)));
  value D = Val_bool(Int_val(C) < Int_val(Val_int(0)));
  if (Bool_val(D)) {
    goto block_1962;
  } else {
    c = B;

    goto block_1946;
  }
block_1962:; //

block_2335:; //
  value E = caml_putc(Val_int(10));

block_2359:; //
  value F = Val_int(Int_val(a) + Int_val(Val_int(1)));
  value G = Val_bool(Val_int(10) != a);
  if (Bool_val(G)) {
    a = F;

    goto block_2303;
  } else {
    goto block_2319;
  }

block_2319:; //
  return Val_unit;
block_1946:; // c
  value H = caml_string_unsafe_get(b, c);

block_2362:; //

block_2264:; //
  value I = caml_putc(H);

block_2361:; //
  value J = Val_int(Int_val(c) + Int_val(Val_int(1)));
  value K = Val_bool(C != c);
  if (Bool_val(K)) {
    c = J;

    goto block_1946;
  } else {
    goto block_1962;
  }

block_2076:; //
  value L = Val_bool(Int_val(Val_int(0)) <= Int_val(y));
  if (Bool_val(L)) {
    goto block_2084;
  } else {
    goto block_2080;
  }
block_2084:; //
  d = t;

block_2086:; // d
  value M = Val_int(0);

block_2352:; //
  e = M;
  f = y;

block_2032:; // e, f
  value N = Val_bool(Val_int(0) == f);
  if (Bool_val(N)) {
    goto block_2036;
  } else {
    goto block_2039;
  }
block_2036:; //

block_2351:; //
  value O = caml_alloc_closure(closure_2334, 1, 0);

block_2350:; //

block_2333:; //

block_2341:; //

block_743:; //
  if (Bool_val(e)) {
    goto block_746;
  } else {
    goto block_797;
  }
block_746:; //
  value P = Field(e, 1);
  value Q = Field(e, 0);
  if (Bool_val(P)) {
    goto block_753;
  } else {
    goto block_788;
  }
block_753:; //
  value R = Field(P, 1);
  value S = Field(P, 0);
  value T = caml_call(O, 1, Q) /* exact */;
  value U = caml_call(O, 1, S) /* exact */;
  value V = Val_int(24029);
  value W = caml_alloc(2, 0, U, V);
  value X = Val_int(1);

block_2356:; //
  g = R;
  h = X;
  i = W;

block_803:; // g, h, i
  if (Bool_val(g)) {
    goto block_806;
  } else {
    goto block_861;
  }
block_806:; //
  value Y = Field(g, 1);
  value Z = Field(g, 0);
  if (Bool_val(Y)) {
    goto block_813;
  } else {
    goto block_849;
  }
block_813:; //
  value _ = Field(Y, 1);
  value $ = Field(Y, 0);
  value aa = caml_call(O, 1, Z) /* exact */;
  value ab = caml_call(O, 1, $) /* exact */;
  value ac = Val_int(24029);
  value ad = caml_alloc(2, 0, ab, ac);
  value ae = caml_alloc(2, 0, aa, ad);
  Field(i, Int_val(h)) = ae;
  value af = Val_int(1);
  g = _;
  h = af;
  i = ad;

  goto block_803;

block_849:; //
  value ag = caml_call(O, 1, Z) /* exact */;
  value ah = Val_int(0);
  value ai = caml_alloc(2, 0, ag, ah);
  Field(i, Int_val(h)) = ai;

block_2355:; //
  value aj = caml_alloc(2, 0, T, W);
  j = aj;

block_2349:; // j

block_2348:; //

block_2332:; //

block_2342:; //

block_1888:; //

block_1891:; //
  value ak = Field(u, 0);

block_1900:; //
  if (Bool_val(j)) {
    goto block_1903;
  } else {
    goto block_1927;
  }
block_1903:; //
  value al = Field(j, 0);
  value am = Field(j, 1);
  if (Bool_val(am)) {
    goto block_1910;
  } else {
    goto block_1924;
  }
block_1910:; //
  value an = Field(j, 1);

block_2354:; //
  k = an;
  l = al;

block_1065:; // k, l
  if (Bool_val(k)) {
    goto block_1068;
  } else {
    goto block_1081;
  }
block_1068:; //
  value ao = Field(k, 1);
  value ap = Field(k, 0);

block_2364:; //

block_1873:; //
  value aq = caml_call(v, 2, ak, ap) /* exact */;
  value ar = caml_call(v, 2, l, aq) /* exact */;

block_2363:; //
  k = ao;
  l = ar;

  goto block_1065;

block_1081:; //

block_2353:; //
  m = l;

block_2347:; // m
  value as = caml_call(v, 2, d, m) /* exact */;
  b = as;

  goto block_2338;

block_1924:; //
  m = al;

  goto block_2347;

block_1927:; //
  m = q;

  goto block_2347;

block_861:; //
  value at = Val_int(0);
  Field(i, Int_val(h)) = at;

  goto block_2355;

block_788:; //
  value au = caml_call(O, 1, Q) /* exact */;
  value av = Val_int(0);
  value aw = caml_alloc(2, 0, au, av);
  j = aw;

  goto block_2349;

block_797:; //
  value ax = Val_int(0);
  j = ax;

  goto block_2349;

block_2039:; //
  value ay = Val_int(Int_val(f) % Int_val(Val_int(10)));

block_2346:; //

block_2155:; //
  value az = Val_bool(Int_val(Val_int(0)) <= Int_val(ay));
  if (Bool_val(az)) {
    goto block_2159;
  } else {
    goto block_2162;
  }
block_2159:; //
  n = ay;

block_2345:; // n
  value aA = Val_int(Int_val(n) + Int_val(Val_int(48)));

block_2344:; //

block_322:; //
  value aB = Val_bool(Int_val(Val_int(0)) <= Int_val(aA));
  if (Bool_val(aB)) {
    goto block_326;
  } else {
    goto block_330;
  }
block_326:; //
  value aC = Val_bool(Int_val(Val_int(255)) < Int_val(aA));
  if (Bool_val(aC)) {
    goto block_330;
  } else {
    goto block_337;
  }
block_330:; //

block_2358:; //

block_206:; //
  value aD = caml_alloc(2, 0, o, p);
  caml_raise(aD);
block_337:; //

block_2343:; //
  value aE = caml_alloc(2, 0, aA, e);
  value aF = Val_int(Int_val(f) / Int_val(Val_int(10)));
  e = aE;
  f = aF;

  goto block_2032;

block_2162:; //
  value aG = Val_int(-Int_val(ay));
  n = aG;

  goto block_2345;

block_2080:; //
  d = s;

  goto block_2086;
}

// free: 1, params: 1
value closure_2236(value *env) {

  value w = env[0];
  value aH = env[1];

block_2236:; //
  value aI = Val_bool(Int_val(Val_int(2)) <= Int_val(aH));
  if (Bool_val(aI)) {
    goto block_2243;
  } else {
    goto block_2240;
  }
block_2243:; //
  value aJ = Val_int(Int_val(aH) + Int_val(Val_int(-2)));
  value aK = caml_call(w, 1, aJ) /* exact */;
  value aL = Val_int(Int_val(aH) + Int_val(Val_int(-1)));
  value aM = caml_call(w, 1, aL) /* exact */;
  value aN = Val_int(Int_val(aM) + Int_val(aK));
  return aN;
block_2240:; //
  value aO = Val_int(1);
  return aO;
}

// free: 0, params: 1
value closure_2334(value *env) {
  value aP;
  value aQ = env[0];

block_2334:; //

block_2340:; //

block_1835:; //
  value aR = caml_create_bytes(Val_int(1));
  value aS = Val_int(0);
  aP = aS;

block_1847:; // aP
  value aT = caml_bytes_unsafe_set(aR, aP, aQ);
  value aU = Val_int(Int_val(aP) + Int_val(Val_int(1)));
  value aV = Val_bool(Val_int(0) != aP);
  if (Bool_val(aV)) {
    aP = aU;

    goto block_1847;
  } else {
    goto block_1862;
  }

block_1862:; //
  value aW = caml_string_of_bytes(aR);
  return aW;
}

// free: 0, params: 2
value closure_158(value *env) {

  value aX = env[0];
  value aY = env[1];

block_158:; //
  value aZ = caml_ml_string_length(aX);
  value a0 = caml_ml_string_length(aY);
  value a1 = Val_int(Int_val(aZ) + Int_val(a0));
  value a2 = caml_string_concat(aX, aY);
  value a3 = caml_bytes_of_string(a2);
  return a2;
}

int main() {
  closure_0(NULL);
  return 0;
}
