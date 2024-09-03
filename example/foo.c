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
value closure_158(value *env);
value closure_2313(value *env);
value closure_2233(value *env);
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
  value l;
  value k;
  value d;
  value h;
  value a;

block_0:; //
  value n =
      caml_alloc(2, 248, caml_copy_string("Invalid_argument"), Val_int(-4));
  value o = caml_copy_string("Char.of_int");
  value p = caml_copy_string("");
  value q = caml_copy_string("0");
  value r = caml_copy_string("-");
  value s = caml_copy_string("");
  value t = caml_alloc(1, 0, caml_copy_string(""));

block_218:; //
  value u = caml_alloc_closure(closure_158, 2, 0);

block_300:; //

block_519:; //

block_1671:; //

block_1991:; //

block_2166:; //

block_2280:; //
  value v = caml_alloc_closure(closure_2233, 1, 1);
  add_arg(v, v);
  value w = Val_int(60);
  value x = caml_call(v, 1, w) /* exact */;

block_2339:; //

block_2253:; //

block_2318:; //

block_2063:; //
  value y = Val_bool(Val_int(0) == x);
  if (Bool_val(y)) {
    goto block_2072;
  } else {
    goto block_2076;
  }
block_2072:; //
  a = q;

block_2317:; // a

block_2316:; //

block_2266:; //

block_2315:; //

block_1934:; //
  value z = caml_ml_string_length(a);
  value A = Val_int(0);
  value B = Val_int(Int_val(z) + Int_val(Val_int(-1)));
  value C = Val_bool(Int_val(B) < Int_val(Val_int(0)));
  if (Bool_val(C)) {
    goto block_1962;
  } else {
    b = A;

    goto block_1946;
  }
block_1962:; //

block_2314:; //
  value D = caml_putc(Val_int(10));

block_2338:; //
  return Val_unit;
block_1946:; // b
  value E = caml_string_unsafe_get(a, b);

block_2341:; //

block_2261:; //
  value F = caml_putc(E);

block_2340:; //
  value G = Val_int(Int_val(b) + Int_val(Val_int(1)));
  value H = Val_bool(B != b);
  if (Bool_val(H)) {
    b = G;

    goto block_1946;
  } else {
    goto block_1962;
  }

block_2076:; //
  value I = Val_bool(Int_val(Val_int(0)) <= Int_val(x));
  if (Bool_val(I)) {
    goto block_2084;
  } else {
    goto block_2080;
  }
block_2084:; //
  c = s;

block_2086:; // c
  value J = Val_int(0);

block_2331:; //
  d = J;
  e = x;

block_2032:; // d, e
  value K = Val_bool(Val_int(0) == e);
  if (Bool_val(K)) {
    goto block_2036;
  } else {
    goto block_2039;
  }
block_2036:; //

block_2330:; //
  value L = caml_alloc_closure(closure_2313, 1, 0);

block_2329:; //

block_2312:; //

block_2320:; //

block_743:; //
  if (Bool_val(d)) {
    goto block_746;
  } else {
    goto block_797;
  }
block_746:; //
  value M = Field(d, 1);
  value N = Field(d, 0);
  if (Bool_val(M)) {
    goto block_753;
  } else {
    goto block_788;
  }
block_753:; //
  value O = Field(M, 1);
  value P = Field(M, 0);
  value Q = caml_call(L, 1, N) /* exact */;
  value R = caml_call(L, 1, P) /* exact */;
  value S = Val_int(24029);
  value T = caml_alloc(2, 0, R, S);
  value U = Val_int(1);

block_2335:; //
  f = O;
  g = U;
  h = T;

block_803:; // f, g, h
  if (Bool_val(f)) {
    goto block_806;
  } else {
    goto block_861;
  }
block_806:; //
  value V = Field(f, 1);
  value W = Field(f, 0);
  if (Bool_val(V)) {
    goto block_813;
  } else {
    goto block_849;
  }
block_813:; //
  value X = Field(V, 1);
  value Y = Field(V, 0);
  value Z = caml_call(L, 1, W) /* exact */;
  value _ = caml_call(L, 1, Y) /* exact */;
  value $ = Val_int(24029);
  value aa = caml_alloc(2, 0, _, $);
  value ab = caml_alloc(2, 0, Z, aa);
  Field(h, Int_val(g)) = ab;
  value ac = Val_int(1);
  f = X;
  g = ac;
  h = aa;

  goto block_803;

block_849:; //
  value ad = caml_call(L, 1, W) /* exact */;
  value ae = Val_int(0);
  value af = caml_alloc(2, 0, ad, ae);
  Field(h, Int_val(g)) = af;

block_2334:; //
  value ag = caml_alloc(2, 0, Q, T);
  i = ag;

block_2328:; // i

block_2327:; //

block_2311:; //

block_2321:; //

block_1888:; //

block_1891:; //
  value ah = Field(t, 0);

block_1900:; //
  if (Bool_val(i)) {
    goto block_1903;
  } else {
    goto block_1927;
  }
block_1903:; //
  value ai = Field(i, 0);
  value aj = Field(i, 1);
  if (Bool_val(aj)) {
    goto block_1910;
  } else {
    goto block_1924;
  }
block_1910:; //
  value ak = Field(i, 1);

block_2333:; //
  j = ak;
  k = ai;

block_1065:; // j, k
  if (Bool_val(j)) {
    goto block_1068;
  } else {
    goto block_1081;
  }
block_1068:; //
  value al = Field(j, 1);
  value am = Field(j, 0);

block_2343:; //

block_1873:; //
  value an = caml_call(u, 2, ah, am) /* exact */;
  value ao = caml_call(u, 2, k, an) /* exact */;

block_2342:; //
  j = al;
  k = ao;

  goto block_1065;

block_1081:; //

block_2332:; //
  l = k;

block_2326:; // l
  value ap = caml_call(u, 2, c, l) /* exact */;
  a = ap;

  goto block_2317;

block_1924:; //
  l = ai;

  goto block_2326;

block_1927:; //
  l = p;

  goto block_2326;

block_861:; //
  value aq = Val_int(0);
  Field(h, Int_val(g)) = aq;

  goto block_2334;

block_788:; //
  value ar = caml_call(L, 1, N) /* exact */;
  value as = Val_int(0);
  value at = caml_alloc(2, 0, ar, as);
  i = at;

  goto block_2328;

block_797:; //
  value au = Val_int(0);
  i = au;

  goto block_2328;

block_2039:; //
  value av = Val_int(Int_val(e) % Int_val(Val_int(10)));

block_2325:; //

block_2155:; //
  value aw = Val_bool(Int_val(Val_int(0)) <= Int_val(av));
  if (Bool_val(aw)) {
    goto block_2159;
  } else {
    goto block_2162;
  }
block_2159:; //
  m = av;

block_2324:; // m
  value ax = Val_int(Int_val(m) + Int_val(Val_int(48)));

block_2323:; //

block_322:; //
  value ay = Val_bool(Int_val(Val_int(0)) <= Int_val(ax));
  if (Bool_val(ay)) {
    goto block_326;
  } else {
    goto block_330;
  }
block_326:; //
  value az = Val_bool(Int_val(Val_int(255)) < Int_val(ax));
  if (Bool_val(az)) {
    goto block_330;
  } else {
    goto block_337;
  }
block_330:; //

block_2337:; //

block_206:; //
  value aA = caml_alloc(2, 0, n, o);
  caml_raise(aA);
block_337:; //

block_2322:; //
  value aB = caml_alloc(2, 0, ax, d);
  value aC = Val_int(Int_val(e) / Int_val(Val_int(10)));
  d = aB;
  e = aC;

  goto block_2032;

block_2162:; //
  value aD = Val_int(-Int_val(av));
  m = aD;

  goto block_2324;

block_2080:; //
  c = r;

  goto block_2086;
}

// free: 0, params: 2
value closure_158(value *env) {

  value aE = env[0];
  value aF = env[1];

block_158:; //
  value aG = caml_ml_string_length(aE);
  value aH = caml_ml_string_length(aF);
  value aI = Val_int(Int_val(aG) + Int_val(aH));
  value aJ = caml_string_concat(aE, aF);
  value aK = caml_bytes_of_string(aJ);
  return aJ;
}

// free: 0, params: 1
value closure_2313(value *env) {
  value aL;
  value aM = env[0];

block_2313:; //

block_2319:; //

block_1835:; //
  value aN = caml_create_bytes(Val_int(1));
  value aO = Val_int(0);
  aL = aO;

block_1847:; // aL
  value aP = caml_bytes_unsafe_set(aN, aL, aM);
  value aQ = Val_int(Int_val(aL) + Int_val(Val_int(1)));
  value aR = Val_bool(Val_int(0) != aL);
  if (Bool_val(aR)) {
    aL = aQ;

    goto block_1847;
  } else {
    goto block_1862;
  }

block_1862:; //
  value aS = caml_string_of_bytes(aN);
  return aS;
}

// free: 1, params: 1
value closure_2233(value *env) {

  value v = env[0];
  value aT = env[1];

block_2233:; //
  value aU = Val_bool(Int_val(Val_int(2)) <= Int_val(aT));
  if (Bool_val(aU)) {
    goto block_2240;
  } else {
    goto block_2237;
  }
block_2240:; //
  value aV = Val_int(Int_val(aT) + Int_val(Val_int(-2)));
  value aW = caml_call(v, 1, aV) /* exact */;
  value aX = Val_int(Int_val(aT) + Int_val(Val_int(-1)));
  value aY = caml_call(v, 1, aX) /* exact */;
  value aZ = Val_int(Int_val(aY) + Int_val(aW));
  return aZ;
block_2237:; //
  value a0 = Val_int(1);
  return a0;
}

int main() {
  closure_0(NULL);
  return 0;
}
