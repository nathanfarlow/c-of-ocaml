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
value closure_2233(value *env);

value closure_2313(value *env);

value closure_158(value *env);

value closure_0(value *env);

// free: 1, params: 1
value closure_2233(value *env) {

  value a = env[0];
  value b = env[1];

block_2233:; //
  value c = Val_bool(Int_val(Val_int(2)) <= Int_val(b));
  if (Bool_val(c)) {
    goto block_2240;
  } else {
    goto block_2237;
  }
block_2240:; //
  value d = Val_int(Int_val(b) + Int_val(Val_int(-2)));
  value e = caml_call(a, 1, d) /* exact */;
  value f = Val_int(Int_val(b) + Int_val(Val_int(-1)));
  value g = caml_call(a, 1, f) /* exact */;
  value h = Val_int(Int_val(g) + Int_val(e));
  return h;
block_2237:; //
  value i = Val_int(1);
  return i;
}

// free: 0, params: 1
value closure_2313(value *env) {
  value j;
  value k = env[0];

block_2313:; //

block_2319:; //

block_1835:; //
  value l = caml_create_bytes(Val_int(1));
  value m = Val_int(0);
  j = m;

block_1847:; // j
  value n = caml_bytes_unsafe_set(l, j, k);
  value o = Val_int(Int_val(j) + Int_val(Val_int(1)));
  value p = Val_bool(Val_int(0) != j);
  if (Bool_val(p)) {
    j = o;

    goto block_1847;
  } else {
    goto block_1862;
  }

block_1862:; //
  value q = caml_string_of_bytes(l);
  return q;
}

// free: 0, params: 2
value closure_158(value *env) {

  value r = env[0];
  value s = env[1];

block_158:; //
  value t = caml_ml_string_length(r);
  value u = caml_ml_string_length(s);
  value v = Val_int(Int_val(t) + Int_val(u));
  value w = caml_string_concat(r, s);
  value x = caml_bytes_of_string(w);
  return w;
}

// free: 0, params: 0
value closure_0(value *env) {
  value K;
  value G;
  value B;
  value D;
  value J;
  value H;
  value I;
  value A;
  value F;
  value y;
  value z;
  value E;
  value C;

block_0:; //
  value L =
      caml_alloc(2, 248, caml_copy_string("Invalid_argument"), Val_int(-4));
  value M = caml_copy_string("Char.of_int");
  value N = caml_copy_string("");
  value O = caml_copy_string("0");
  value P = caml_copy_string("-");
  value Q = caml_copy_string("");
  value R = caml_alloc(1, 0, caml_copy_string(""));

block_218:; //
  value S = caml_alloc_closure(closure_158, 2, 0);

block_300:; //

block_519:; //

block_1671:; //

block_1991:; //

block_2166:; //

block_2280:; //
  value a = caml_alloc_closure(closure_2233, 1, 1);
  add_arg(a, a);
  value T = Val_int(60);
  value U = caml_call(a, 1, T) /* exact */;

block_2339:; //

block_2253:; //

block_2318:; //

block_2063:; //
  value V = Val_bool(Val_int(0) == U);
  if (Bool_val(V)) {
    goto block_2072;
  } else {
    goto block_2076;
  }
block_2072:; //
  y = O;

block_2317:; // y

block_2316:; //

block_2266:; //

block_2315:; //

block_1934:; //
  value W = caml_ml_string_length(y);
  value X = Val_int(0);
  value Y = Val_int(Int_val(W) + Int_val(Val_int(-1)));
  value Z = Val_bool(Int_val(Y) < Int_val(Val_int(0)));
  if (Bool_val(Z)) {
    goto block_1962;
  } else {
    z = X;

    goto block_1946;
  }
block_1962:; //

block_2314:; //
  value _ = caml_putc(Val_int(10));

block_2338:; //
  return Val_unit;
block_1946:; // z
  value $ = caml_string_unsafe_get(y, z);

block_2341:; //

block_2261:; //
  value aa = caml_putc($);

block_2340:; //
  value ab = Val_int(Int_val(z) + Int_val(Val_int(1)));
  value ac = Val_bool(Y != z);
  if (Bool_val(ac)) {
    z = ab;

    goto block_1946;
  } else {
    goto block_1962;
  }

block_2076:; //
  value ad = Val_bool(Int_val(Val_int(0)) <= Int_val(U));
  if (Bool_val(ad)) {
    goto block_2084;
  } else {
    goto block_2080;
  }
block_2084:; //
  A = Q;

block_2086:; // A
  value ae = Val_int(0);

block_2331:; //
  B = ae;
  C = U;

block_2032:; // B, C
  value af = Val_bool(Val_int(0) == C);
  if (Bool_val(af)) {
    goto block_2036;
  } else {
    goto block_2039;
  }
block_2036:; //

block_2330:; //
  value ag = caml_alloc_closure(closure_2313, 1, 0);

block_2329:; //

block_2312:; //

block_2320:; //

block_743:; //
  if (Bool_val(B)) {
    goto block_746;
  } else {
    goto block_797;
  }
block_746:; //
  value ah = Field(B, 1);
  value ai = Field(B, 0);
  if (Bool_val(ah)) {
    goto block_753;
  } else {
    goto block_788;
  }
block_753:; //
  value aj = Field(ah, 1);
  value ak = Field(ah, 0);
  value al = caml_call(ag, 1, ai) /* exact */;
  value am = caml_call(ag, 1, ak) /* exact */;
  value an = Val_int(24029);
  value ao = caml_alloc(2, 0, am, an);
  value ap = Val_int(1);

block_2335:; //
  D = aj;
  E = ap;
  F = ao;

block_803:; // D, E, F
  if (Bool_val(D)) {
    goto block_806;
  } else {
    goto block_861;
  }
block_806:; //
  value aq = Field(D, 1);
  value ar = Field(D, 0);
  if (Bool_val(aq)) {
    goto block_813;
  } else {
    goto block_849;
  }
block_813:; //
  value as = Field(aq, 1);
  value at = Field(aq, 0);
  value au = caml_call(ag, 1, ar) /* exact */;
  value av = caml_call(ag, 1, at) /* exact */;
  value aw = Val_int(24029);
  value ax = caml_alloc(2, 0, av, aw);
  value ay = caml_alloc(2, 0, au, ax);
  Field(F, Int_val(E)) = ay;
  value az = Val_int(1);
  D = as;
  E = az;
  F = ax;

  goto block_803;

block_849:; //
  value aA = caml_call(ag, 1, ar) /* exact */;
  value aB = Val_int(0);
  value aC = caml_alloc(2, 0, aA, aB);
  Field(F, Int_val(E)) = aC;

block_2334:; //
  value aD = caml_alloc(2, 0, al, ao);
  G = aD;

block_2328:; // G

block_2327:; //

block_2311:; //

block_2321:; //

block_1888:; //

block_1891:; //
  value aE = Field(R, 0);

block_1900:; //
  if (Bool_val(G)) {
    goto block_1903;
  } else {
    goto block_1927;
  }
block_1903:; //
  value aF = Field(G, 0);
  value aG = Field(G, 1);
  if (Bool_val(aG)) {
    goto block_1910;
  } else {
    goto block_1924;
  }
block_1910:; //
  value aH = Field(G, 1);

block_2333:; //
  H = aH;
  I = aF;

block_1065:; // H, I
  if (Bool_val(H)) {
    goto block_1068;
  } else {
    goto block_1081;
  }
block_1068:; //
  value aI = Field(H, 1);
  value aJ = Field(H, 0);

block_2343:; //

block_1873:; //
  value aK = caml_call(S, 2, aE, aJ) /* exact */;
  value aL = caml_call(S, 2, I, aK) /* exact */;

block_2342:; //
  H = aI;
  I = aL;

  goto block_1065;

block_1081:; //

block_2332:; //
  J = I;

block_2326:; // J
  value aM = caml_call(S, 2, A, J) /* exact */;
  y = aM;

  goto block_2317;

block_1924:; //
  J = aF;

  goto block_2326;

block_1927:; //
  J = N;

  goto block_2326;

block_861:; //
  value aN = Val_int(0);
  Field(F, Int_val(E)) = aN;

  goto block_2334;

block_788:; //
  value aO = caml_call(ag, 1, ai) /* exact */;
  value aP = Val_int(0);
  value aQ = caml_alloc(2, 0, aO, aP);
  G = aQ;

  goto block_2328;

block_797:; //
  value aR = Val_int(0);
  G = aR;

  goto block_2328;

block_2039:; //
  value aS = Val_int(Int_val(C) % Int_val(Val_int(10)));

block_2325:; //

block_2155:; //
  value aT = Val_bool(Int_val(Val_int(0)) <= Int_val(aS));
  if (Bool_val(aT)) {
    goto block_2159;
  } else {
    goto block_2162;
  }
block_2159:; //
  K = aS;

block_2324:; // K
  value aU = Val_int(Int_val(K) + Int_val(Val_int(48)));

block_2323:; //

block_322:; //
  value aV = Val_bool(Int_val(Val_int(0)) <= Int_val(aU));
  if (Bool_val(aV)) {
    goto block_326;
  } else {
    goto block_330;
  }
block_326:; //
  value aW = Val_bool(Int_val(Val_int(255)) < Int_val(aU));
  if (Bool_val(aW)) {
    goto block_330;
  } else {
    goto block_337;
  }
block_330:; //

block_2337:; //

block_206:; //
  value aX = caml_alloc(2, 0, L, M);
  caml_raise(aX);
block_337:; //

block_2322:; //
  value aY = caml_alloc(2, 0, aU, B);
  value aZ = Val_int(Int_val(C) / Int_val(Val_int(10)));
  B = aY;
  C = aZ;

  goto block_2032;

block_2162:; //
  value a0 = Val_int(-Int_val(aS));
  K = a0;

  goto block_2324;

block_2080:; //
  A = P;

  goto block_2086;
}

int main() {
  closure_0(NULL);
  return 0;
}
