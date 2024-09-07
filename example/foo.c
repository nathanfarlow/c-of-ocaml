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
value closure_2265(value *env);

value closure_158(value *env);

value closure_2342(value *env);

value closure_0(value *env);

// free: 1, params: 1
value closure_2265(value *env) {

  value a = env[0];
  value b = env[1];

block_2265:; //
  value c = Val_bool(Int_val(Val_int(2)) <= Int_val(b));
  if (Bool_val(c)) {
    goto block_2272;
  } else {
    goto block_2269;
  }
block_2272:; //
  value d = Val_int(Int_val(b) + Int_val(Val_int(-2)));
  value e = caml_call(a, 1, d) /* exact */;
  value f = Val_int(Int_val(b) + Int_val(Val_int(-1)));
  value g = caml_call(a, 1, f) /* exact */;
  value h = Val_int(Int_val(g) + Int_val(e));
  return h;
block_2269:; //
  value i = Val_int(1);
  return i;
}

// free: 0, params: 2
value closure_158(value *env) {

  value j = env[0];
  value k = env[1];

block_158:; //
  value l = caml_ml_string_length(j);
  value m = caml_ml_string_length(k);
  value n = Val_int(Int_val(l) + Int_val(m));
  value o = caml_string_concat(j, k);
  value p = caml_bytes_of_string(o);
  return o;
}

// free: 0, params: 1
value closure_2342(value *env) {
  value q;
  value r = env[0];

block_2342:; //

block_2348:; //

block_1835:; //
  value s = caml_create_bytes(Val_int(1));
  value t = Val_int(0);
  q = t;

block_1847:; // q
  value u = caml_bytes_unsafe_set(s, q, r);
  value v = Val_int(Int_val(q) + Int_val(Val_int(1)));
  value w = Val_bool(Val_int(0) != q);
  if (Bool_val(w)) {
    q = v;

    goto block_1847;
  } else {
    goto block_1862;
  }

block_1862:; //
  value x = caml_string_of_bytes(s);
  return x;
}

// free: 0, params: 0
value closure_0(value *env) {
  value K;
  value G;
  value B;
  value D;
  value L;
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
  value M =
      caml_alloc(2, 248, caml_copy_string("Invalid_argument"), Val_int(-4));
  value N = caml_copy_string("Char.of_int");
  value O = caml_copy_string("");
  value P = caml_copy_string("0");
  value Q = caml_copy_string("-");
  value R = caml_copy_string("");
  value S = caml_alloc(1, 0, caml_copy_string(""));

block_218:; //
  value T = caml_alloc_closure(closure_158, 2, 0);

block_300:; //

block_519:; //

block_1671:; //

block_1991:; //

block_2166:; //

block_2252:; //

block_2295:; //
  value a = caml_alloc_closure(closure_2265, 1, 1);
  add_arg(a, a);
  value U = Val_int(0);
  y = U;

block_2311:; // y
  value V = caml_call(a, 1, y) /* exact */;

block_2368:; //

block_2285:; //

block_2345:; //

block_2063:; //
  value W = Val_bool(Val_int(0) == V);
  if (Bool_val(W)) {
    goto block_2072;
  } else {
    goto block_2076;
  }
block_2072:; //
  z = P;

block_2344:; // z

block_2343:; //

block_2238:; //

block_2347:; //

block_1934:; //
  value X = caml_ml_string_length(z);
  value Y = Val_int(0);
  value Z = Val_int(Int_val(X) + Int_val(Val_int(-1)));
  value _ = Val_bool(Int_val(Z) < Int_val(Val_int(0)));
  if (Bool_val(_)) {
    goto block_1962;
  } else {
    A = Y;

    goto block_1946;
  }
block_1962:; //

block_2346:; //
  value $ = caml_putc(Val_int(10));

block_2367:; //
  value aa = Val_int(Int_val(y) + Int_val(Val_int(1)));
  value ab = Val_bool(Val_int(20) != y);
  if (Bool_val(ab)) {
    y = aa;

    goto block_2311;
  } else {
    goto block_2327;
  }

block_2327:; //
  return Val_unit;
block_1946:; // A
  value ac = caml_string_unsafe_get(z, A);

block_2370:; //

block_2233:; //
  value ad = caml_putc(ac);

block_2369:; //
  value ae = Val_int(Int_val(A) + Int_val(Val_int(1)));
  value af = Val_bool(Z != A);
  if (Bool_val(af)) {
    A = ae;

    goto block_1946;
  } else {
    goto block_1962;
  }

block_2076:; //
  value ag = Val_bool(Int_val(Val_int(0)) <= Int_val(V));
  if (Bool_val(ag)) {
    goto block_2084;
  } else {
    goto block_2080;
  }
block_2084:; //
  B = R;

block_2086:; // B
  value ah = Val_int(0);

block_2360:; //
  C = ah;
  D = V;

block_2032:; // C, D
  value ai = Val_bool(Val_int(0) == D);
  if (Bool_val(ai)) {
    goto block_2036;
  } else {
    goto block_2039;
  }
block_2036:; //

block_2359:; //
  value aj = caml_alloc_closure(closure_2342, 1, 0);

block_2358:; //

block_2341:; //

block_2349:; //

block_743:; //
  if (Bool_val(C)) {
    goto block_746;
  } else {
    goto block_797;
  }
block_746:; //
  value ak = Field(C, 1);
  value al = Field(C, 0);
  if (Bool_val(ak)) {
    goto block_753;
  } else {
    goto block_788;
  }
block_753:; //
  value am = Field(ak, 1);
  value an = Field(ak, 0);
  value ao = caml_call(aj, 1, al) /* exact */;
  value ap = caml_call(aj, 1, an) /* exact */;
  value aq = Val_int(24029);
  value ar = caml_alloc(2, 0, ap, aq);
  value as = Val_int(1);

block_2364:; //
  E = am;
  F = as;
  G = ar;

block_803:; // E, F, G
  if (Bool_val(E)) {
    goto block_806;
  } else {
    goto block_861;
  }
block_806:; //
  value at = Field(E, 1);
  value au = Field(E, 0);
  if (Bool_val(at)) {
    goto block_813;
  } else {
    goto block_849;
  }
block_813:; //
  value av = Field(at, 1);
  value aw = Field(at, 0);
  value ax = caml_call(aj, 1, au) /* exact */;
  value ay = caml_call(aj, 1, aw) /* exact */;
  value az = Val_int(24029);
  value aA = caml_alloc(2, 0, ay, az);
  value aB = caml_alloc(2, 0, ax, aA);
  Field(G, Int_val(F)) = aB;
  value aC = Val_int(1);
  E = av;
  F = aC;
  G = aA;

  goto block_803;

block_849:; //
  value aD = caml_call(aj, 1, au) /* exact */;
  value aE = Val_int(0);
  value aF = caml_alloc(2, 0, aD, aE);
  Field(G, Int_val(F)) = aF;

block_2363:; //
  value aG = caml_alloc(2, 0, ao, ar);
  H = aG;

block_2357:; // H

block_2356:; //

block_2340:; //

block_2350:; //

block_1888:; //

block_1891:; //
  value aH = Field(S, 0);

block_1900:; //
  if (Bool_val(H)) {
    goto block_1903;
  } else {
    goto block_1927;
  }
block_1903:; //
  value aI = Field(H, 0);
  value aJ = Field(H, 1);
  if (Bool_val(aJ)) {
    goto block_1910;
  } else {
    goto block_1924;
  }
block_1910:; //
  value aK = Field(H, 1);

block_2362:; //
  I = aK;
  J = aI;

block_1065:; // I, J
  if (Bool_val(I)) {
    goto block_1068;
  } else {
    goto block_1081;
  }
block_1068:; //
  value aL = Field(I, 1);
  value aM = Field(I, 0);

block_2372:; //

block_1873:; //
  value aN = caml_call(T, 2, aH, aM) /* exact */;
  value aO = caml_call(T, 2, J, aN) /* exact */;

block_2371:; //
  I = aL;
  J = aO;

  goto block_1065;

block_1081:; //

block_2361:; //
  K = J;

block_2355:; // K
  value aP = caml_call(T, 2, B, K) /* exact */;
  z = aP;

  goto block_2344;

block_1924:; //
  K = aI;

  goto block_2355;

block_1927:; //
  K = O;

  goto block_2355;

block_861:; //
  value aQ = Val_int(0);
  Field(G, Int_val(F)) = aQ;

  goto block_2363;

block_788:; //
  value aR = caml_call(aj, 1, al) /* exact */;
  value aS = Val_int(0);
  value aT = caml_alloc(2, 0, aR, aS);
  H = aT;

  goto block_2357;

block_797:; //
  value aU = Val_int(0);
  H = aU;

  goto block_2357;

block_2039:; //
  value aV = Val_int(Int_val(D) % Int_val(Val_int(10)));

block_2354:; //

block_2155:; //
  value aW = Val_bool(Int_val(Val_int(0)) <= Int_val(aV));
  if (Bool_val(aW)) {
    goto block_2159;
  } else {
    goto block_2162;
  }
block_2159:; //
  L = aV;

block_2353:; // L
  value aX = Val_int(Int_val(L) + Int_val(Val_int(48)));

block_2352:; //

block_322:; //
  value aY = Val_bool(Int_val(Val_int(0)) <= Int_val(aX));
  if (Bool_val(aY)) {
    goto block_326;
  } else {
    goto block_330;
  }
block_326:; //
  value aZ = Val_bool(Int_val(Val_int(255)) < Int_val(aX));
  if (Bool_val(aZ)) {
    goto block_330;
  } else {
    goto block_337;
  }
block_330:; //

block_2366:; //

block_206:; //
  value a0 = caml_alloc(2, 0, M, N);
  caml_raise(a0);
block_337:; //

block_2351:; //
  value a1 = caml_alloc(2, 0, aX, C);
  value a2 = Val_int(Int_val(D) / Int_val(Val_int(10)));
  C = a1;
  D = a2;

  goto block_2032;

block_2162:; //
  value a3 = Val_int(-Int_val(aV));
  L = a3;

  goto block_2353;

block_2080:; //
  B = Q;

  goto block_2086;
}

int main() {
  closure_0(NULL);
  return 0;
}
