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
  unatint total_provided = c->args_idx + num_args;
  value *new_args = malloc(total_provided * sizeof(value));

  memcpy(new_args, c->args, c->args_idx * sizeof(value));

  va_list args;
  va_start(args, num_args);
  for (unatint i = 0; i < num_args; i++) {
    new_args[c->args_idx + i] = va_arg(args, value);
  }
  va_end(args);

  value result;
  if (total_provided >= c->total_args) {
    result = c->fun(new_args);
    if (total_provided > c->total_args) {
      unatint excess_args = total_provided - c->total_args;
      result = caml_call(result, excess_args, new_args + c->total_args);
    }
  } else {
    result = caml_alloc_closure(c->fun, c->total_args - total_provided,
                                total_provided);
    closure_t *new_c = (closure_t *)(((block *)result)->data);
    memcpy(new_c->args, new_args, total_provided * sizeof(value));
  }

  free(new_args);
  return result;
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

/* TODO: Implement this */
value caml_register_global(value a, value b, value c) { return Val_unit; }
value c2265(value *env);
value c158(value *env);
value c2342(value *env);
value c0(value *env);
value c2265(value *env) {

  value a = env[0];
  value b = env[1];

b2265:
  value i = Val_bool(Int_val(Val_int(2)) <= Int_val(b));
  if (Bool_val(i)) {
    goto b2272;
  } else {
    goto b2269;
  }
b2272:
  value d = Val_int(Int_val(b) + Int_val(Val_int(-2)));
  value e = caml_call(a, 1, d);
  value f = Val_int(Int_val(b) + Int_val(Val_int(-1)));
  value g = caml_call(a, 1, f);
  value c = Val_int(Int_val(g) + Int_val(e));
  return c;
b2269:
  value h = Val_int(1);
  return h;
}

value c158(value *env) {

  value j = env[0];
  value k = env[1];

b158:
  value m = caml_ml_string_length(j);
  value n = caml_ml_string_length(k);
  value o = Val_int(Int_val(m) + Int_val(n));
  value l = caml_string_concat(j, k);
  value p = caml_bytes_of_string(l);
  return l;
}

value c2342(value *env) {
  value q;
  value r = env[0];

b2342:

  goto b2348;
b2348:

  goto b1835;
b1835:
  value u = caml_create_bytes(Val_int(1));
  value x = Val_int(0);
  q = x;

  goto b1847;
b1847:
  value w = caml_bytes_unsafe_set(u, q, r);
  value s = Val_int(Int_val(q) + Int_val(Val_int(1)));
  value v = Val_bool(Val_int(0) != q);
  if (Bool_val(v)) {
    q = s;

    goto b1847;
  } else {
    goto b1862;
  }

b1862:
  value t = caml_string_of_bytes(u);
  return t;
}

value c0(value *env) {
  value A;
  value B;
  value C;
  value D;
  value E;
  value F;
  value G;
  value H;
  value I;
  value J;
  value K;
  value L;
  value y;
  value z;

b0:
  value a4 = caml_alloc(2, 248, caml_copy_string("Out_of_memory"), Val_int(-1));
  value a5 = caml_alloc(2, 248, caml_copy_string("Sys_error"), Val_int(-2));
  value a6 = caml_alloc(2, 248, caml_copy_string("Failure"), Val_int(-3));
  value aM =
      caml_alloc(2, 248, caml_copy_string("Invalid_argument"), Val_int(-4));
  value a7 = caml_alloc(2, 248, caml_copy_string("End_of_file"), Val_int(-5));
  value a8 =
      caml_alloc(2, 248, caml_copy_string("Division_by_zero"), Val_int(-6));
  value a9 = caml_alloc(2, 248, caml_copy_string("Not_found"), Val_int(-7));
  value a_ = caml_alloc(2, 248, caml_copy_string("Match_failure"), Val_int(-8));
  value a$ =
      caml_alloc(2, 248, caml_copy_string("Stack_overflow"), Val_int(-9));
  value ba =
      caml_alloc(2, 248, caml_copy_string("Sys_blocked_io"), Val_int(-10));
  value bb =
      caml_alloc(2, 248, caml_copy_string("Assert_failure"), Val_int(-11));
  value bc = caml_alloc(2, 248, caml_copy_string("Undefined_recursive_module"),
                        Val_int(-12));
  value aN = caml_copy_string("Char.of_int");
  value ar = caml_copy_string("");
  value X = caml_copy_string("0");
  value a0 = caml_copy_string("-");
  value aZ = caml_copy_string("");
  value as = caml_alloc(1, 0, caml_copy_string(""));
  value bd = caml_register_global(
      Val_int(11), bc, caml_copy_string("Undefined_recursive_module"));
  value be =
      caml_register_global(Val_int(10), bb, caml_copy_string("Assert_failure"));
  value bf =
      caml_register_global(Val_int(9), ba, caml_copy_string("Sys_blocked_io"));
  value bg =
      caml_register_global(Val_int(8), a$, caml_copy_string("Stack_overflow"));
  value bh =
      caml_register_global(Val_int(7), a_, caml_copy_string("Match_failure"));
  value bi =
      caml_register_global(Val_int(6), a9, caml_copy_string("Not_found"));
  value bj = caml_register_global(Val_int(5), a8,
                                  caml_copy_string("Division_by_zero"));
  value bk =
      caml_register_global(Val_int(4), a7, caml_copy_string("End_of_file"));
  value bl = caml_register_global(Val_int(3), aM,
                                  caml_copy_string("Invalid_argument"));
  value bm = caml_register_global(Val_int(2), a6, caml_copy_string("Failure"));
  value bn =
      caml_register_global(Val_int(1), a5, caml_copy_string("Sys_error"));
  value bo =
      caml_register_global(Val_int(0), a4, caml_copy_string("Out_of_memory"));

  goto b218;
b218:
  value al = caml_alloc_closure(c158, 2, 0);

  goto b300;
b300:

  goto b519;
b519:

  goto b1671;
b1671:

  goto b1991;
b1991:

  goto b2166;
b2166:

  goto b2252;
b2252:

  goto b2295;
b2295:
  value a = caml_alloc_closure(c2265, 1, 1);
  add_arg(a, a);
  value a3 = Val_int(0);
  L = a3;

  goto b2311;
b2311:
  value aY = caml_call(a, 1, L);

  goto b2368;
b2368:

  goto b2285;
b2285:

  goto b2345;
b2345:

  goto b2063;
b2063:
  value a2 = Val_bool(Val_int(0) == aY);
  if (Bool_val(a2)) {
    goto b2072;
  } else {
    goto b2076;
  }
b2072:
  z = X;

  goto b2344;
b2344:

  goto b2343;
b2343:

  goto b2238;
b2238:

  goto b2347;
b2347:

  goto b1934;
b1934:
  value W = caml_ml_string_length(z);
  value M = Val_int(0);
  value S = Val_int(Int_val(W) + Int_val(Val_int(-1)));
  value V = Val_bool(Int_val(S) < Int_val(Val_int(0)));
  if (Bool_val(V)) {
    goto b1962;
  } else {
    y = M;

    goto b1946;
  }
b1962:

  goto b2346;
b2346:
  value P = caml_putc(Val_int(10));

  goto b2367;
b2367:
  value N = Val_int(Int_val(L) + Int_val(Val_int(1)));
  value O = Val_bool(Val_int(20) != L);
  if (Bool_val(O)) {
    L = N;

    goto b2311;
  } else {
    goto b2327;
  }

b2327:
  return Val_unit;
b1946:
  value T = caml_string_unsafe_get(z, y);

  goto b2370;
b2370:

  goto b2233;
b2233:
  value U = caml_putc(T);

  goto b2369;
b2369:
  value Q = Val_int(Int_val(y) + Int_val(Val_int(1)));
  value R = Val_bool(S != y);
  if (Bool_val(R)) {
    y = Q;

    goto b1946;
  } else {
    goto b1962;
  }

b2076:
  value a1 = Val_bool(Int_val(Val_int(0)) <= Int_val(aY));
  if (Bool_val(a1)) {
    goto b2084;
  } else {
    goto b2080;
  }
b2084:
  K = aZ;

  goto b2086;
b2086:
  value aX = Val_int(0);

  goto b2360;
b2360:
  I = aX;
  J = aY;

  goto b2032;
b2032:
  value aW = Val_bool(Val_int(0) == J);
  if (Bool_val(aW)) {
    goto b2036;
  } else {
    goto b2039;
  }
b2036:

  goto b2359;
b2359:
  value ac = caml_alloc_closure(c2342, 1, 0);

  goto b2358;
b2358:

  goto b2341;
b2341:

  goto b2349;
b2349:

  goto b743;
b743:
  if (Bool_val(I)) {
    goto b746;
  } else {
    goto b797;
  }
b746:
  value aC = Field(I, 1);
  value aE = Field(I, 0);
  if (Bool_val(aC)) {
    goto b753;
  } else {
    goto b788;
  }
b753:
  value aA = Field(aC, 1);
  value aD = Field(aC, 0);
  value au = caml_call(ac, 1, aE);
  value aF = caml_call(ac, 1, aD);
  value aG = Val_int(24029);
  value av = caml_alloc(2, 0, aF, aG);
  value aB = Val_int(1);

  goto b2364;
b2364:
  E = aA;
  F = aB;
  G = av;

  goto b803;
b803:
  if (Bool_val(E)) {
    goto b806;
  } else {
    goto b861;
  }
b806:
  value $ = Field(E, 1);
  value ab = Field(E, 0);
  if (Bool_val($)) {
    goto b813;
  } else {
    goto b849;
  }
b813:
  value Y = Field($, 1);
  value aa = Field($, 0);
  value ad = caml_call(ac, 1, ab);
  value ae = caml_call(ac, 1, aa);
  value af = Val_int(24029);
  value _ = caml_alloc(2, 0, ae, af);
  value ag = caml_alloc(2, 0, ad, _);
  Field(G, Int_val(F)) = ag;
  value Z = Val_int(1);
  E = Y;
  F = Z;
  G = _;

  goto b803;

b849:
  value aw = caml_call(ac, 1, ab);
  value ax = Val_int(0);
  value ay = caml_alloc(2, 0, aw, ax);
  Field(G, Int_val(F)) = ay;

  goto b2363;
b2363:
  value at = caml_alloc(2, 0, au, av);
  D = at;

  goto b2357;
b2357:

  goto b2356;
b2356:

  goto b2340;
b2340:

  goto b2350;
b2350:

  goto b1888;
b1888:

  goto b1891;
b1891:
  value aj = Field(as, 0);

  goto b1900;
b1900:
  if (Bool_val(D)) {
    goto b1903;
  } else {
    goto b1927;
  }
b1903:
  value ap = Field(D, 0);
  value aq = Field(D, 1);
  if (Bool_val(aq)) {
    goto b1910;
  } else {
    goto b1924;
  }
b1910:
  value ao = Field(D, 1);

  goto b2362;
b2362:
  B = ao;
  C = ap;

  goto b1065;
b1065:
  if (Bool_val(B)) {
    goto b1068;
  } else {
    goto b1081;
  }
b1068:
  value ah = Field(B, 1);
  value ak = Field(B, 0);

  goto b2372;
b2372:

  goto b1873;
b1873:
  value am = caml_call(al, 2, aj, ak);
  value ai = caml_call(al, 2, C, am);

  goto b2371;
b2371:
  B = ah;
  C = ai;

  goto b1065;

b1081:

  goto b2361;
b2361:
  A = C;

  goto b2355;
b2355:
  value an = caml_call(al, 2, K, A);
  z = an;

  goto b2344;

b1924:
  A = ap;

  goto b2355;

b1927:
  A = ar;

  goto b2355;

b861:
  value az = Val_int(0);
  Field(G, Int_val(F)) = az;

  goto b2363;

b788:
  value aI = caml_call(ac, 1, aE);
  value aJ = Val_int(0);
  value aH = caml_alloc(2, 0, aI, aJ);
  D = aH;

  goto b2357;

b797:
  value aK = Val_int(0);
  D = aK;

  goto b2357;

b2039:
  value aT = Val_int(Int_val(J) % Int_val(Val_int(10)));

  goto b2354;
b2354:

  goto b2155;
b2155:
  value aV = Val_bool(Int_val(Val_int(0)) <= Int_val(aT));
  if (Bool_val(aV)) {
    goto b2159;
  } else {
    goto b2162;
  }
b2159:
  H = aT;

  goto b2353;
b2353:
  value aQ = Val_int(Int_val(H) + Int_val(Val_int(48)));

  goto b2352;
b2352:

  goto b322;
b322:
  value aS = Val_bool(Int_val(Val_int(0)) <= Int_val(aQ));
  if (Bool_val(aS)) {
    goto b326;
  } else {
    goto b330;
  }
b326:
  value aR = Val_bool(Int_val(Val_int(255)) < Int_val(aQ));
  if (Bool_val(aR)) {
    goto b330;
  } else {
    goto b337;
  }
b330:

  goto b2366;
b2366:

  goto b206;
b206:
  value aL = caml_alloc(2, 0, aM, aN);
  caml_raise(aL);
b337:

  goto b2351;
b2351:
  value aO = caml_alloc(2, 0, aQ, I);
  value aP = Val_int(Int_val(J) / Int_val(Val_int(10)));
  I = aO;
  J = aP;

  goto b2032;

b2162:
  value aU = Val_int(-Int_val(aT));
  H = aU;

  goto b2353;

b2080:
  K = a0;

  goto b2086;
}

int main() {
  c0(NULL);
  return 0;
}