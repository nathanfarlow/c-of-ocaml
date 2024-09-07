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
value c1875(value *env);
value c2222(value *env);
value c1898(value *env);
value c2034(value *env);
value c2249(value *env);
value c0(value *env);
value c1915(value *env);
value c1860(value *env);
value c2050(value *env);
value c1875(value *env) {

  value a = env[0];
  value b = env[1];

b1875:
  value d = Val_int(Int_val(a) * Int_val(b));
  value c = Val_int((unatint)Int_val(d) >> Int_val(Val_int(8)));
  return c;
}

value c2222(value *env) {

  value e = env[0];
  value f = env[1];
  value g = env[2];
  value h = env[3];
  value i = env[4];

b2222:
  value r = Val_bool(Val_int(0) == i);
  if (Bool_val(r)) {
    goto b2226;
  } else {
    goto b2229;
  }
b2226:
  return e;
b2229:
  value m = Val_int(Int_val(i) % Int_val(Val_int(10)));
  value p = Val_int(Int_val(i) / Int_val(Val_int(10)));
  value q = caml_call(h, 1, p);

  goto b2375;
b2375:

  goto b1730;
b1730:
  value o = Val_bool(Int_val(Val_int(0)) <= Int_val(m));
  if (Bool_val(o)) {
    goto b1734;
  } else {
    goto b1741;
  }
b1734:
  value n = Val_bool(Int_val(Val_int(10)) <= Int_val(m));
  if (Bool_val(n)) {
    goto b1741;
  } else {
    goto b1748;
  }
b1741:
  value j = caml_alloc(2, 0, f, g);
  caml_raise(j);
b1748:
  value k = caml_string_unsafe_get(caml_copy_string("0123456789"), m);

  goto b2374;
b2374:
  value l = caml_putc(k);
  return e;
}

value c1898(value *env) {

  value s = env[0];

b1898:
  value u = Val_int(Int_val(s) + Int_val(Val_int(128)));
  value t = Val_int((unatint)Int_val(u) >> Int_val(Val_int(8)));
  return t;
}

value c2034(value *env) {

  value v = env[0];
  value w = env[1];

b2034:
  value B = Val_bool(Val_int(0) == w);
  if (Bool_val(B)) {
    goto b2038;
  } else {
    goto b2041;
  }
b2038:
  value x = Val_int(1);
  return x;
b2041:
  value z = Val_int(Int_val(w) + Int_val(Val_int(-1)));
  value A = caml_call(v, 1, z);
  value y = Val_int(Int_val(w) * Int_val(A));
  return y;
}

value c2249(value *env) {

  value e = env[0];
  value f = env[1];
  value g = env[2];
  value C = env[3];

b2249:
  value h = caml_alloc_closure(c2222, 1, 4);
  add_arg(h, e);
  add_arg(h, f);
  add_arg(h, g);
  add_arg(h, h);
  value J = Val_bool(Int_val(Val_int(0)) <= Int_val(C));
  if (Bool_val(J)) {
    goto b2267;
  } else {
    goto b2258;
  }
b2267:
  value F = Val_bool(Val_int(0) == C);
  if (Bool_val(F)) {
    goto b2271;
  } else {
    goto b2277;
  }
b2271:
  value D = caml_putc(Val_int(48));
  return e;
b2277:
  value E = caml_call(h, 1, C);
  return E;
b2258:
  value H = caml_putc(Val_int(45));
  value I = Val_int(-Int_val(C));
  value G = caml_call(h, 1, I);
  return G;
}

value c0(value *env) {

b0:
  value e = /* undefined */ Val_unit;
  value f =
      caml_alloc(2, 248, caml_copy_string("Invalid_argument"), Val_int(-4));
  value g = caml_copy_string("String.get");

  goto b218;
b218:

  goto b300;
b300:

  goto b1433;
b1433:

  goto b1753;
b1753:

  goto b1801;
b1801:

  goto b2106;
b2106:
  value P = caml_alloc_closure(c1898, 1, 0);

  value M = caml_alloc_closure(c1875, 2, 0);

  value ag = caml_alloc_closure(c1860, 2, 0);

  goto b2380;
b2380:

  goto b2060;
b2060:
  value aa = caml_alloc_closure(c2050, 1, 0);

  value ae = caml_alloc_closure(c1915, 2, 2);
  add_arg(ae, ae);
  add_arg(ae, M);

  goto b2379;
b2379:

  goto b2281;
b2281:
  value R = caml_alloc_closure(c2249, 1, 3);
  add_arg(R, e);
  add_arg(R, f);
  add_arg(R, g);

  goto b2416;
b2416:

  goto b1907;
b1907:
  value aJ = Val_int(512);

  goto b2415;
b2415:

  goto b2414;
b2414:

  goto b2412;
b2412:
  value aI = Val_int(256);

  goto b2413;
b2413:
  value ad = caml_call(ag, 2, aI, aJ);

  goto b2411;
b2411:

  goto b2409;
b2409:
  value aq = Val_int(256000);

  goto b2410;
b2410:

  goto b2408;
b2408:

  goto b1986;
b1986:
  value aw = Val_int(7);
  value ax = caml_call(aa, 1, aw);
  value ay = Val_int(7);
  value az = caml_call(ae, 2, ad, ay);
  value ar = caml_call(ag, 2, az, ax);
  value aA = Val_int(5);
  value aB = caml_call(aa, 1, aA);
  value aC = Val_int(5);
  value aD = caml_call(ae, 2, ad, aC);
  value at = caml_call(ag, 2, aD, aB);
  value aE = Val_int(3);
  value aF = caml_call(aa, 1, aE);
  value aG = Val_int(3);
  value aH = caml_call(ae, 2, ad, aG);
  value av = caml_call(ag, 2, aH, aF);

  goto b2392;
b2392:

  goto b2390;
b2390:
  value au = Val_int(Int_val(ad) - Int_val(av));

  goto b2391;
b2391:

  goto b2389;
b2389:

  goto b2387;
b2387:
  value as = Val_int(Int_val(au) + Int_val(at));

  goto b2388;
b2388:

  goto b2386;
b2386:

  goto b2385;
b2385:
  value ap = Val_int(Int_val(as) - Int_val(ar));

  goto b2407;
b2407:
  value O = caml_call(M, 2, ap, aq);

  goto b2406;
b2406:

  goto b2404;
b2404:
  value L = Val_int(256000);

  goto b2405;
b2405:

  goto b2403;
b2403:

  goto b1934;
b1934:
  value $ = Val_int(6);
  value ab = caml_call(aa, 1, $);
  value ac = Val_int(6);
  value af = caml_call(ae, 2, ad, ac);
  value W = caml_call(ag, 2, af, ab);
  value ah = Val_int(4);
  value ai = caml_call(aa, 1, ah);
  value aj = Val_int(4);
  value ak = caml_call(ae, 2, ad, aj);
  value Y = caml_call(ag, 2, ak, ai);
  value al = Val_int(2);
  value am = caml_call(aa, 1, al);
  value an = Val_int(2);
  value ao = caml_call(ae, 2, ad, an);
  value _ = caml_call(ag, 2, ao, am);

  goto b2401;
b2401:

  goto b2399;
b2399:

  goto b2400;
b2400:

  goto b2398;
b2398:

  goto b1885;
b1885:
  value Z = Val_int(Int_val(Val_int(256)) - Int_val(_));

  goto b2397;
b2397:

  goto b2396;
b2396:

  goto b1893;
b1893:
  value X = Val_int(Int_val(Z) + Int_val(Y));

  goto b2395;
b2395:

  goto b2394;
b2394:

  goto b2393;
b2393:
  value K = Val_int(Int_val(X) - Int_val(W));

  goto b2402;
b2402:
  value N = caml_call(M, 2, K, L);
  value Q = caml_call(P, 1, O);
  value S = caml_call(R, 1, Q);
  value T = caml_putc(Val_int(32));
  value U = caml_call(P, 1, N);
  value V = caml_call(R, 1, U);
  return Val_unit;
}

value c1915(value *env) {

  value ae = env[0];
  value M = env[1];
  value aK = env[2];
  value aL = env[3];

b1915:
  value aQ = Val_bool(Val_int(0) == aL);
  if (Bool_val(aQ)) {
    goto b1919;
  } else {
    goto b1923;
  }
b1919:

  goto b2384;
b2384:

  goto b2383;
b2383:
  value aM = Val_int(256);
  return aM;
b1923:
  value aO = Val_int(Int_val(aL) + Int_val(Val_int(-1)));
  value aP = caml_call(ae, 2, aK, aO);
  value aN = caml_call(M, 2, aK, aP);
  return aN;
}

value c1860(value *env) {

  value aR = env[0];
  value aS = env[1];

b1860:
  value aV = Val_int(Int_val(aS) / Int_val(Val_int(2)));
  value aW = Val_int(Int_val(aR) << Int_val(Val_int(8)));

  goto b2378;
b2378:

  goto b2376;
b2376:
  value aU = Val_int(Int_val(aW) + Int_val(aV));

  goto b2377;
b2377:
  value aT = Val_int(Int_val(aU) / Int_val(aS));
  return aT;
}

value c2050(value *env) {

  value aX = env[0];

b2050:
  value v = caml_alloc_closure(c2034, 1, 1);
  add_arg(v, v);
  value aZ = caml_call(v, 1, aX);

  goto b2382;
b2382:

  goto b2381;
b2381:
  value aY = Val_int(Int_val(aZ) << Int_val(Val_int(8)));
  return aY;
}

int main() {
  c0(NULL);
  return 0;
}