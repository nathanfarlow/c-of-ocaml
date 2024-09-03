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
value closure_1828(value *env);

value closure_2002(value *env);

value closure_1883(value *env);

value closure_1866(value *env);

value closure_1843(value *env);

value closure_0(value *env);

value closure_2018(value *env);

value closure_2217(value *env);

value closure_2190(value *env);

// free: 0, params: 2
value closure_1828(value *env) {

  value a = env[0];
  value b = env[1];

block_1828:; //
  value c = Val_int(Int_val(b) / Int_val(Val_int(2)));
  value d = Val_int(Int_val(a) << Int_val(Val_int(8)));

block_2346:; //

block_2344:; //
  value e = Val_int(Int_val(d) + Int_val(c));

block_2345:; //
  value f = Val_int(Int_val(e) / Int_val(b));
  return f;
}

// free: 1, params: 1
value closure_2002(value *env) {

  value g = env[0];
  value h = env[1];

block_2002:; //
  value i = Val_bool(Val_int(0) == h);
  if (Bool_val(i)) {
    goto block_2006;
  } else {
    goto block_2009;
  }
block_2006:; //
  value j = Val_int(1);
  return j;
block_2009:; //
  value k = Val_int(Int_val(h) + Int_val(Val_int(-1)));
  value l = caml_call(g, 1, k) /* exact */;
  value m = Val_int(Int_val(h) * Int_val(l));
  return m;
}

// free: 2, params: 2
value closure_1883(value *env) {

  value n = env[0];
  value o = env[1];
  value p = env[2];
  value q = env[3];

block_1883:; //
  value r = Val_bool(Val_int(0) == q);
  if (Bool_val(r)) {
    goto block_1887;
  } else {
    goto block_1891;
  }
block_1887:; //

block_2352:; //

block_2351:; //
  value s = Val_int(256);
  return s;
block_1891:; //
  value t = Val_int(Int_val(q) + Int_val(Val_int(-1)));
  value u = caml_call(n, 2, p, t) /* exact */;
  value v = caml_call(o, 2, p, u) /* exact */;
  return v;
}

// free: 0, params: 1
value closure_1866(value *env) {

  value w = env[0];

block_1866:; //
  value x = Val_int(Int_val(w) + Int_val(Val_int(128)));
  value y = Val_int((unsigned long)Int_val(x) >> Int_val(Val_int(8)));
  return y;
}

// free: 0, params: 2
value closure_1843(value *env) {

  value z = env[0];
  value A = env[1];

block_1843:; //
  value B = Val_int(Int_val(z) * Int_val(A));
  value C = Val_int((unsigned long)Int_val(B) >> Int_val(Val_int(8)));
  return C;
}

// free: 0, params: 0
value closure_0(value *env) {

block_0:; //
  value D = Val_unit /* aka undefined */;
  value E =
      caml_alloc(2, 248, caml_copy_string("Invalid_argument"), Val_int(-4));
  value F = caml_copy_string("String.get");

block_218:; //

block_300:; //

block_1433:; //

block_1753:; //

block_2074:; //
  value G = caml_alloc_closure(closure_1866, 1, 0);

  value o = caml_alloc_closure(closure_1843, 2, 0);

  value H = caml_alloc_closure(closure_1828, 2, 0);

block_2348:; //

block_2028:; //
  value I = caml_alloc_closure(closure_2018, 1, 0);

  value n = caml_alloc_closure(closure_1883, 2, 2);
  add_arg(n, n);
  add_arg(n, o);

block_2347:; //

block_2249:; //
  value J = caml_alloc_closure(closure_2217, 1, 3);
  add_arg(J, D);
  add_arg(J, E);
  add_arg(J, F);

block_2384:; //

block_1875:; //
  value K = Val_int(512);

block_2383:; //

block_2382:; //

block_2380:; //
  value L = Val_int(256);

block_2381:; //
  value M = caml_call(H, 2, L, K) /* exact */;

block_2379:; //

block_2377:; //
  value N = Val_int(256000);

block_2378:; //

block_2376:; //

block_1954:; //
  value O = Val_int(7);
  value P = caml_call(I, 1, O) /* exact */;
  value Q = Val_int(7);
  value R = caml_call(n, 2, M, Q) /* exact */;
  value S = caml_call(H, 2, R, P) /* exact */;
  value T = Val_int(5);
  value U = caml_call(I, 1, T) /* exact */;
  value V = Val_int(5);
  value W = caml_call(n, 2, M, V) /* exact */;
  value X = caml_call(H, 2, W, U) /* exact */;
  value Y = Val_int(3);
  value Z = caml_call(I, 1, Y) /* exact */;
  value _ = Val_int(3);
  value $ = caml_call(n, 2, M, _) /* exact */;
  value aa = caml_call(H, 2, $, Z) /* exact */;

block_2360:; //

block_2358:; //
  value ab = Val_int(Int_val(M) - Int_val(aa));

block_2359:; //

block_2357:; //

block_2355:; //
  value ac = Val_int(Int_val(ab) + Int_val(X));

block_2356:; //

block_2354:; //

block_2353:; //
  value ad = Val_int(Int_val(ac) - Int_val(S));

block_2375:; //
  value ae = caml_call(o, 2, ad, N) /* exact */;

block_2374:; //

block_2372:; //
  value af = Val_int(256000);

block_2373:; //

block_2371:; //

block_1902:; //
  value ag = Val_int(6);
  value ah = caml_call(I, 1, ag) /* exact */;
  value ai = Val_int(6);
  value aj = caml_call(n, 2, M, ai) /* exact */;
  value ak = caml_call(H, 2, aj, ah) /* exact */;
  value al = Val_int(4);
  value am = caml_call(I, 1, al) /* exact */;
  value an = Val_int(4);
  value ao = caml_call(n, 2, M, an) /* exact */;
  value ap = caml_call(H, 2, ao, am) /* exact */;
  value aq = Val_int(2);
  value ar = caml_call(I, 1, aq) /* exact */;
  value as = Val_int(2);
  value at = caml_call(n, 2, M, as) /* exact */;
  value au = caml_call(H, 2, at, ar) /* exact */;

block_2369:; //

block_2367:; //

block_2368:; //

block_2366:; //

block_1853:; //
  value av = Val_int(Int_val(Val_int(256)) - Int_val(au));

block_2365:; //

block_2364:; //

block_1861:; //
  value aw = Val_int(Int_val(av) + Int_val(ap));

block_2363:; //

block_2362:; //

block_2361:; //
  value ax = Val_int(Int_val(aw) - Int_val(ak));

block_2370:; //
  value ay = caml_call(o, 2, ax, af) /* exact */;
  value az = caml_call(G, 1, ae) /* exact */;
  value aA = caml_call(J, 1, az) /* exact */;
  value aB = caml_putc(Val_int(32));
  value aC = caml_call(G, 1, ay) /* exact */;
  value aD = caml_call(J, 1, aC) /* exact */;
  return Val_unit;
}

// free: 0, params: 1
value closure_2018(value *env) {

  value aE = env[0];

block_2018:; //
  value g = caml_alloc_closure(closure_2002, 1, 1);
  add_arg(g, g);
  value aF = caml_call(g, 1, aE) /* exact */;

block_2350:; //

block_2349:; //
  value aG = Val_int(Int_val(aF) << Int_val(Val_int(8)));
  return aG;
}

// free: 3, params: 1
value closure_2217(value *env) {

  value D = env[0];
  value E = env[1];
  value F = env[2];
  value aH = env[3];

block_2217:; //
  value aI = caml_alloc_closure(closure_2190, 1, 4);
  add_arg(aI, D);
  add_arg(aI, E);
  add_arg(aI, F);
  add_arg(aI, aI);
  value aJ = Val_bool(Int_val(Val_int(0)) <= Int_val(aH));
  if (Bool_val(aJ)) {
    goto block_2235;
  } else {
    goto block_2226;
  }
block_2235:; //
  value aK = Val_bool(Val_int(0) == aH);
  if (Bool_val(aK)) {
    goto block_2239;
  } else {
    goto block_2245;
  }
block_2239:; //
  value aL = caml_putc(Val_int(48));
  return D;
block_2245:; //
  value aM = caml_call(aI, 1, aH) /* exact */;
  return aM;
block_2226:; //
  value aN = caml_putc(Val_int(45));
  value aO = Val_int(-Int_val(aH));
  value aP = caml_call(aI, 1, aO) /* exact */;
  return aP;
}

// free: 4, params: 1
value closure_2190(value *env) {

  value D = env[0];
  value E = env[1];
  value F = env[2];
  value aI = env[3];
  value aQ = env[4];

block_2190:; //
  value aR = Val_bool(Val_int(0) == aQ);
  if (Bool_val(aR)) {
    goto block_2194;
  } else {
    goto block_2197;
  }
block_2194:; //
  return D;
block_2197:; //
  value aS = Val_int(Int_val(aQ) % Int_val(Val_int(10)));
  value aT = Val_int(Int_val(aQ) / Int_val(Val_int(10)));
  value aU = caml_call(aI, 1, aT) /* exact */;

block_2343:; //

block_1730:; //
  value aV = Val_bool(Int_val(Val_int(0)) <= Int_val(aS));
  if (Bool_val(aV)) {
    goto block_1734;
  } else {
    goto block_1741;
  }
block_1734:; //
  value aW = Val_bool(Int_val(Val_int(10)) <= Int_val(aS));
  if (Bool_val(aW)) {
    goto block_1741;
  } else {
    goto block_1748;
  }
block_1741:; //
  value aX = caml_alloc(2, 0, E, F);
  caml_raise(aX);
block_1748:; //
  value aY = caml_string_unsafe_get(caml_copy_string("0123456789"), aS);

block_2342:; //
  value aZ = caml_putc(aY);
  return D;
}

int main() {
  closure_0(NULL);
  return 0;
}
