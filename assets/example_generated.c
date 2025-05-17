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

#define Tag_closure 1
#define Tag_string 252
#define Tag_object 248
#define Tag_no_scan 251

#define Val_unit Val_int(0)

typedef unsigned char uchar;

typedef struct {
  unatint size;
  struct block *next;
  /* TODO: coalesce */
  uchar tag;
  uchar marked;
  uchar offset;
  value data[];
} block;

uchar block_gc = 0;
block *root;

#define MAX_STACK_SIZE 1024
value stack[MAX_STACK_SIZE];
value *bp = stack;
value *sp = stack;

unatint num_bytes_allocated = 0;
unatint max_bytes_until_gc = 680;

typedef struct {
  value (*fun)(value *);
  unatint args_idx;
  unatint total_args;
  value args[];
} closure_t;

void mark(value p) {
  if (Is_block(p)) {

    block *b = (block *)p;

    if (b->marked) {
      return;
    }

    b->marked = 1;

    if (b->tag == Tag_closure) {
      closure_t *c = (closure_t *)b->data;
      unatint i;
      for (i = 0; i < c->args_idx; i++) {
        mark(c->args[i]);
      }
    } else if (b->tag == Tag_object) {
    } else if (b->tag < Tag_no_scan) {
      unatint i;
      for (i = 0; i < b->size; i++) {
        mark(b->data[i]);
      }
    }
  }
}

void sweep() {
  block *b = root;
  block *prev = NULL;

  num_bytes_allocated = 0;

  while (b != NULL) {
    block *next = (block *)b->next;
    if (b->marked) {
      b->marked = 0;
      prev = b;
      num_bytes_allocated += sizeof(block) + b->size * sizeof(value) + 1;
    } else {
      if (prev == NULL) {
        root = next;
      } else {
        prev->next = (struct block *)next;
      }

      free((void *)(((uchar *)b) - b->offset));
    }
    b = next;
  }
}

void gc() {

  if (block_gc) {
    return;
  }

  value *p;
  for (p = stack; p < sp; p++) {
    mark(*p);
  }

  sweep();
  max_bytes_until_gc = num_bytes_allocated * 2;
}

block *caml_alloc_block(unatint size, uchar tag) {

  unatint wanted_bytes = sizeof(block) + size * sizeof(value) + 10;
  unatint aligned_size = (wanted_bytes + 1);

  if (num_bytes_allocated + aligned_size > max_bytes_until_gc) {
    gc();
  }

  num_bytes_allocated += aligned_size;
  block *b = malloc(aligned_size);

  if (Is_int((value)b)) {
    b = (block *)(((uchar *)b) + 1);
    b->offset = 1;
  } else {
    b->offset = 0;
  }

  b->size = size;
  b->tag = tag;
  b->marked = 0;
  b->next = (struct block *)root;
  root = b;

  memset(b->data, 1, size * sizeof(value));

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
  unatint i;
  for (i = 0; i < num_args; i++) {
    new_args[c->args_idx + i] = va_arg(args, value);
  }
  va_end(args);

  value result;
  if (total_provided >= c->total_args) {
    value *prev_bp = bp;
    bp = sp;
    result = c->fun(new_args);
    sp = bp;
    bp = prev_bp;
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

value caml_alloc(uchar tag, natint size, ...) {
  block *b = caml_alloc_block(size, tag);
  va_list args;
  va_start(args, size);
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

#define caml_raise(v) exit(-1)

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

/* TODO: Implement these */
value caml_register_global(value a, value b, value c) { return Val_unit; }
value caml_ensure_stack_capacity(value) { return Val_unit; }
value c2479(value* env);
value c174(value* env);
value c2383(value* env);
value c0(value* env);
value s_176434974;
value s_294329037;
value s_949821258;
value s_680650890;
value s_252069304;
value s_834731544;
value s_671588152;
value s_186459799;
value s_154408390;
value s_208627236;
value s_656651812;
value s_756918818;
value s_160631202;
value s_114338481;
value s_979516401;
value s_0;
value c2479(value* env)
{
memset(bp, 1, 7 * sizeof(value));
sp += 7;
value a = env[0];

b2479:;

goto b2485;
b2485:;

goto b1953;
b1953:;
bp[2] = caml_create_bytes(Val_int(1));
bp[1] = Val_int(0);
bp[4] = bp[1];

goto b1965;
b1965:;
bp[0] = caml_bytes_unsafe_set(bp[2], bp[4], a);
bp[3] = Val_int(Int_val(bp[4]) + Int_val(Val_int(1)));
bp[5] = Val_bool(Val_int(0) != bp[4]);
if (Bool_val(bp[5])) { bp[4] = bp[3];

goto b1965; } else { 
goto b1980; }

b1980:;
bp[6] = caml_string_of_bytes(bp[2]);
return bp[6];
}
value c174(value* env)
{
memset(bp, 1, 5 * sizeof(value));
sp += 5;
value b = env[0];
value c = env[1];

b174:;
bp[4] = caml_ml_string_length(b);
bp[3] = caml_ml_string_length(c);
bp[2] = Val_int(Int_val(bp[4]) + Int_val(bp[3]));
bp[1] = caml_string_concat(b, c);
bp[0] = caml_bytes_of_string(bp[1]);
return bp[1];
}
value c2383(value* env)
{
memset(bp, 1, 7 * sizeof(value));
sp += 7;
value d = env[0];
value e = env[1];

b2383:;
bp[6] = Val_bool(Int_val(Val_int(2)) <= Int_val(e));
if (Bool_val(bp[6])) { 
goto b2390; } else { 
goto b2387; }
b2390:;
bp[0] = Val_int(Int_val(e) + Int_val(Val_int(-2)));
bp[3] = caml_call(d, 1, bp[0]);
bp[1] = Val_int(Int_val(e) + Int_val(Val_int(-1)));
bp[5] = caml_call(d, 1, bp[1]);
bp[4] = Val_int(Int_val(bp[5]) + Int_val(bp[3]));
return bp[4];
b2387:;
bp[2] = Val_int(1);
return bp[2];
}
value c0(value* env)
{
memset(bp, 1, 116 * sizeof(value));
sp += 116;


b0:;
block_gc = 1;
bp[55] = caml_alloc(248, 2, s_834731544, Val_int(-1));
block_gc = 0;
block_gc = 1;
bp[102] = caml_alloc(248, 2, s_114338481, Val_int(-2));
block_gc = 0;
block_gc = 1;
bp[66] = caml_alloc(248, 2, s_176434974, Val_int(-3));
block_gc = 0;
block_gc = 1;
bp[95] = caml_alloc(248, 2, s_656651812, Val_int(-4));
block_gc = 0;
block_gc = 1;
bp[48] = caml_alloc(248, 2, s_756918818, Val_int(-5));
block_gc = 0;
block_gc = 1;
bp[63] = caml_alloc(248, 2, s_154408390, Val_int(-6));
block_gc = 0;
block_gc = 1;
bp[52] = caml_alloc(248, 2, s_294329037, Val_int(-7));
block_gc = 0;
block_gc = 1;
bp[69] = caml_alloc(248, 2, s_979516401, Val_int(-8));
block_gc = 0;
block_gc = 1;
bp[115] = caml_alloc(248, 2, s_208627236, Val_int(-9));
block_gc = 0;
block_gc = 1;
bp[49] = caml_alloc(248, 2, s_186459799, Val_int(-10));
block_gc = 0;
block_gc = 1;
bp[90] = caml_alloc(248, 2, s_671588152, Val_int(-11));
block_gc = 0;
block_gc = 1;
bp[60] = caml_alloc(248, 2, s_252069304, Val_int(-12));
block_gc = 0;
bp[39] = s_949821258;
bp[13] = s_0;
bp[45] = s_680650890;
bp[84] = s_160631202;
bp[56] = s_0;
block_gc = 1;
bp[22] = caml_alloc(0, 1, s_0);
block_gc = 0;
bp[38] = caml_register_global(Val_int(11), bp[60], s_252069304);
bp[58] = caml_register_global(Val_int(10), bp[90], s_671588152);
bp[82] = caml_register_global(Val_int(9), bp[49], s_186459799);
bp[83] = caml_register_global(Val_int(8), bp[115], s_208627236);
bp[111] = caml_register_global(Val_int(7), bp[69], s_979516401);
bp[0] = caml_register_global(Val_int(6), bp[52], s_294329037);
bp[46] = caml_register_global(Val_int(5), bp[63], s_154408390);
bp[26] = caml_register_global(Val_int(4), bp[48], s_756918818);
bp[44] = caml_register_global(Val_int(3), bp[95], s_656651812);
bp[101] = caml_register_global(Val_int(2), bp[66], s_176434974);
bp[50] = caml_register_global(Val_int(1), bp[102], s_114338481);
bp[5] = caml_register_global(Val_int(0), bp[55], s_834731544);

goto b234;
b234:;
bp[109] = caml_alloc_closure(c174, 2, 0);;


goto b328;
b328:;

goto b547;
b547:;

goto b1773;
b1773:;
bp[94] = caml_ensure_stack_capacity(Val_int(61));

goto b2106;
b2106:;

goto b2281;
b2281:;

goto b2367;
b2367:;

goto b2403;
b2403:;
bp[43] = caml_alloc_closure(c2383, 1, 1);;
add_arg(bp[43], bp[43]);

goto b2426;
b2426:;
bp[92] = Val_int(0);
bp[107] = caml_alloc(0, 1, bp[92]);
bp[67] = Val_int(0);
bp[106] = bp[67];

goto b2440;
b2440:;
bp[57] = Field(bp[107], 0);
bp[53] = caml_call(bp[43], 1, bp[57]);

goto b2507;
b2507:;

goto b2416;
b2416:;

goto b2482;
b2482:;

goto b2178;
b2178:;
bp[27] = Val_bool(Val_int(0) == bp[53]);
if (Bool_val(bp[27])) { 
goto b2187; } else { 
goto b2191; }
b2187:;
bp[104] = bp[45];

goto b2481;
b2481:;

goto b2480;
b2480:;

goto b2353;
b2353:;

goto b2484;
b2484:;

goto b2049;
b2049:;
bp[105] = caml_ml_string_length(bp[104]);
bp[12] = Val_int(0);
bp[19] = Val_int(Int_val(bp[105]) + Int_val(Val_int(-1)));
bp[11] = Val_bool(Int_val(bp[19]) < Int_val(Val_int(0)));
if (Bool_val(bp[11])) { 
goto b2077; } else { bp[41] = bp[12];

goto b2061; }
b2077:;

goto b2483;
b2483:;
bp[31] = caml_putc(Val_int(10));

goto b2506;
b2506:;

goto b2505;
b2505:;

goto b163;
b163:;
bp[59] = Field(bp[107], 0);
bp[62] = Val_int(Int_val(bp[59]) + Int_val(Val_int(1)));
Field(bp[107], 0) = bp[62];

goto b2504;
b2504:;
bp[4] = Val_int(Int_val(bp[106]) + Int_val(Val_int(1)));
bp[97] = Val_bool(Val_int(200) != bp[106]);
if (Bool_val(bp[97])) { bp[106] = bp[4];

goto b2440; } else { 
goto b2464; }

b2464:;
return Val_unit;
b2061:;
bp[24] = caml_string_unsafe_get(bp[104], bp[41]);

goto b2509;
b2509:;

goto b2348;
b2348:;
bp[88] = caml_putc(bp[24]);

goto b2508;
b2508:;
bp[14] = Val_int(Int_val(bp[41]) + Int_val(Val_int(1)));
bp[2] = Val_bool(bp[19] != bp[41]);
if (Bool_val(bp[2])) { bp[41] = bp[14];

goto b2061; } else { 
goto b2077; }


b2191:;
bp[35] = Val_bool(Int_val(Val_int(0)) <= Int_val(bp[53]));
if (Bool_val(bp[35])) { 
goto b2199; } else { 
goto b2195; }
b2199:;
bp[29] = bp[56];

goto b2201;
b2201:;
bp[23] = Val_int(0);

goto b2497;
b2497:;
bp[85] = bp[23];
bp[108] = bp[53];

goto b2147;
b2147:;
bp[71] = Val_bool(Val_int(0) == bp[108]);
if (Bool_val(bp[71])) { 
goto b2151; } else { 
goto b2154; }
b2151:;

goto b2496;
b2496:;
bp[68] = caml_alloc_closure(c2479, 1, 0);;


goto b2495;
b2495:;

goto b2478;
b2478:;

goto b2486;
b2486:;

goto b771;
b771:;
if (Bool_val(bp[85])) { 
goto b774; } else { 
goto b825; }
b774:;
bp[21] = Field(bp[85], 1);
bp[34] = Field(bp[85], 0);
if (Bool_val(bp[21])) { 
goto b781; } else { 
goto b816; }
b781:;
bp[103] = Field(bp[21], 1);
bp[81] = Field(bp[21], 0);
bp[70] = caml_call(bp[68], 1, bp[34]);
bp[47] = caml_call(bp[68], 1, bp[81]);
bp[73] = Val_int(24029);
bp[75] = caml_alloc(0, 2, bp[47], bp[73]);
bp[15] = Val_int(1);

goto b2501;
b2501:;
bp[113] = bp[103];
bp[9] = bp[15];
bp[80] = bp[75];

goto b831;
b831:;
if (Bool_val(bp[113])) { 
goto b834; } else { 
goto b889; }
b834:;
bp[51] = Field(bp[113], 1);
bp[87] = Field(bp[113], 0);
if (Bool_val(bp[51])) { 
goto b841; } else { 
goto b877; }
b841:;
bp[74] = Field(bp[51], 1);
bp[28] = Field(bp[51], 0);
bp[96] = caml_call(bp[68], 1, bp[87]);
bp[93] = caml_call(bp[68], 1, bp[28]);
bp[64] = Val_int(24029);
bp[6] = caml_alloc(0, 2, bp[93], bp[64]);
bp[42] = caml_alloc(0, 2, bp[96], bp[6]);
Field(bp[80], Int_val(bp[9])) = bp[42];
bp[33] = Val_int(1);
bp[113] = bp[74];
bp[9] = bp[33];
bp[80] = bp[6];

goto b831;

b877:;
bp[89] = caml_call(bp[68], 1, bp[87]);
bp[30] = Val_int(0);
bp[114] = caml_alloc(0, 2, bp[89], bp[30]);
Field(bp[80], Int_val(bp[9])) = bp[114];

goto b2500;
b2500:;
bp[77] = caml_alloc(0, 2, bp[70], bp[75]);
bp[25] = bp[77];

goto b2494;
b2494:;

goto b2493;
b2493:;

goto b2477;
b2477:;

goto b2487;
b2487:;

goto b2006;
b2006:;

goto b2009;
b2009:;
bp[78] = Field(bp[22], 0);

goto b2015;
b2015:;
if (Bool_val(bp[25])) { 
goto b2018; } else { 
goto b2042; }
b2018:;
bp[98] = Field(bp[25], 0);
bp[61] = Field(bp[25], 1);
if (Bool_val(bp[61])) { 
goto b2025; } else { 
goto b2039; }
b2025:;
bp[72] = Field(bp[25], 1);

goto b2499;
b2499:;
bp[8] = bp[72];
bp[110] = bp[98];

goto b1093;
b1093:;
if (Bool_val(bp[8])) { 
goto b1096; } else { 
goto b1109; }
b1096:;
bp[76] = Field(bp[8], 1);
bp[36] = Field(bp[8], 0);

goto b2511;
b2511:;

goto b1991;
b1991:;
bp[40] = caml_call(bp[109], 2, bp[78], bp[36]);
bp[79] = caml_call(bp[109], 2, bp[110], bp[40]);

goto b2510;
b2510:;
bp[8] = bp[76];
bp[110] = bp[79];

goto b1093;

b1109:;

goto b2498;
b2498:;
bp[16] = bp[110];

goto b2492;
b2492:;
bp[10] = caml_call(bp[109], 2, bp[29], bp[16]);
bp[104] = bp[10];

goto b2481;

b2039:;
bp[16] = bp[98];

goto b2492;

b2042:;
bp[16] = bp[13];

goto b2492;

b889:;
bp[112] = Val_int(0);
Field(bp[80], Int_val(bp[9])) = bp[112];

goto b2500;

b816:;
bp[18] = caml_call(bp[68], 1, bp[34]);
bp[3] = Val_int(0);
bp[17] = caml_alloc(0, 2, bp[18], bp[3]);
bp[25] = bp[17];

goto b2494;

b825:;
bp[99] = Val_int(0);
bp[25] = bp[99];

goto b2494;

b2154:;
bp[37] = Val_int(Int_val(bp[108]) % Int_val(Val_int(10)));

goto b2491;
b2491:;

goto b2270;
b2270:;
bp[20] = Val_bool(Int_val(Val_int(0)) <= Int_val(bp[37]));
if (Bool_val(bp[20])) { 
goto b2274; } else { 
goto b2277; }
b2274:;
bp[54] = bp[37];

goto b2490;
b2490:;
bp[1] = Val_int(Int_val(bp[54]) + Int_val(Val_int(48)));

goto b2489;
b2489:;

goto b350;
b350:;
bp[100] = Val_bool(Int_val(Val_int(0)) <= Int_val(bp[1]));
if (Bool_val(bp[100])) { 
goto b354; } else { 
goto b358; }
b354:;
bp[65] = Val_bool(Int_val(Val_int(255)) < Int_val(bp[1]));
if (Bool_val(bp[65])) { 
goto b358; } else { 
goto b365; }
b358:;

goto b2503;
b2503:;

goto b222;
b222:;
bp[7] = caml_alloc(0, 2, bp[95], bp[39]);
caml_raise(bp[7]);
b365:;

goto b2488;
b2488:;
bp[32] = caml_alloc(0, 2, bp[1], bp[85]);
bp[86] = Val_int(Int_val(bp[108]) / Int_val(Val_int(10)));
bp[85] = bp[32];
bp[108] = bp[86];

goto b2147;


b2277:;
bp[91] = Val_int(-Int_val(bp[37]));
bp[54] = bp[91];

goto b2490;

b2195:;
bp[29] = bp[84];

goto b2201;

}
int main() {
s_176434974 = caml_copy_string("Failure");
*(sp++) = s_176434974;
s_294329037 = caml_copy_string("Not_found");
*(sp++) = s_294329037;
s_949821258 = caml_copy_string("Char.of_int");
*(sp++) = s_949821258;
s_680650890 = caml_copy_string("0");
*(sp++) = s_680650890;
s_252069304 = caml_copy_string("Undefined_recursive_module");
*(sp++) = s_252069304;
s_834731544 = caml_copy_string("Out_of_memory");
*(sp++) = s_834731544;
s_671588152 = caml_copy_string("Assert_failure");
*(sp++) = s_671588152;
s_186459799 = caml_copy_string("Sys_blocked_io");
*(sp++) = s_186459799;
s_154408390 = caml_copy_string("Division_by_zero");
*(sp++) = s_154408390;
s_208627236 = caml_copy_string("Stack_overflow");
*(sp++) = s_208627236;
s_656651812 = caml_copy_string("Invalid_argument");
*(sp++) = s_656651812;
s_756918818 = caml_copy_string("End_of_file");
*(sp++) = s_756918818;
s_160631202 = caml_copy_string("-");
*(sp++) = s_160631202;
s_114338481 = caml_copy_string("Sys_error");
*(sp++) = s_114338481;
s_979516401 = caml_copy_string("Match_failure");
*(sp++) = s_979516401;
s_0 = caml_copy_string("");
*(sp++) = s_0;
bp = sp; c0(NULL); return 0;}