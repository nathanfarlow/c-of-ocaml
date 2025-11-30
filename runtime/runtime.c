#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uintptr_t value;
typedef uintptr_t uintnat;
typedef intptr_t intnat;
typedef unsigned char uchar;

#define Is_int(v) (((v) & 1) != 0)
#define Is_block(v) (((v) & 1) == 0)

#define Val_int(x) (((value)(x) << 1) | 1)
#define Int_val(v) ((intnat)(v) >> 1)

#define Val_bool(x) ((x) ? Val_int(1) : Val_int(0))
#define Bool_val(v) (Int_val(v) != 0)

#define Val_unit Val_int(0)

#define Tag_closure 247
#define Tag_no_scan 251
#define Tag_string 252

/* Block header: tag (8 bits) | mark (1 bit) | size (55 bits) */
#define Make_header(sz, tag) ((uintnat)(tag) | ((uintnat)(sz) << 9))
#define Header_tag(h) ((h) & 0xFF)
#define Header_size(h) ((h) >> 9)
#define Header_marked(h) (((h) >> 8) & 1)
#define Header_set_mark(h) ((h) | (1 << 8))
#define Header_clear_mark(h) ((h) & ~(uintnat)(1 << 8))

#define Field(v, i) (((value *)(v))[1 + (i)])
#define Header(v) (((value *)(v))[0])
#define Tag_val(v) Header_tag(Header(v))
#define Size_val(v) Header_size(Header(v))

/* Heap: fixed-size bump allocator */
#define HEAP_SIZE (1024 * 1024)
static value heap[HEAP_SIZE];
static value *hp = heap;

/* Stack */
#define STACK_SIZE (1024 * 16)
static value stack[STACK_SIZE];
value *bp = stack;
value *sp = stack;

static void check_stack(intnat n) {
  if (sp + n > stack + STACK_SIZE) {
    printf("Stack overflow\n");
    exit(1);
  }
}

/* Mark phase */
static void mark(value v) {
  if (Is_int(v)) return;

  value *p = (value *)v;
  if (p < heap || p >= hp) return;

  uintnat h = *p;
  if (Header_marked(h)) return;

  Header(v) = Header_set_mark(h);
  uintnat size = Header_size(h);
  uchar tag = Header_tag(h);

  if (tag < Tag_no_scan) {
    uintnat i;
    for (i = 0; i < size; i++)
      mark(Field(v, i));
  }
}

/* Compute where an object will move to after compaction */
static value *forward_addr(value *obj) {
  value *dst = heap;
  value *src = heap;
  while (src < obj) {
    uintnat h = *src;
    uintnat size = Header_size(h);
    if (Header_marked(h))
      dst += 1 + size;
    src += 1 + size;
  }
  return dst;
}

/* Update a single pointer */
static value forward(value v) {
  if (Is_int(v)) return v;
  return (value)forward_addr((value *)v);
}

/* Compact phase: update pointers then slide objects */
static void compact(void) {
  value *src, *dst;
  uintnat i;

  /* Update roots */
  for (src = stack; src < sp; src++)
    *src = forward(*src);

  /* Update pointers in live objects */
  for (src = heap; src < hp; ) {
    uintnat h = *src;
    uintnat size = Header_size(h);
    if (Header_marked(h)) {
      uchar tag = Header_tag(h);
      if (tag < Tag_no_scan) {
        for (i = 0; i < size; i++)
          src[1 + i] = forward(src[1 + i]);
      }
    }
    src += 1 + size;
  }

  /* Slide objects down */
  dst = heap;
  for (src = heap; src < hp; ) {
    uintnat h = *src;
    uintnat size = Header_size(h);
    if (Header_marked(h)) {
      uintnat words = 1 + size;
      if (dst != src)
        memmove(dst, src, words * sizeof(value));
      *dst = Header_clear_mark(h);
      dst += words;
    }
    src += 1 + size;
  }
  hp = dst;
}

static void gc(void) {
  value *p;
  for (p = stack; p < sp; p++)
    mark(*p);
  compact();
}

static value *alloc(uintnat size, uchar tag) {
  uintnat words = 1 + size;
  if (hp + words > heap + HEAP_SIZE) {
    gc();
    if (hp + words > heap + HEAP_SIZE) {
      printf("Out of memory\n");
      exit(1);
    }
  }
  value *block = hp;
  hp += words;
  *block = Make_header(size, tag);
  return block;
}

value caml_alloc(uchar tag, intnat size, ...) {
  value *block = alloc(size, tag);
  va_list args;
  va_start(args, size);
  intnat i;
  for (i = 0; i < size; i++)
    block[1 + i] = va_arg(args, value);
  va_end(args);
  return (value)block;
}

/* Closures */
typedef struct {
  value (*fun)(value *);
  uintnat args_idx;
  uintnat total_args;
  value args[];
} closure_t;

#define Closure_data(v) ((closure_t *)&Field(v, 0))

value caml_alloc_closure(value (*fun)(value *), uintnat num_args, uintnat num_env) {
  uintnat data_size = (sizeof(closure_t) + (num_args + num_env) * sizeof(value) + sizeof(value) - 1) / sizeof(value);
  value *block = alloc(data_size, Tag_closure);
  closure_t *c = (closure_t *)&block[1];
  c->fun = fun;
  c->args_idx = 0;
  c->total_args = num_args + num_env;
  return (value)block;
}

void add_arg(value closure, value arg) {
  closure_t *c = Closure_data(closure);
  c->args[c->args_idx++] = arg;
}

static value caml_call_with_args(value closure, uintnat num_args, value *args_array) {
  closure_t *c = Closure_data(closure);
  uintnat total_provided = c->args_idx + num_args;
  check_stack(total_provided);

  value *args_on_stack = sp;
  uintnat i;
  for (i = 0; i < c->args_idx; i++)
    *(sp++) = c->args[i];
  for (i = 0; i < num_args; i++)
    *(sp++) = args_array[i];

  value result;
  if (total_provided >= c->total_args) {
    value *prev_bp = bp;
    bp = sp;
    result = c->fun(args_on_stack);
    sp = bp;
    bp = prev_bp;

    if (total_provided > c->total_args) {
      uintnat excess = total_provided - c->total_args;
      result = caml_call_with_args(result, excess, args_on_stack + c->total_args);
    }
  } else {
    result = caml_alloc_closure(c->fun, c->total_args - total_provided, total_provided);
    closure_t *new_c = Closure_data(result);
    memcpy(new_c->args, args_on_stack, total_provided * sizeof(value));
    new_c->args_idx = total_provided;
  }

  sp = args_on_stack;
  return result;
}

value caml_call(value closure, uintnat num_args, ...) {
  value args_array[num_args];
  va_list args;
  va_start(args, num_args);
  uintnat i;
  for (i = 0; i < num_args; i++)
    args_array[i] = va_arg(args, value);
  va_end(args);
  return caml_call_with_args(closure, num_args, args_array);
}

/* Strings */
#define String_length(v) Int_val(Field(v, 0))
#define String_data(v) ((char *)&Field(v, 1))

static value alloc_string(uintnat len) {
  uintnat data_words = (len + sizeof(value)) / sizeof(value);
  value *block = alloc(1 + data_words, Tag_string);
  block[1] = Val_int(len);
  return (value)block;
}

value caml_copy_string(const char *s) {
  uintnat len = strlen(s);
  value v = alloc_string(len);
  memcpy(String_data(v), s, len + 1);
  return v;
}

value caml_create_bytes(value len) {
  uintnat n = Int_val(len);
  value v = alloc_string(n);
  String_data(v)[n] = '\0';
  return v;
}

value caml_ml_string_length(value s) { return Field(s, 0); }
value caml_ml_bytes_length(value s) { return Field(s, 0); }

value caml_string_unsafe_get(value s, value i) {
  return Val_int((uchar)String_data(s)[Int_val(i)]);
}

value caml_bytes_unsafe_get(value s, value i) {
  return Val_int((uchar)String_data(s)[Int_val(i)]);
}

value caml_bytes_unsafe_set(value s, value i, value c) {
  String_data(s)[Int_val(i)] = Int_val(c);
  return Val_unit;
}

value caml_string_of_bytes(value s) { return s; }
value caml_bytes_of_string(value s) { return s; }

value caml_string_notequal(value s1, value s2) {
  uintnat len1 = String_length(s1);
  uintnat len2 = String_length(s2);
  if (len1 != len2) return Val_bool(1);
  return Val_bool(memcmp(String_data(s1), String_data(s2), len1) != 0);
}

value caml_string_concat(value s1, value s2) {
  uintnat len1 = String_length(s1);
  uintnat len2 = String_length(s2);
  value result = caml_create_bytes(Val_int(len1 + len2));
  memcpy(String_data(result), String_data(s1), len1);
  memcpy(String_data(result) + len1, String_data(s2), len2);
  return result;
}

value caml_blit_bytes(value src, value src_pos, value dst, value dst_pos, value len) {
  memcpy(String_data(dst) + Int_val(dst_pos),
         String_data(src) + Int_val(src_pos),
         Int_val(len));
  return Val_unit;
}

/* I/O */
value caml_putc(value c) {
  putchar(Int_val(c));
  return Val_unit;
}

value caml_getc(value unit) {
  (void)unit;
  return Val_int(getchar());
}

/* Misc */
value caml_int_compare(value a, value b) {
  intnat x = Int_val(a), y = Int_val(b);
  return Val_int((x > y) - (x < y));
}

#define caml_raise(v) exit(1)

value caml_exit(value code) { exit(Int_val(code)); }
value caml_register_global(value a, value b, value c) { (void)a; (void)b; (void)c; return Val_unit; }
value caml_ensure_stack_capacity(value n) { (void)n; return Val_unit; }
