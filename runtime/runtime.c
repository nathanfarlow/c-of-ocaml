#include <setjmp.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Memory configuration */
#define HEAP_SIZE_BYTES (8 * 1024 * 1024)
#define STACK_SIZE_BYTES (128 * 1024)
#define TRAP_STACK_SIZE 64

typedef uintptr_t value;
typedef uintptr_t uintnat;
typedef intptr_t intnat;
typedef unsigned char uchar;

#define HEAP_SIZE (HEAP_SIZE_BYTES / sizeof(value))
#define STACK_SIZE (STACK_SIZE_BYTES / sizeof(value))

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

/* Block header: tag (8 bits) | mark (1 bit) | size (remaining bits) */
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

/* Heap */
static value heap[HEAP_SIZE];
static value *hp = heap;

/* Stack */
static value stack[STACK_SIZE];
value *bp = stack;
value *sp = stack;

static void check_stack(intnat n) {
  if (sp + n > stack + STACK_SIZE) {
    printf("Stack overflow (%lu bytes)\n", (unsigned long)STACK_SIZE_BYTES);
    exit(1);
  }
}

void reserve_stack(intnat n) {
  check_stack(n);
  memset(sp, 1, n * sizeof(value)); /* 1 looks like int to GC */
  sp += n;
}

/* Exception handling */
typedef struct {
  jmp_buf buf;
  value *sp;
  value *bp;
} trap_frame;

trap_frame trap_stack[TRAP_STACK_SIZE];
trap_frame *trap_sp = trap_stack;
value exn_value = 0;

static void check_trap_stack(void) {
  if (trap_sp >= trap_stack + TRAP_STACK_SIZE) {
    printf("Trap stack overflow\n");
    exit(1);
  }
}

static void caml_raise(value exn) {
  if (trap_sp == trap_stack) {
    printf("Uncaught exception\n");
    exit(2);
  }
  trap_sp--;
  exn_value = exn;
  sp = trap_sp->sp;
  bp = trap_sp->bp;
  longjmp(trap_sp->buf, 1);
}

/* Closures - declared early for GC */
typedef struct {
  value (*fun)(value *);
  uintnat args_idx;
  uintnat total_args;
  value args[];
} closure_t;

#define Closure_data(v) ((closure_t *)&Field(v, 0))

static void mark(value v) {
  value *p;
  uintnat h, size, i;
  uchar tag;
  closure_t *c;
  if (Is_int(v)) return;
  p = (value *)v;
  if (p < heap || p >= hp) return;
  h = *p;
  if (Header_marked(h)) return;
  *p = Header_set_mark(h);
  size = Header_size(h);
  tag = Header_tag(h);
  if (tag == Tag_closure) {
    c = Closure_data(v);
    for (i = 0; i < c->args_idx; i++)
      mark(c->args[i]);
  } else if (tag < Tag_no_scan) {
    for (i = 0; i < size; i++)
      mark(Field(v, i));
  }
}

static value forward(value v) {
  value *p, *src, *dst;
  uintnat h, size;
  if (Is_int(v)) return v;
  p = (value *)v;
  if (p < heap || p >= hp) return v;
  dst = heap;
  for (src = heap; src < p; ) {
    h = *src;
    size = Header_size(h);
    if (Header_marked(h))
      dst += 1 + size;
    src += 1 + size;
  }
  return (value)dst;
}

static void compact(void) {
  value *src, *dst;
  uintnat h, size, words, i;
  uchar tag;
  closure_t *c;

  /* Phase 1: Update roots */
  for (src = stack; src < sp; src++)
    *src = forward(*src);
  if (exn_value)
    exn_value = forward(exn_value);

  /* Phase 2: Update pointers in live objects */
  for (src = heap; src < hp;) {
    h = *src;
    size = Header_size(h);
    if (Header_marked(h)) {
      tag = Header_tag(h);
      if (tag == Tag_closure) {
        c = (closure_t *)&src[1];
        for (i = 0; i < c->args_idx; i++)
          c->args[i] = forward(c->args[i]);
      } else if (tag < Tag_no_scan) {
        for (i = 0; i < size; i++)
          src[1 + i] = forward(src[1 + i]);
      }
    }
    src += 1 + size;
  }

  /* Phase 3: Slide objects down */
  dst = heap;
  for (src = heap; src < hp;) {
    h = *src;
    size = Header_size(h);
    words = 1 + size;
    if (Header_marked(h)) {
      if (dst != src)
        memmove(dst, src, words * sizeof(value));
      *dst = Header_clear_mark(h);
      dst += words;
    }
    src += words;
  }
  hp = dst;
}

static void gc(void) {
  value *p;
  for (p = stack; p < sp; p++)
    mark(*p);
  if (exn_value)
    mark(exn_value);
  compact();
}

static value *alloc(uintnat size, uchar tag) {
  uintnat words = 1 + size;
  value *block;
  if (hp + words > heap + HEAP_SIZE) {
    gc();
    if (hp + words > heap + HEAP_SIZE) {
      printf("Out of heap memory (%lu bytes)\n", (unsigned long)HEAP_SIZE_BYTES);
      exit(1);
    }
  }
  block = hp;
  hp += words;
  *block = Make_header(size, tag);
  memset(block + 1, 1, size * sizeof(value)); /* 1 looks like int to GC */
  return block;
}

value caml_alloc(uchar tag, intnat size, ...) {
  va_list args;
  value *saved_sp = sp;
  value *block;
  intnat i;

  /* Push args to stack as GC roots */
  va_start(args, size);
  check_stack(size);
  for (i = 0; i < size; i++)
    *(sp++) = va_arg(args, value);
  va_end(args);

  block = alloc(size, tag);

  for (i = 0; i < size; i++)
    block[1 + i] = saved_sp[i];
  sp = saved_sp;

  return (value)block;
}

value caml_alloc_closure(value (*fun)(value *), uintnat num_args, uintnat num_env) {
  uintnat data_size = (sizeof(closure_t) + (num_args + num_env) * sizeof(value) + sizeof(value) - 1) / sizeof(value);
  value *block;
  closure_t *c;
  block = alloc(data_size, Tag_closure);
  c = (closure_t *)&block[1];
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
  closure_t *new_c;
  /* Cache before potential GC */
  value (*fun)(value *) = c->fun;
  uintnat closure_args_idx = c->args_idx;
  uintnat total_args = c->total_args;
  uintnat total_provided = closure_args_idx + num_args;
  uintnat i, excess;
  value *args_on_stack, *prev_bp;
  value result;

  check_stack(total_provided);

  args_on_stack = sp;
  for (i = 0; i < closure_args_idx; i++)
    *(sp++) = c->args[i];
  for (i = 0; i < num_args; i++)
    *(sp++) = args_array[i];

  if (total_provided >= total_args) {
    prev_bp = bp;
    bp = sp;
    result = fun(args_on_stack);
    sp = bp;
    bp = prev_bp;

    if (total_provided > total_args) {
      excess = total_provided - total_args;
      result = caml_call_with_args(result, excess, args_on_stack + total_args);
    }
  } else {
    result = caml_alloc_closure(fun, total_args - total_provided, total_provided);
    new_c = Closure_data(result);
    memcpy(new_c->args, args_on_stack, total_provided * sizeof(value));
    new_c->args_idx = total_provided;
  }

  sp = args_on_stack;
  return result;
}

value caml_call(value closure, uintnat num_args, ...) {
  va_list args;
  uintnat i;
  value *args_on_stack = sp;
  value result;

  check_stack(num_args);
  va_start(args, num_args);
  for (i = 0; i < num_args; i++)
    *(sp++) = va_arg(args, value);
  va_end(args);

  result = caml_call_with_args(closure, num_args, args_on_stack);
  sp = args_on_stack;
  return result;
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

value caml_exit(value code) { exit(Int_val(code)); }
value caml_register_global(value a, value b, value c) { (void)a; (void)b; (void)c; return Val_unit; }
value caml_ensure_stack_capacity(value n) { (void)n; return Val_unit; }

static intnat fresh_oo_id = 0;
value caml_fresh_oo_id(value unit) { (void)unit; return Val_int(fresh_oo_id++); }
