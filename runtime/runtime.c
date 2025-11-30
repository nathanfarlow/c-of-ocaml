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
#define Tag_object 248
#define Tag_no_scan 251
#define Tag_string 252

/*
 * The offset field handles alignment: malloc may return an address with the
 * low bit set (looks like an integer), so we shift by 1 byte and remember
 * the offset for freeing.
 */
typedef struct {
  uintnat size;
  struct block *next;
  uchar tag;
  uchar marked;
  uchar offset;
  value data[];
} block;

#define Field(v, i) (((block *)(v))->data[i])
#define Tag_val(v) (((block *)(v))->tag)

typedef struct {
  value (*fun)(value *);
  uintnat args_idx;
  uintnat total_args;
  value args[];
} closure_t;

#define CLOSURE_HEADER_SIZE \
  ((sizeof(closure_t) + sizeof(value) - 1) / sizeof(value))

#define MAX_STACK_SIZE (1024 * 16)
#define MIN_HEAP_SIZE 4096

static block *root = NULL;
static value stack[MAX_STACK_SIZE];

value *bp = stack;
value *sp = stack;

static uintnat num_bytes_allocated = 0;
static uintnat max_bytes_until_gc = MIN_HEAP_SIZE;

static void check_stack(intnat n) {
  if (sp + n > stack + MAX_STACK_SIZE) {
    fprintf(stderr, "Stack overflow: need %ld, have %ld\n",
            (long)n, (long)(stack + MAX_STACK_SIZE - sp));
    exit(1);
  }
}

static void mark(value p) {
  if (Is_int(p))
    return;

  block *b = (block *)p;
  if (b->marked)
    return;

  b->marked = 1;

  if (b->tag == Tag_closure) {
    closure_t *c = (closure_t *)b->data;
    uintnat i;
    for (i = 0; i < c->args_idx; i++) {
      mark(c->args[i]);
    }
  } else if (b->tag < Tag_no_scan) {
    uintnat i;
    for (i = 0; i < b->size; i++) {
      mark(b->data[i]);
    }
  }
}

static void sweep(void) {
  block *b = root;
  block *prev = NULL;

  num_bytes_allocated = 0;

  while (b != NULL) {
    block *next = (block *)b->next;
    if (b->marked) {
      b->marked = 0;
      prev = b;
      num_bytes_allocated += sizeof(block) + b->size * sizeof(value);
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

static void gc(void) {
  value *p;
  for (p = stack; p < sp; p++) {
    mark(*p);
  }
  sweep();

  max_bytes_until_gc = num_bytes_allocated * 2;
  if (max_bytes_until_gc < MIN_HEAP_SIZE)
    max_bytes_until_gc = MIN_HEAP_SIZE;
}

static block *caml_alloc_block(uintnat size, uchar tag) {
  uintnat alloc_size = sizeof(block) + size * sizeof(value) + 1;

  if (num_bytes_allocated + alloc_size > max_bytes_until_gc) {
    gc();
  }

  num_bytes_allocated += alloc_size;
  block *b = malloc(alloc_size);

  if (b == NULL) {
    gc();
    b = malloc(alloc_size);
    if (b == NULL) {
      fprintf(stderr, "Fatal error: out of memory\n");
      exit(1);
    }
  }

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

  /* Initialize to odd values so uninitialized fields look like integers */
  memset(b->data, 1, size * sizeof(value));

  return b;
}

value caml_alloc(uchar tag, intnat size, ...) {
  block *b = caml_alloc_block(size, tag);
  va_list args;
  va_start(args, size);
  intnat i;
  for (i = 0; i < size; i++) {
    b->data[i] = va_arg(args, value);
  }
  va_end(args);
  return (value)b;
}

value caml_alloc_closure(value (*fun)(value *), uintnat num_args,
                         uintnat num_env) {
  uintnat total_slots = num_args + num_env;
  block *b = caml_alloc_block(CLOSURE_HEADER_SIZE + total_slots, Tag_closure);
  closure_t *c = (closure_t *)b->data;
  c->fun = fun;
  c->args_idx = 0;
  c->total_args = total_slots;
  return (value)b;
}

void add_arg(value closure, value arg) {
  closure_t *c = (closure_t *)(((block *)closure)->data);
  c->args[c->args_idx++] = arg;
}

static value caml_call_with_args(value closure, uintnat num_args,
                                 value *args_array) {
  closure_t *c = (closure_t *)(((block *)closure)->data);
  uintnat total_provided = c->args_idx + num_args;
  check_stack(total_provided);

  value *args_on_stack = sp;
  uintnat i;
  for (i = 0; i < c->args_idx; i++) {
    *(sp++) = c->args[i];
  }
  for (i = 0; i < num_args; i++) {
    *(sp++) = args_array[i];
  }

  value result;
  if (total_provided >= c->total_args) {
    value *prev_bp = bp;
    bp = sp;
    result = c->fun(args_on_stack);
    sp = bp;
    bp = prev_bp;

    if (total_provided > c->total_args) {
      uintnat excess_args = total_provided - c->total_args;
      result =
          caml_call_with_args(result, excess_args, args_on_stack + c->total_args);
    }
  } else {
    result =
        caml_alloc_closure(c->fun, c->total_args - total_provided, total_provided);
    closure_t *new_c = (closure_t *)(((block *)result)->data);
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
  for (i = 0; i < num_args; i++) {
    args_array[i] = va_arg(args, value);
  }
  va_end(args);

  return caml_call_with_args(closure, num_args, args_array);
}

#define Str_val(v) ((char *)&Field(v, 1))

static block *alloc_string_block(uintnat len) {
  uintnat num_values = (len + sizeof(value)) / sizeof(value) + 1;
  block *b = caml_alloc_block(num_values, Tag_string);
  b->data[0] = Val_int(len);
  return b;
}

value caml_copy_string(const char *s) {
  uintnat len = strlen(s);
  block *b = alloc_string_block(len);
  memcpy(Str_val((value)b), s, len + 1);
  return (value)b;
}

value caml_create_bytes(value value_len) {
  uintnat len = Int_val(value_len);
  block *b = alloc_string_block(len);
  Str_val((value)b)[len] = '\0';
  return (value)b;
}

value caml_ml_string_length(value s) {
  return Field(s, 0);
}

value caml_ml_bytes_length(value s) {
  return Field(s, 0);
}

value caml_string_unsafe_get(value s, value i) {
  return Val_int((uchar)Str_val(s)[Int_val(i)]);
}

value caml_bytes_unsafe_get(value s, value i) {
  return Val_int((uchar)Str_val(s)[Int_val(i)]);
}

value caml_bytes_unsafe_set(value s, value i, value c) {
  Str_val(s)[Int_val(i)] = Int_val(c);
  return Val_unit;
}

value caml_string_of_bytes(value s) {
  return s;
}

value caml_bytes_of_string(value s) {
  return s;
}

value caml_string_notequal(value s1, value s2) {
  uintnat len1 = Int_val(Field(s1, 0));
  uintnat len2 = Int_val(Field(s2, 0));
  if (len1 != len2)
    return Val_bool(1);
  return Val_bool(memcmp(Str_val(s1), Str_val(s2), len1) != 0);
}

value caml_string_concat(value s1, value s2) {
  uintnat len1 = Int_val(Field(s1, 0));
  uintnat len2 = Int_val(Field(s2, 0));
  value result = caml_create_bytes(Val_int(len1 + len2));
  memcpy(Str_val(result), Str_val(s1), len1);
  memcpy(Str_val(result) + len1, Str_val(s2), len2);
  return result;
}

value caml_blit_bytes(value src, value src_pos, value dst, value dst_pos,
                      value len) {
  uintnat src_len = Int_val(Field(src, 0));
  uintnat dst_len = Int_val(Field(dst, 0));
  uintnat sp_val = Int_val(src_pos);
  uintnat dp_val = Int_val(dst_pos);
  uintnat len_val = Int_val(len);

  if (sp_val + len_val > src_len || dp_val + len_val > dst_len) {
    fprintf(stderr, "Fatal error: bytes blit out of bounds\n");
    exit(1);
  }

  memcpy(Str_val(dst) + dp_val, Str_val(src) + sp_val, len_val);
  return Val_unit;
}

value caml_putc(value c) {
  putchar(Int_val(c));
  return Val_unit;
}

value caml_getc(value unit) {
  (void)unit;
  return Val_int(getchar());
}

value caml_int_compare(value a, value b) {
  intnat x = Int_val(a);
  intnat y = Int_val(b);
  if (x < y)
    return Val_int(-1);
  if (x > y)
    return Val_int(1);
  return Val_int(0);
}

#define caml_raise(v) exit(1)

value caml_exit(value code) {
  exit(Int_val(code));
}

value caml_register_global(value a, value b, value c) {
  (void)a;
  (void)b;
  (void)c;
  return Val_unit;
}

value caml_ensure_stack_capacity(value n) {
  (void)n;
  return Val_unit;
}
