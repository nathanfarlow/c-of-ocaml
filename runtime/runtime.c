#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef uintptr_t value;
typedef uintptr_t unatint;
typedef uintptr_t uintnat;
typedef intptr_t natint;

#define Is_int(v) (((v) & 1) != 0)
#define Is_block(v) (((v) & 1) == 0)

#define Val_int(x) (((value)(x) << 1) | 1)
#define Int_val(v) ((natint)(v) >> 1)

/* Use 247 to avoid conflict with OCaml variant tags (0-245) */
#define Tag_closure 247
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

#define MAX_STACK_SIZE (1024 * 16)
value stack[MAX_STACK_SIZE];
value *bp = stack;
value *sp = stack;

unatint num_bytes_allocated = 0;
unatint max_bytes_until_gc = 4096;

typedef struct {
  value (*fun)(value *);
  unatint args_idx;
  unatint total_args;
  value args[];
} closure_t;

void check_stack(const char *where) {
  if (sp > stack + MAX_STACK_SIZE) {
    fprintf(stderr, "Stack overflow in %s: sp=%ld, max=%d\n", where,
            (long)(sp - stack), MAX_STACK_SIZE);
    exit(1);
  }
}

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
  if (max_bytes_until_gc < 4096)
    max_bytes_until_gc = 4096;
}

block *caml_alloc_block(unatint size, uchar tag) {

  unatint wanted_bytes = sizeof(block) + size * sizeof(value) + 10;
  unatint aligned_size = (wanted_bytes + 1);

  if (num_bytes_allocated + aligned_size > max_bytes_until_gc) {
    gc();
  }

  num_bytes_allocated += aligned_size;
  block *b = malloc(aligned_size);

  if (b == NULL) {
    gc();
    b = malloc(aligned_size);
    if (b == NULL) {
      fprintf(stderr, "Fatal error: out of memory :(\n");
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

/* Args are pushed to global stack so GC can trace them */
static value caml_call_with_args(value closure, unatint num_args,
                                 value *args_array) {
  closure_t *c = (closure_t *)(((block *)closure)->data);
  unatint total_provided = c->args_idx + num_args;

  value *args_on_stack = sp;
  unatint i;
  for (i = 0; i < c->args_idx; i++) {
    *(sp++) = c->args[i];
  }
  for (i = 0; i < num_args; i++) {
    *(sp++) = args_array[i];
  }
  check_stack("caml_call_with_args");

  value result;
  if (total_provided >= c->total_args) {
    value *prev_bp = bp;
    bp = sp;
    result = c->fun(args_on_stack);
    sp = bp;
    bp = prev_bp;

    if (total_provided > c->total_args) {
      unatint excess_args = total_provided - c->total_args;
      result = caml_call_with_args(result, excess_args,
                                   args_on_stack + c->total_args);
    }
  } else {
    result = caml_alloc_closure(c->fun, c->total_args - total_provided,
                                total_provided);
    closure_t *new_c = (closure_t *)(((block *)result)->data);
    memcpy(new_c->args, args_on_stack, total_provided * sizeof(value));
    new_c->args_idx = total_provided;
  }

  sp = args_on_stack;
  return result;
}

value caml_call(value closure, unatint num_args, ...) {
  value args_array[num_args];
  va_list args;
  va_start(args, num_args);
  unatint i;
  for (i = 0; i < num_args; i++) {
    args_array[i] = va_arg(args, value);
  }
  va_end(args);

  return caml_call_with_args(closure, num_args, args_array);
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
  /* Read varargs onto the global stack first, so GC can trace them */
  value *saved_sp = sp;
  va_list args;
  va_start(args, size);
  natint i;
  for (i = 0; i < size; i++) {
    *(sp++) = va_arg(args, value);
  }
  va_end(args);
  check_stack("caml_alloc");

  /* Now allocate (GC may run here, but args are on our stack) */
  block *b = caml_alloc_block(size, tag);

  for (i = 0; i < size; i++) {
    b->data[i] = saved_sp[i];
  }

  sp = saved_sp;
  return (value)b;
}

#define Field(v, i) (((block *)(v))->data[i])
#define Str_val(v) ((char *)&Field(v, 1))
#define Tag_val(v) (((block *)(v))->tag)

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

value caml_bytes_unsafe_get(value s, value i) {
  char c = Str_val(s)[Int_val(i)];
  return Val_int(c);
}

value caml_string_of_bytes(value s) { return s; }

value caml_string_notequal(value s1, value s2) {
  unatint len1 = Int_val(Field(s1, 0));
  unatint len2 = Int_val(Field(s2, 0));
  if (len1 != len2)
    return Val_bool(1);
  return Val_bool(memcmp(Str_val(s1), Str_val(s2), len1) != 0);
}

value caml_string_concat(value s1, value s2) {
  /* Push s1, s2 onto stack so GC can trace them during allocation */
  value *saved_sp = sp;
  *(sp++) = s1;
  *(sp++) = s2;

  unatint len1 = Int_val(Field(s1, 0));
  unatint len2 = Int_val(Field(s2, 0));
  unatint total_len = len1 + len2;
  value new_string = caml_create_bytes(Val_int(total_len));

  /* Re-read s1, s2 from stack in case they moved (they won't with our GC, but
   * good practice) */
  s1 = saved_sp[0];
  s2 = saved_sp[1];
  memcpy(Str_val(new_string), Str_val(s1), len1);
  memcpy(Str_val(new_string) + len1, Str_val(s2), len2);

  sp = saved_sp;
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

value caml_int_compare(value a, value b) {
  natint x = Int_val(a);
  natint y = Int_val(b);
  if (x < y)
    return Val_int(-1);
  if (x > y)
    return Val_int(1);
  return Val_int(0);
}

value caml_exit(value code) { exit(Int_val(code)); }

/* TODO: Implement these */
value caml_register_global(value a, value b, value c) { return Val_unit; }
value caml_ensure_stack_capacity(value) { return Val_unit; }
