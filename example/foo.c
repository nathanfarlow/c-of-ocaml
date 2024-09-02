// free: 0, params: 0
value* closure_0(value* env) {


block_0: // 
  value a = caml_alloc(2, 248, caml_copy_string("Out_of_memory"), Val_int(-1));
  value b = caml_alloc(2, 248, caml_copy_string("Sys_error"), Val_int(-2));
  value c = caml_alloc(2, 248, caml_copy_string("Failure"), Val_int(-3));
  value d = caml_alloc(2, 248, caml_copy_string("Invalid_argument"), Val_int(-4));
  value e = caml_alloc(2, 248, caml_copy_string("End_of_file"), Val_int(-5));
  value f = caml_alloc(2, 248, caml_copy_string("Division_by_zero"), Val_int(-6));
  value g = caml_alloc(2, 248, caml_copy_string("Not_found"), Val_int(-7));
  value h = caml_alloc(2, 248, caml_copy_string("Match_failure"), Val_int(-8));
  value i = caml_alloc(2, 248, caml_copy_string("Stack_overflow"), Val_int(-9));
  value j = caml_alloc(2, 248, caml_copy_string("Sys_blocked_io"), Val_int(-10));
  value k = caml_alloc(2, 248, caml_copy_string("Assert_failure"), Val_int(-11));
  value l = caml_alloc(2, 248, caml_copy_string("Undefined_recursive_module"), Val_int(-12));
  value m = caml_copy_int64(9218868437227405312);
  value n = caml_copy_int64(-4503599627370496);
  value o = caml_copy_int64(9221120237041090561);
  value p = caml_copy_int64(9218868437227405311);
  value q = caml_copy_int64(4503599627370496);
  value r = caml_copy_int64(4372995238176751616);
  value s = caml_copy_string("hello");
  value t = caml_alloc(2, 0, caml_alloc(1, 0, caml_copy_string("world")), caml_alloc(2, 0, Val_int(0), Val_int(0)));
  value u = caml_register_global(Val_int(11), l, caml_copy_string("Undefined_recursive_module"));
  value v = caml_register_global(Val_int(10), k, caml_copy_string("Assert_failure"));
  value w = caml_register_global(Val_int(9), j, caml_copy_string("Sys_blocked_io"));
  value x = caml_register_global(Val_int(8), i, caml_copy_string("Stack_overflow"));
  value y = caml_register_global(Val_int(7), h, caml_copy_string("Match_failure"));
  value z = caml_register_global(Val_int(6), g, caml_copy_string("Not_found"));
  value A = caml_register_global(Val_int(5), f, caml_copy_string("Division_by_zero"));
  value B = caml_register_global(Val_int(4), e, caml_copy_string("End_of_file"));
  value C = caml_register_global(Val_int(3), d, caml_copy_string("Invalid_argument"));
  value D = caml_register_global(Val_int(2), c, caml_copy_string("Failure"));
  value E = caml_register_global(Val_int(1), b, caml_copy_string("Sys_error"));
  value F = caml_register_global(Val_int(0), a, caml_copy_string("Out_of_memory"));
  
goto block_736;
block_736: // 
  
goto block_2108;
block_2108: // 
  value G = caml_ensure_stack_capacity(Val_int(199));
  value H = caml_fresh_oo_id(Val_int(0));
  value I = caml_int64_float_of_bits(m);
  value J = caml_int64_float_of_bits(n);
  value K = caml_int64_float_of_bits(o);
  value L = caml_int64_float_of_bits(p);
  value M = caml_int64_float_of_bits(q);
  value N = caml_int64_float_of_bits(r);
  value O = caml_ml_open_descriptor_in(Val_int(0));
  value P = caml_ml_open_descriptor_out(Val_int(1));
  value Q = caml_ml_open_descriptor_out(Val_int(2));
value R = caml_alloc_closure(closure_1781, 1)

R->env[0] = b;
  value S = caml_alloc(1, 0, R);
  
goto block_2830;
block_2830: // 
  value T = caml_alloc(1, 0, s);
  value U = caml_alloc(2, 0, T, t);
  
goto block_2871;
block_2871: // 
  value V = U;
W = V;

goto block_2797;
block_2797: //W 
  if (W) { 
goto block_2800; } else { 
goto block_2812; }
block_2800: // 
  value X = Field(W, 1);
  value Y = Field(W, 0);
  
goto block_2873;
block_2873: // 
  
goto block_2815;
block_2815: // 
  if (Y) { 
goto block_2818; } else { 
goto block_2825; }
block_2818: // 
  value Z = Field(Y, 0);
  value _ = weeeee(Z);
  
goto block_2872;
block_2872: // 
  value $ = X;
W = $;

goto block_2797;

block_2825: // 
  value aa = weeeee(caml_copy_string("hello"));
  
goto block_2872;

block_2812: // 
  
goto block_2870;
block_2870: // 
  
goto block_2869;
block_2869: // 
  
goto block_1147;
block_1147: // 
  value ab = Val_int(0);
  value ac = caml_atomic_load(S);
  value ad = caml_call1(ac, ab) // not exact;
  
goto block_2868;
block_2868: // 
  return Val_unit;
}

// free: 1, params: 1
value* closure_1781(value* env) {
  value b = env[0];
  value ae = env[1];

block_1781: // 
  value af = caml_ml_out_channels_list(Val_int(0));
  
goto block_2867;
block_2867: // 
  value ag = af;
ah = ag;

goto block_1744;
block_1744: //ah 
  if (ah) { 
goto block_1747; } else { 
goto block_1778; }
block_1747: // 
  value ai = Field(ah, 1);
  value aj = Field(ah, 0);
  
goto block_1753;
block_1753: // 
  // Exception handling not implemented
block_1778: // 
  value ak = Val_int(0);
  return ak;
}

int main() {
  caml_main(closure_0);
  return 0;
}
