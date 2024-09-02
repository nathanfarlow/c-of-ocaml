// free: 0, params: 0
value *closure_0(value *env) {

block_0: //
  value a = caml_copy_string("hello");
  value b = caml_alloc(2, 0, caml_alloc(1, 0, caml_copy_string("world")),
                       caml_alloc(2, 0, Val_int(0), Val_int(0)));

  goto block_736;
block_736: //

  goto block_834;
block_834: //

  goto block_2113;
block_2113: //

  goto block_2920;
block_2920: //

  goto block_829;
block_829: //

  goto block_2919;
block_2919: //

  goto block_2528;
block_2528: //

  goto block_2823;
block_2823: //

  goto block_2872;
block_2872: //
  value c = caml_alloc(1, 0, a);
  value d = caml_alloc(2, 0, c, b);

  goto block_2918;
block_2918: //
  value e = d;
  f = e;

  goto block_2839;
block_2839: // f
  if (f) {
    goto block_2842;
  } else {
    goto block_2854;
  }
block_2842: //
  value g = Field(f, 1);
  value h = Field(f, 0);

  goto block_2922;
block_2922: //

  goto block_2857;
block_2857: //
  if (h) {
    goto block_2860;
  } else {
    goto block_2867;
  }
block_2860: //
  value i = Field(h, 0);
  value j = weeeee(i);

  goto block_2921;
block_2921: //
  value k = g;
  f = k;

  goto block_2839;

block_2867: //
  value l = weeeee(caml_copy_string("hello"));

  goto block_2921;

block_2854: //

  goto block_2917;
block_2917: //

  goto block_2916;
block_2916: //

  goto block_1157;
block_1157: //

  goto block_2914;
block_2914: //

  goto block_825;
block_825: //

  goto block_2913;
block_2913: //

  goto block_2924;
block_2924: //

  goto block_1786;
block_1786: //
  value m = caml_ml_out_channels_list(Val_int(0));

  goto block_2912;
block_2912: //
  value n = m;
  o = n;

  goto block_1749;
block_1749: // o
  if (o) {
    goto block_1752;
  } else {
    goto block_1783;
  }
block_1752: //
  value p = Field(o, 1);

  goto block_1758;
block_1758: //

  goto block_1759;
block_1759: //

  goto block_1763;
block_1763: //

  goto block_1779;
block_1779: //
  value q = p;
  o = q;

  goto block_1749;

block_1783: //

  goto block_2923;
block_2923: //

  goto block_2915;
block_2915: //
  return Val_unit;
}

int main() {
  caml_main(closure_0);
  return 0;
}
