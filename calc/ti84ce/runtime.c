#include <graphx.h>
#include <stdlib.h>
#include <ti/getcsc.h>
#include <ti/screen.h>

value caml_gfx_begin(value _unit) {
  gfx_Begin();
  return Val_unit;
}

value caml_gfx_end(value _unit) {
  gfx_End();
  return Val_unit;
}

value caml_gfx_set_draw_buffer(value _unit) {
  gfx_SetDrawBuffer();
  return Val_unit;
}

value caml_gfx_swap_draw(value _unit) {
  gfx_SwapDraw();
  return Val_unit;
}

value caml_gfx_fill_screen(value color) {
  gfx_FillScreen(Int_val(color));
  return Val_unit;
}

value caml_gfx_set_color(value color) {
  gfx_SetColor(Int_val(color));
  return Val_unit;
}

value caml_gfx_line(value x1, value y1, value x2, value y2) {
  gfx_Line(Int_val(x1), Int_val(y1), Int_val(x2), Int_val(y2));
  return Val_unit;
}

value caml_gfx_print_string(value s, value x, value y) {
  gfx_PrintStringXY(Str_val(s), Int_val(x), Int_val(y));
  return Val_unit;
}

value caml_gfx_get_string_width(value s) {
  return Val_int(gfx_GetStringWidth(Str_val(s)));
}

value caml_gfx_set_text_fg_color(value color) {
  gfx_SetTextFGColor(Int_val(color));
  return Val_unit;
}

value caml_os_clr_home(value _unit) {
  os_ClrHome();
  return Val_unit;
}

value caml_os_put_str_full(value str) {
  os_PutStrFull(Str_val(str));
  return Val_unit;
}

value caml_os_get_csc(value _unit) { return Val_int(os_GetCSC()); }

#include <debug.h>
value caml_dbg_print(value s) {
  dbg_printf("%s\n", Str_val(s));
  return Val_unit;
}
