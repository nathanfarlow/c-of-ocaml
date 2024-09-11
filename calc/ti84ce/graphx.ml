external begin_ : unit -> unit = "caml_gfx_begin"
external end_ : unit -> unit = "caml_gfx_end"
external set_draw_buffer : unit -> unit = "caml_gfx_set_draw_buffer"
external swap_draw : unit -> unit = "caml_gfx_swap_draw"
external fill_screen : int -> unit = "caml_gfx_fill_screen"
external set_color : int -> unit = "caml_gfx_set_color"
external line : int -> int -> int -> int -> unit = "caml_gfx_line"
external print_string : string -> int -> int -> unit = "caml_gfx_print_string"
external get_string_width : string -> int = "caml_gfx_get_string_width"
external set_text_fg_color : int -> unit = "caml_gfx_set_text_fg_color"

let lcd_width = 320
let lcd_height = 240
