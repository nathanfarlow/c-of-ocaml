open Core

let command =
  Command.basic
    ~summary:"Compile OCaml bytecode executable (.bc) to C"
    (let%map_open.Command input_file = anon ("INPUT_FILE" %: Filename_unix.arg_type) in
     fun () ->
       In_channel.create input_file
       |> C_of_ocaml.Driver.go
       |> Out_channel.output_string stdout)
;;

let () = Command_unix.run command
