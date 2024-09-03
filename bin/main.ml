open Core

let command =
  Command.basic
    ~summary:"Run the Coo driver"
    (let%map_open.Command
       (* flag "-o" (required Filename_unix.arg_type) ~doc:"FILE Output file" *)
       input_file
       =
       anon ("INPUT_FILE" %: Filename_unix.arg_type)
     in
     fun () ->
       In_channel.create input_file |> Coo.Driver.go |> Out_channel.output_string stdout)
;;

let () = Command_unix.run command
