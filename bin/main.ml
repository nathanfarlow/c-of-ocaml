open! Core

let () =
  Command_unix.run
    (Command.basic
       ~summary:"Compile a cmo file to C. Give cmo file to stdin"
       (let%map_open.Command () = return () in
        Coo.Driver.go))
;;
