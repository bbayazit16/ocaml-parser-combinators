open Parser

let rec repl () =
  print_string "> ";
  flush stdout;
  match read_line () with
  | exception End_of_file -> exit 0
  | input ->
      if String.trim input = String.empty then
        repl ()
      else
        match parse_json input with
        | Ok (json, _) -> 
            print_endline (string_of_json json);
            repl ()
        | Error e -> 
            print_endline ("Error: " ^ e);
            repl ()

let () = repl ()
