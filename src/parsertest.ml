open Ast

let _ =
  let lexbuf = Lexing.from_channel (open_in "./test_src/input.bruh") in
  let program = Parser.program Scanner.token lexbuf in
  let output = open_out "./test_src/output.txt" in
  Printf.fprintf output "%s\n" (string_of_program program);
  close_out output;
