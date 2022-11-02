open Ast

let _ =
  let lexbuf = Lexing.from_channel (open_in "./test_src/input.bruh") in
  let prog = Parser.program Scanner.token lexbuf in
  let _ = do_program prog in 0