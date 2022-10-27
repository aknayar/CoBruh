open Ast

let _ =
  let lexbuf = Lexing.from_channel (open_in "./input.bruh") in
  let prog = Parser.program_rule Scanner.token lexbuf in
  let _ = do_program prog in 0