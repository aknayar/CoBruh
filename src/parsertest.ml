open Ast
open Parser

let deflate token = 
  let q = Queue.create () in
  fun lexbuf -> 
    if not (Queue.is_empty q) then Queue.pop q else   
      match token lexbuf with 
        | [] -> EOF 
        | [tok] -> tok
        | hd::tl -> List.iter (fun tok -> Queue.add tok q) tl ; hd 

let _ =
  let lexbuf = Lexing.from_channel (open_in "./test_src/parserinput.bruh") in
  let program = Parser.program (deflate Scanner.token) lexbuf in
  let output = open_out "./test_src/parseroutput.txt" in
  Printf.fprintf output "%s\n" (string_of_program program);
  close_out output;
