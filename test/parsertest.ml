open CoBruh

let deflate token = 
  let q = Queue.create () in
  fun lexbuf -> 
    if not (Queue.is_empty q) then Queue.pop q else   
      match token lexbuf with 
        | [] -> Parser.EOF 
        | [tok] -> tok
        | hd::tl -> List.iter (fun tok -> Queue.add tok q) tl; hd 

let _ =
  let input_file = open_in "./parserinput.bruh" in
  let lexbuf = Lexing.from_channel input_file in
  let program = Parser.program (deflate Scanner.token) lexbuf in
  let output_file = open_out "./parseroutput.txt" in
  Printf.fprintf output_file "%s\n" (Ast.string_of_program program);
  close_out output_file;
