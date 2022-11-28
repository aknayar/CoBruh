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
  let input_file = open_in "./input.bruh" in
  let lexbuf = Lexing.from_channel input_file in
  let program = Parser.program (deflate Scanner.token) lexbuf in
  let parser_output = Ast.string_of_program program in
  Printf.fprintf stdout "\n%s\n" (parser_output);
  let sprogram = try Some (Semant.check program) with Failure err -> Printf.fprintf stdout "Error: %s\n" err; None in
  let semantics_output = match sprogram with
      Some _ -> "Passed semantics check"
    | None -> "Failed semantics check" in
  Printf.fprintf stdout "\n%s\n" semantics_output
