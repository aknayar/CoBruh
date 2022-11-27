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
  let input_file = open_in "./semantinput.bruh" in
  let lexbuf = Lexing.from_channel input_file in
  let program = Parser.program (deflate Scanner.token) lexbuf in
  let sprogram = try Some (Semant.check program) with Failure err -> Printf.fprintf stdout "Error: %s\n" err; None in
  let output_text = match sprogram with
      Some _ -> "Passed semantics check"
    | None -> "Failed semantics check" in
  let output_file = open_out "./semantoutput.txt" in
  Printf.fprintf stdout "%s\n" output_text;
  close_out output_file;
