open Sast
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
  let lexbuf = Lexing.from_channel stdin in
  let program = Parser.program (deflate Scanner.token) lexbuf in
  let sprogram = try Some (Semant.check program) with Failure err -> Printf.fprintf output "Error: %s\n" err; None in
  let output_text = match sprogram with
      Some _ -> "Passed semantics check"
    | None -> "Failed semantics check" in
  Printf.fprintf stdout "%s\n" output_text
