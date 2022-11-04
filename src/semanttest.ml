open Sast

let _ =
  let lexbuf = Lexing.from_channel (open_in "./test_src/semantinput.bruh") in
  let program = Parser.program Scanner.token lexbuf in
  let sprogram = try Some (Semant.check program) with Failure err -> None in
  let output = open_out "./test_src/semantoutput.txt" in
  let output_text = match sprogram with
      Some _ -> "Passed semantics check"
    | None -> "Failed semantics check" in
  Printf.fprintf output "%s\n" (output_text);
  close_out output;
  0
