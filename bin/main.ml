open CoBruh

type action = Parse | Semantics | Ir | Compile

let _ =
  let deflate token = 
    let q = Queue.create () in
    fun lexbuf -> 
      if not (Queue.is_empty q) then Queue.pop q else   
        match token lexbuf with 
          | [] -> Parser.EOF 
          | [tok] -> tok
          | hd::tl -> List.iter (fun tok -> Queue.add tok q) tl; hd 
  in
  let action = ref Compile in
  let set_action a () = action := a in
  let speclist = [
    ("-a", Arg.Unit (set_action Parse), "Print AST");
    ("-s", Arg.Unit (set_action Semantics), "Print semantics check");
    ("-l", Arg.Unit (set_action Ir), "Print LLVM IR");
    ("-c", Arg.Unit (set_action Compile), "Check and print LLVM IR (default)");
  ] in  
  let usage_msg = "usage: ./cobruh [-a|-s|-l|-c] [file.bruh]" in
  let in_channel = ref stdin in
  let out_channel = ref stdout in
  Arg.parse speclist (fun filename -> in_channel := open_in filename) usage_msg;
  
  let lexbuf = Lexing.from_channel !in_channel in
  let get_ast () = Option.get (try Some (Parser.program (deflate Scanner.token) lexbuf) with Failure err -> Printf.fprintf !out_channel "\nError in scanner/parser: %s\n" err; exit 0) in
  let get_sast ast = Option.get (try Some (Semant.check ast) with Failure err -> Printf.fprintf !out_channel "\nError in semantics: %s\n" err; exit 0) in
  let get_ir sast = Option.get (try Some (Irgen.translate sast) with Failure err -> Printf.fprintf !out_channel "\nError in IR: %s\n" err; exit 0) in
  match !action with
      Parse -> Printf.fprintf !out_channel "\n%s\n" (Ast.string_of_program (get_ast ()))
    | Semantics -> 
        let ast = get_ast () in
        Printf.fprintf !out_channel "\n%s\n" (Ast.string_of_program ast);
        let _ = get_sast ast in
        Printf.fprintf !out_channel "\n%s\n" "Passed semantics check"
    | Ir ->
        let ast = get_ast () in
        let sast = get_sast ast in
        let ir = get_ir sast in
        Printf.fprintf !out_channel "\n%s\n" (Llvm.string_of_llmodule ir)
    | Compile ->
        let ast = get_ast () in
        let sast = get_sast ast in
        let ir = get_ir sast in
        Llvm_analysis.assert_valid_module ir;
        Printf.fprintf !out_channel "\n%s\n" (Llvm.string_of_llmodule ir)
