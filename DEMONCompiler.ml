open Format 
open mipsDEMONLib
open DEMONAst

exception CompException of string

let (genvar: (string, unit) Hashtbl.t) = Hashtbl.create 17

module StrMap = Map.Make(String)

let compile_expr = function
    Cst i ->
    begin
        match i with
            I j -> li t0 j
            (*|F j ->l.s f0 j*)
            |B true-> li t0 1
            |_ -> nop
    end
    |Var s ->nop
    |Binop op->
    begin
        match op with   (*verificar*)
            Plus ->li v0 5 ++ compile_expr e1 ++ move t0 v0 ++ li v0 5 ++ move t1 v0 ++ add t2 t0 t1 ++ move a0 t2 ++ li v0 1 ++ syscall
            |Minus ->li v0 5 ++ compile_expr e1 ++ move t0 v0 ++ li v0 5 ++ move t1 v0 ++ sub t2 t0 t1 ++ move a0 t2 ++ li v0 1 ++ syscall
            |Times ->li v0 5 ++ compile_expr e1 ++ move t0 v0 ++ li v0 5 ++ move t1 v0 ++ mult t2 t0 t1 ++ move a0 t2 ++ li v0 1 ++ syscall
            |Div ->li v0 5 ++ compile_expr e1 ++ move t0 v0 ++ li v0 5 ++ move t1 v0 ++ div t2 t0 t1 ++ move a0 t2 ++ li v0 1 ++ syscall
            |Smaller ->nop
            |Larger ->nop
            |Lequal ->nop
            |Sequal ->nop
            |Equals ->nop
            |Notequal ->nop
    end
    |Unop binop e->nop
    |Letin str e1 e2->nop
    | _ -> nop

let compile_instr = function
    Set str e ->nop
    |Print e -> li v0 1 ++ compile_expr e ++ move a0 t0 ++ syscall
    |If e stmtl ->nop
    |IfElse e stmtl1 stmtl2 ->nop
    |While e stmtl ->nop
    |For str e1 e2 stmtl ->nop
    | _ -> nop


(* Compilação do programa p e grava o código no ficheiro ofile *)
let compile_program p ofile =
  let code = List.map compile_instr p in
  let code = List.fold_right (++) code nop in
  let p =
    { text =
        label "main" ++
        code;
      data = nop
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  Mips.print_program fmt p;
  (* "flush" do buffer para garantir que tudo foi para aí escrito
     antes de o fechar *)
  fprintf fmt "@?";
  close_out f
