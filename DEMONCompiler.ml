open Format 
open DEMONAst
open mipsDEMONLib

exception CompException of string

let (genvar: (string, unit) Hashtbl.t) = Hashtbl.create 17

module StrMap = Map.Make(String)

let compile_expr = function
    Cst i ->
    begin
        match i with
            I j -> li t0 js
            (*|F j ->l.s f0 j*)
            |B true-> li t0 1
            |_ -> nop
    end
    |Var s ->nop
    |Binop op ( op,e1,e2 )->
      let first = compile_expr e1 in
      let second = compile_expr e2 in
      begin
        match op with   (*verificar*)
          Plus -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ add t0 t1 oreg t2 
          |Minus -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ sub t0 t1 oreg t2 
          |Times -> first ++ move t3 t0 ++ second ++ move t4 t0 ++ mult t0 t3 oreg t4 
          |Div -> first ++ move t3 t0 ++ second ++ move t4 t0 ++ div t1 t2 ++ mflo t0
          |Smaller -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ slt t0 t0 oreg t1
          |Larger -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ sgt t0 t0 oreg t1
          |Lequal -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ sle t0 t0 oreg t1
          |Sequal -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ sge t0 t0 oreg t1
          |Equals -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ beq t0 t0 oreg t1
          |Notequal -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ bne t0 t0 oreg t1
      end
    |Unop ( binop, e )->nop
    |Letin ( str, e1, e2 )->nop
    | _ -> nop

let compile_instr = function
    Set ( str, e ) ->nop
    |Print ( e ) -> li v0 1 ++ compile_expr e ++ move a0 t0 ++ syscall
    |If ( e, stmtl ) ->nop
    |IfElse ( e, stmtl1, stmtl2 )  ->nop
    |While ( e, stmtl ) ->nop
    |For ( str, e1, e2, stmtl ) ->nop
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
