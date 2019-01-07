open Format 
open DEMONAst
open MipsDEMONLib

exception CompException of string

let (genvar: (string, unit) Hashtbl.t) = Hashtbl.create 17

module StrMap = Map.Make(String)
(* 
first ++ move t1 t0 ++ second ++ move t2 t0 ++ beq t0 t0 oreg t1
first ++ move t1 t0 ++ second ++ move t2 t0 ++ bne t0 t0 oreg t1
    | _ -> nop
*)

let rec compile_expr = function
    Econst i ->
    begin
        match i with
            I j -> li t0 j
            (*|F j ->l.s f0 j*)
            |B true-> li t0 1
            |_ -> nop
    end
    |Evar s ->nop
    |Binop (op,e1,e2)->
      let first = compile_expr e1 in
      let second = compile_expr e2 in
      begin
        match op with   (*verificar*)
          Plus -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ add t0 t1 oreg t2 
          |Minus -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ sub t0 t1 oreg t2 
          |Times -> first ++ move t3 t0 ++ second ++ move t4 t0 ++ mul t0 t3 oreg t4 
          |Div -> first ++ move t3 t0 ++ second ++ move t4 t0 ++ div t1 t2 ++ mflo t0
          |Smaller -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ slt t0 t0 oreg t1 
          |Larger -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ sgt t0 t0 oreg t1
          |Lequal -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ sle t0 t0 oreg t1
          |Sequal -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ sge t0 t0 oreg t1
          |Equals -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ seq t0 t0 oreg t1
          |Notequal -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ sne t0 t0 oreg t1
          |And -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ and_ fp t0 t1
          |Or -> first ++ move t1 t0 ++ second ++ move t2 t0 ++ or_ fp t0 t1
          |Not -> first ++ move t1 t0 ++ not_ t0 t1
      end
    |Unop ( binop, e )->nop
    |Letin ( str, e1, e2 )->nop

let compile_instr = function
    Setter ( string, e ) ->nop
    |Print ( e ) -> li v0 1 ++ compile_expr e ++ move a0 t0 ++ syscall
    |Sif ( e, stmt ) ->nop
    |Sifelse ( e, stmt1, stmt2 )  ->nop
    |Swhile ( e, stmt ) ->nop
    |Sfor ( string, e1, e2, stmt ) ->nop
    |Sfordt ( string, e1, e2, stmt) -> nop


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
  MipsDEMONLib.print_program fmt p;
  (* "flush" do buffer para garantir que tudo foi para aí escrito
     antes de o fechar *)
  fprintf fmt "@?";
  close_out f