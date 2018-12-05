* Produção de código para a linguagem Arith *)

open Format
open X86_64
open Ast

(* Excepção por levantar quando uma variável
(local ou global) é mal utilizada *)
exception VarUndef of string

(* Tamanho da frame, em byte (cada variável local ocupa 8 bytes) *)
let frame_size = ref 0

(* As variáveis globais são arquivadas numa tabela de dispersão *)
let (genv : (string, unit) Hashtbl.t) = Hashtbl.create 17

(* Utilizamos uma tabela associativa cujas chaves são as variáveis locais
   (cadeias de caracteres) e onde o valor associado é a posição
   relativamente  a $fp (em bytes) *)
module StrMap = Map.Make(String)

(* Compilação de uma expressão *)
let compile_expr =
  (* Função recursiva local a compile_expr utilizada para gerar o código
     máquina da árvore de sintaxe abstracta associada a um valor de tipo
     Ast.expr ; no fim da execução deste código, o valor deve estar
     no topo da pilha *)
  let rec comprec env next = function
    | Cst i ->
        movq (imm i) (reg rax) ++
        pushq rax
    | Var x ->
        begin
          try
            let ofs = - (StrMap.find x env) in
            movq (ind ~ofs rbp) (reg rax) ++
            pushq rax
          with Not_found ->
            if not (Hashtbl.mem genv x) then raise (VarUndef x);
            movq (lab x) (reg rax) ++
            pushq rax
        end
    | Binop (Div, e1, e2)-> (* um caso particular para a divisão *)
        comprec env next e1 ++
        comprec env next e2 ++
        movq (imm 0) (reg rdx) ++
        popq rbx ++
        popq rax ++
        idivq (reg rbx) ++
        pushq rax
    | Binop (o, e1, e2)->
        let op = match o with
          | Add -> addq
          | Sub -> subq
          | Mul -> imulq
          | Div -> assert false
        in
        comprec env next e1 ++
        comprec env next e2 ++
        popq rbx ++
        popq rax ++
        op (reg rbx) (reg rax) ++
        pushq rax
    | Letin (x, e1, e2) ->
        if !frame_size = next then frame_size := 8 + !frame_size;
        comprec env next e1 ++
        popq rax ++
        movq (reg rax) (ind ~ofs:(-next) rbp) ++
        comprec (StrMap.add x next env) (next + 8) e2
  in
  comprec StrMap.empty 0

(* Compilação de uma instrução *)
let compile_instr = function
  | Set (x, e) ->
      let code =
        compile_expr e ++
        popq rax ++
        movq (reg rax) (lab x)
      in
      Hashtbl.replace genv x ();
      code
  | Print e ->
      compile_expr e ++
      popq rdi ++
      call "print_int"


(* Compilação do programa p e grava o código no ficheiro ofile *)
let compile_program p ofile =
  let code = List.map compile_instr p in
  let code = List.fold_right (++) code nop in
  let p =
    { text =
        glabel "main" ++
        subq (imm !frame_size) (reg rsp) ++ (* aloca a frame *)
        leaq (ind ~ofs:(!frame_size - 8) rsp) rbp ++ (* $fp = ... *)
        code ++
        addq (imm !frame_size) (reg rsp) ++ (* desaloca a frame *)
        movq (imm 0) (reg rax) ++ (* exit *)
        ret ++
        label "print_int" ++
        movq (reg rdi) (reg rsi) ++
        movq (ilab ".Sprint_int") (reg rdi) ++
        movq (imm 0) (reg rax) ++
        call "printf" ++
        ret;
      data =
        Hashtbl.fold (fun x _ l -> label x ++ dquad [1] ++ l) genv
          (label ".Sprint_int" ++ string "%d\n")
    }
  in
  let f = open_out ofile in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  (* "flush" do buffer para garantir que tudo foi para aí escrito
     antes de o fechar *)
  fprintf fmt "@?";
  close_out f