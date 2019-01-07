(*Expressoes inteiras*)
(* Sintaxe abstracta proveniente da analise sint�ctica *)

type econst =  I of int | F of float | B of bool

type binop = Plus | Minus | Times | Div 

type boolop = And | Or | Not | Larger | Smaller | Lequal
| Sequal | Equals | Notequal 

type expr = 
          | Econst of econst
          | Evar of string
          | Boolop of boolop * expr * expr 
          | Bunop of boolop * expr
          | Unop of binop * expr 
          | Binop of binop * expr * expr
          | Letin of string * expr * expr
          
type stmt =
          | Setter of string * expr
          | Print of expr
          | Sif of expr * stmt list
          | Sifelse of expr * stmt list * stmt list 
          | Swhile of expr * stmt list
          | Sfor of string * expr * expr * stmt list 
          | Sfordt of string * expr * expr * stmt list 

type prog = stmt list 

(* Sintaxe abstracta ap�s aloca��o das vari�veis (ver compile.ml) *)
(*type pconst =  I of int | F of float | B of bool

type pexpr =
  | PConst of pconst
  (*| LVar of string*)
  | PBinop of binop * pexpr * pexpr
  | PLetin of string * pexpr * pexpr
  | PUnop of binop * pexpr

type pstmt =
  | PSetter of string * pexpr
  | PPrint of pexpr
  | PSif of pexpr * pstmt list
  | PSifelse of pexpr * pstmt list * pstmt list
  | PSwhile of pexpr * pstmt list
  | PSfor of string * pexpr * pexpr * pstmt list
  | PSfordt of string * pexpr * pexpr * pstmt list

type pprogram = pstmt list*)