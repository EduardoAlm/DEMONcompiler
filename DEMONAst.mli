(*Expressoes inteiras*)


type binop = Plus | Minus | Times | Div | And | Or | Not | Larger | Smaller | Lequal
            | Sequal | Equals | Notequal 

type econst =  I of int | F of float | B of bool

type expr = 
          | Econst of econst
          | Evar of string
          | Binop of binop * expr * expr
          | Letin of string * expr * expr
          | Unop of binop * expr 

type stmt =
          | Setter of string * expr
          | Print of expr
          | Sif of expr * stmt list
          | Sifelse of expr * stmt list * stmt list
          | Swhile of expr * stmt list
          | Sfor of string * expr * expr * stmt list
          | Sfordt of string * expr * expr * stmt list

type prog = stmt list   