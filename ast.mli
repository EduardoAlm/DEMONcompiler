(*Expressoes inteiras*)
type econst =  I of int | F of float | B of bool

type binop = Plus | Minus | Times | Div | And | Or | Not | Larger | Smaller | Lequal
            | Sequal | Equals | Notequal 

type expr = 
          | Econst of econst
          | Evar of string
          | Binop of binop * expr * expr
          | Letin of string * expr * expr
          | Unop of binop * expr 
