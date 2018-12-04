%{
    open Ast
%} 

%token <int> INT
%token <float> FLOAT
%token <bool> BOOLEAN
%token <string> IDENT
%token <string> IN
%token EOF
%token PLUS MINUS TIMES DIV
%token LARGER SMALLER LEQUAL SEQUAL EQ NOTEQUAL EQUALS
%token NOT AND OR
%token IF THEN ELSE SET 
%token LET
%token LPAR RPAR

%left RPAR
%left LPAR 
%nonassoc IN
%left OR
%left AND
%nonassoc NOT
%nonassoc LARGER SMALLER LEQUAL SEQUAL NOTEQUAL EQUALS
%left PLUS MINUS
%left TIMES DIV
%nonassoc NEG

%start prog

%type <Ast.expr> prog
%%

prog: 
    | s = stmt EOF {s} ;
   
expr: 
    | c = const {Econst c}
    | id = IDENT {Evar id}
    | LPAR e=expr RPAR { e }
    | e1=expr o=op e2=expr  {Binop(o, e1, e2)}
    | NOT e=expr {Unop(Not, e)}
    | MINUS e = expr %prec NEG {Unop(Minus, e)}
    | LET id = IDENT EQ e1=expr IN e2=expr  {Letin(id, e1, e2)}  
    
const: i = INT {I i}
    | f = FLOAT {F f}
    | b = BOOLEAN {B b}  
    
stmt:
    IF e=expr THEN s=stmt { if e then s }
    |IF e=expr THEN s2=stmt2 ELSE s=stmt { if e then s2 else s}
    |ASSIGN {  };

stmt2:
    IF e THEN stmt2 ELSE stmt2 {if e then stmt2 else stmt2 }
    |ASSIGN { }; 

%inline op:
    PLUS {Plus}
   | MINUS {Minus}
   | TIMES {Times}
   | DIV {Div}
   | AND {And}
   | OR {Or}
   | NOT {Not}
   | LARGER {Larger}
   | SMALLER {Smaller}
   | LEQUAL {Lequal}
   | SEQUAL {Sequal}
   | EQUALS {Equals}
   | NOTEQUAL {Notequal}


                           (*    *
                                / \
                               / / \ 
                              /_____\
                                |_|   *)

