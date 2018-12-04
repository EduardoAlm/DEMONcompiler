%{
    open ast
%} 

%token <int> INT
%token <float> FLOAT
%token <bool> BOOLEAN
(*int vai ser um contrutor que recebe sempre um inteiro*)
%token EOF
%token PLUS MINUS TIMES DIV
%token LARGER SMALLER LEQUAL SEQUAL EQUAL NOTEQUAL 
%token NOT AND OR
%token IF THEN ELSE ASSIGN

%left OR
%left AND
%nonassoc NOT
%nonassoc LARGER SMALLER LEQUAL SEQUAL EQUAL NOTEQUAL 
%left PLUS MINUS
%left TIMES DIV
%left NEG
%start <Ast.expr> main
%%

main: e = expr EOF {e}
    | s = stmt EOF {s}
    
 
expr: c 
    | e1=expr e2=expr o=op {Binop(o, e1, e2)}
    | MINUS e = expr %prec NEG{Binop(Minus, I 0, e)}
    
   c: i = INT {I i}
    | f = FLOAT {F f}
    | b = BOOLEAN {B b}   

stmt:
    IF e THEN stmt { if e then stmt }
    |IF e THEN stmt2 ELSE stmt { if e then stmt2 else stmt}
    |ASSIGN { };

stmt2:
    IF e THEN stmt2 ELSE stmt2 {if e then stmt2 else stmt2 }
    |ASSIGN { }; 

op:
    PLUS {Plus}
   | MINUS {Minus}
   | TIMES {Times}
   | DIV {Div}
   | AND {And}
   | OR {Or}
   | NOT {Not}
   | LARGER {'>'}
   | SMALLER {'<'}
   | LEQUAL {">="}
   | SEQUAL {"<="}
   | EQUAL {"=="}
   | NOTEQUAL {"!="}

                                 *
                                / \
                               / / \ 
                              /_____\
                                |_|

