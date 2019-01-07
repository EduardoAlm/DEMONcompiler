%{
    open DEMONAst
%} 

%token <int> INT
%token <float> FLOAT
%token TRUE
%token FALSE
%token <string> IDENT
%token IN
%token EOF
%token PLUS MINUS TIMES DIV
%token LARGER SMALLER LEQUAL SEQUAL EQ NOTEQUAL EQUALS
%token NOT AND OR
%token IF THEN ELSE SET
%token WHILE FOR TO DOWNTO DO DONE
%token LET
%token LPAR RPAR
%token SCOLON
%token PRINT

%nonassoc IN
%left OR
%left AND
%nonassoc NOT
%nonassoc LARGER SMALLER LEQUAL SEQUAL NOTEQUAL EQUALS
%left PLUS MINUS
%left TIMES DIV
%nonassoc NEG

%start prog

%type <DEMONAst.prog> prog
%%

prog: 
      s = stmts EOF {List.rev s} 
      ;

stmts: i = stmt {[i]}
     | l =stmts SCOLON i=stmt {i::l}
     ; 

const: i = INT {I i}
    | f = FLOAT {F f}
    |TRUE {B true}
    |FALSE {B false}    
    ;

expr: 
      c = const {Econst c}
    | id = IDENT {Evar id}
    | LPAR e=expr RPAR { e }
    | e1=expr o=op e2=expr  {Binop(o, e1, e2)}
    | e1=expr o=boolop e2= expr {Boolop(o, e1, e2)}
    | NOT e=expr {Bunop(Not, e)}
    | MINUS e = expr %prec NEG {Unop(Minus, e)}
    | LET id = IDENT EQ e1=expr IN e2=expr  {Letin(id, e1, e2)}  
    ;


stmt:
     SET id=IDENT EQ e=expr {Setter(id, e)} 
    | PRINT e = expr {Print e} 
    | IF e=expr THEN s=stmts DONE { Sif(e, s) }
    | IF e=expr THEN s2=stmts ELSE s=stmts DONE { Sifelse (e, s2, s) }
    | WHILE e=expr DO s=stmts DONE {Swhile(e, s)}
    | FOR id=IDENT EQ e1=expr TO e2=expr DO s=stmts DONE {Sfor (id, e1, e2, s)}
    | FOR id=IDENT EQ e1=expr DOWNTO e2=expr DO s=stmts DONE {Sfordt (id, e1, e2, s)}
    ;

%inline op:
    PLUS {Plus}
   | MINUS {Minus}
   | TIMES {Times}
   | DIV {Div}   
   ;

%inline boolop:
    AND {And}
   | OR {Or}
   | NOT {Not}
   | LARGER {Larger}
   | SMALLER {Smaller}
   | LEQUAL {Lequal}
   | SEQUAL {Sequal}
   | EQUALS {Equals}
   | NOTEQUAL {Notequal}
    ;


                           (*    *
                                / \
                               / / \ 
                              /_____\
                                |_|   *)

