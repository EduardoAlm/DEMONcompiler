{
  open Lexer
  open Parser

  exception Lexing_error of string

  let kwTbl =
    [
      "if", IF;
      "then", THEN;
      "else", ELSE;
      "while", WHILE;
      "for", FOR;
      "to", TO;
      "do", DO;
      "downto", DOWNTO;
      "done", DONE;
      "let", LET;
      "in", IN;
      "print", PRINT;
    ]
  
  let id_or_kw =
    let h = Hashtbl.create 13 in
    List.iter (fun (s, t) -> Hashtbl.add h s t) kwTbl;
    fun s ->
      let s = String.lowercase s in 
      try List.assoc s kwdTbl with _ -> IDENT s 

   let newline lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_lnum = pos.pos_lnum + 1; pos_bol = pos.pos_cnum }
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let ident = letter (letter | digit | '_')*
let integer = ['0'-'9']+
let float = digit* '.' digit+
let bool = ("true" | "false")
let space = [' ' '\t']

rule token = parse 
  | "//" [^ '\n']* '\n'
  | '\n' {newline lexbuf;token lexbuf} 
  | space+ {token lexbuf}   
  | ident as id {id_or_kw id}
  | '+' {PLUS}
  | '-' {MINUS}
  | '*' {TIMES} 
  | '/' {DIV}
  | '>' {LARGER} 
  | ">=" {LEQUAL} 
  | '<' {SMALLER} 
  | "<=" {SEQUAL} 
  | '=' {EQ}
  | "==" {EQUALS}
  | "!=" {NOTEQUAL}
  | "not" {NOT}
  | "&&"  {AND}
  | "||" {OR}
  | ';' {SCOLON}
  | '(' {LPAR}
  | ')' {RPAR}
  | "(*" {comment lexbuf}
  | integer as i { Econst (int_of_string i)}
  | float as f { Econst (float_of_string f)}
  | bool as b { Econst (bool_of_string b)}
  | "//" [^ '\n']* eof
  | eof {EOF}
  | _ as c {raise (Lexing_error ("illegal character: " ^ String.make 1 c ^ ">:)"))}

      and comment = parse
      | "*)"    { token lexbuf }
      | _       { comment lexbuf }
      | eof     { raise (Lexing_error ("unterminated comment")) }