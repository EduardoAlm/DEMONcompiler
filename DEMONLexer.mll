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
      "do", DONE;
      "downto", DOWNTO;
      "done", DONE;
      "let", LET;
      "in", IN;
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
let id = letter (letter | digit | '_')*
let integer = ['0'-'9']+
let space = [' ' '\t']

rule token = parse 
  | "//" [                                                                                                                                                                                                                               ]