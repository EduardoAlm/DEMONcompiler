(* Biblioteca para produzir código MIPS
   2008 Jean-Christophe Filliâtre (CNRS)
   - version initiale
   2013 Kim Nguyen (Université Paris Sud)
   - sous-types text et data
   - types fantômes pour oreg et oi
   - plus d'opérations et de directives
   - manipulation de la pile
   - ocamldoc
   2018 João Reis (University of Beira Interior)
   - tradução para português
*)

(** {0 Biblioteca para escrever programamas MIPS }
    O módulo {!Mips} permite a escrita de código MIPS usando código OCaml,
    sem necessidade de um pré-processador.
    Exemplo:
    O programa abaixo, feito à esquerda em MIPS e à direita em
    OCaml, carrega duas constantes, realiza alguma operações
    aritméticas e exibe o resultado no ecrã
    {[
        .text                                                |  { text =
        main:                                                |        label "main"
         #carrega 42 para $a0 e 23 para $a1                  |    ++  comment "carrega 42 para $a0 e 23 para $a1"
         li $a0,  42                                         |    ++  li  a0 42
         li $a1,  23                                         |    ++  li  a1 23
         mul $a0, $a0, $a1                                   |    ++  mul a0 a0 oreg a1 (* usamos oreg para dizer que o último
                                                             |                             operando é um registo *)
         #colocar o conteúdo de $a0 na pilha                 |    ++  comment "colocar o conteúdo de $a0 na pilha"
         sub $sp, $sp, 4                                     |    ++  sub sp sp oi 4
         sw  $a0,  0($sp)                                    |    ++  sw a0 areg (0, sp)
                                                             |
         #chamar a rotina de print                           |    ++  comment "chamar a rotina de print"
         jal print_int                                       |    ++  jal "print_int"
                                                             |
         #terminar                                           |    ++  comment "terminar"
         li $v0, 10                                          |    ++  li v0 10
         syscall                                             |    ++  syscall
                                                             |
      print_int:                                             |    ++  label "print_int"
         lw $a0,  0($sp)                                     |    ++  lw a0 areg (0, sp)
         add $sp, $sp, 4                                     |    ++  add sp sp oi 4
         li $v0, 1                                           |    ++  li v0 1
         syscall                                             |    ++  syscall
         #exibir o carriage return                           |    ++  comment "exibir o carriage return"
         la $a0, newline                                     |    ++  la a0 alab "newline"
         li $v0, 4                                           |    ++  li v0  4
         syscall                                             |    ++  syscall
         jr $ra                                              |    ++  jr ra  ; (* fim da etiqueta de texto *)
                                                             |
        .data                                                |    data =
       newline:                                              |        label "newline"
        .asciiz  "\n"                                        |    ++  asciiz "\n" ;
                                                             |  } (* fim do registo *)
    ]}
*)

type 'a asm
(** tipo abstrato para representar o código assembly. O parâmetro
    ['a] é utilisado como tipo fantasma. *)

type text = [ `text ] asm
(** tipo que representa o código assembly localizado na zona de
    texto *)

type data = [ `data ] asm
(** tipo que representante o código assembly localizado na zona de
    dados *)

type program = {
  text : text;
  data : data;
}
(** um programa é constituído por uma zona de texto e uma zona de
    dados *)

val print_program : Format.formatter -> program -> unit
  (** [print_program fmt p] imprime o código do programa [p] no
      formatador [fmt] *)

val print_in_file: file:string -> program -> unit

type register = string

val v0 : register
val v1 : register
val a0 : register
val a1 : register
val a2 : register
val a3 : register
val t0 : register
val t1 : register
val t2 : register
val t3 : register
val t4 : register
val s0 : register
val s1 : register
val ra : register
val sp : register
val fp : register
val gp : register
val zero : register
(** Constantes que representam os registos manipuláveis. [zero]
    está ligado a 0 *)


type label = string
(** As etiquetas de endereços são cadeias de caracteres *)

type 'a operand
val oreg : register operand
val oi : int operand
val oi32 : int32 operand

(** tipo abstrato que representa o último operando de uma
    expressão aritmética bem como 3 constantes (um registo,
    um inteiro ou um inteiro de 32 bits)
*)



(** {1 Operações aritméticas } *)



val li : register -> int -> text
val li32 : register -> int32 -> text
(** Carregamento de constantes inteiras *)

val abs : register -> register -> text
(** [abs r1 r2] alojar em r1 o valor absoluto de r2 *)

val neg : register -> register -> text
(** [neg r1 r2] alojar em r1 a negação de r2 *)

val addiu : register -> register -> 'a operand -> 'a -> text
val add : register -> register -> 'a operand -> 'a -> text
val sub : register -> register -> 'a operand -> 'a -> text
val mul : register -> register -> 'a operand -> 'a -> text
val rem : register -> register -> 'a operand -> 'a -> text
val div : register -> register -> text

(** As 5 operações aritméticas de base: [add rdst rsrc1 ospec o]
   alojar em rdst o resultado da operação entre rsrc1 e o. A
   constante ospec especifica se é um imediato, imediato de 32 bits
   ou um registo.
   Exemple:
   [add v0 v1 oreg v2]
   [div v0 v1 oi 424]
   [sub t0 a0 oi32 2147483647l]
 *)

(** {1 Operações lógicas } *)

val and_ : register -> register -> register -> text
val or_ : register -> register -> register -> text
val not_ : register -> register -> text
val clz : register -> register -> text
(** Operações de manipulação de bits. "e" bit a bit, "ou" bit a
    bit, "não" bit a bit e clz (count leading zero) *)


(** {1 Comparações } *)

val seq : register -> register -> 'a operand -> 'a -> text
val sne : register -> register -> 'a operand -> 'a -> text
val sge : register -> register -> 'a operand -> 'a -> text
val sgt : register -> register -> 'a operand -> 'a -> text
val sle : register -> register -> 'a operand -> 'a -> text
val slt : register -> register -> 'a operand -> 'a -> text
  (** a condicional [sop ra rb rc] coloca [ra] a 1 se [rb op rc] e a 0
      em caso contrário (eq : ==, ge : >=, gt : >, le : <=, lt : <=,
      ne : !=) *)

(** {1 Saltos } *)

val b : label -> text
(** salto incondicional *)

val beq : register -> register -> label -> text
val bne : register -> register -> label -> text
val bge : register -> register -> label -> text
val bgt : register -> register -> label -> text
val ble : register -> register -> label -> text
val blt : register -> register -> label -> text
val mflo : register -> text
(** [bop ra rb label] salta para a etiqueta [label] se [ra op rb] *)

val beqz : register -> label -> text
val bnez : register -> label -> text
val bgez : register -> label -> text
val bgtz : register -> label -> text
val blez : register ->  label -> text
val bltz : register ->  label -> text
(** [bopz ra rb label] salta para a etiqueta [label] se [ra op 0] *)

val jr : register -> text
(** [jr r] Continua a execução no endereço especificado no registo [r] *)
val jal : label -> text
(** [jal l] Continua a execução no endereço especificado pela etiqueta [l],
    sauve l'adresse de retour dans $ra.
*)
val jalr : register -> text
(** [jalr r] Continue l'exécution à l'adresse spécifiée par le
    registre [r], guardando o endereço de retorno em $ra.
*)

(** {1 Leitura / escrita em memória } *)

type 'a address
(** tipo abstrato que representa os endereços *)

val alab : label address
val areg : (int * register) address
(** Os endereços são dados por uma etiqueta ou por um par
    offset, registo *)

val j : 'a address -> 'a -> text
val la : register -> 'a address -> 'a -> text
(** [la reg alab "foo"] carrega para [reg] o endereço da etiqueta "foo"
    [la reg1 areg (x, reg2)] carrega para [reg1] o endereço contido em
    [reg2] com offset de [x] octetos
 *)

val lbu : register -> 'a address -> 'a -> text
(** carregar o octeto para o endereço dado sem extensão de sinal (valor
    entre 0 e 255) *)
val lw : register -> 'a address -> 'a -> text
(** carregar o inteiro de 32bits para o endereço dado *)
val sb : register -> 'a address -> 'a -> text
(** escrever os 8 bits menor significância do registo dado pelo endereço
    dado *)
val sw : register -> 'a address -> 'a -> text
(** escrever o conteúdo do registo para o endereço dado *)
val move : register -> register -> text

(** {1 Diversos } *)

val nop : [> ] asm
(** a instrução vazia. Pode estar em texto ou em dados *)

val label : label ->  [> ] asm
(** uma etiqueta. Pode encontrar-se em texto ou dados *)

val syscall : text
(** a instrução syscall *)

val comment : string -> [> ] asm
(** colocar um comentário no código gerado. Pode encontrar-se em 
    texto ou dados *)

val align : int ->  [> ] asm
(** [align n] alinha o código após a instrução em 2^n octetos *)

val asciiz : string -> data
(** coloca uma cadeia de carateres constante (terminada par 0) na
    zona de dados *)

val dword : int list -> data
(** coloca uma lista de palavras de memória na zona de dados *)

val address : label list -> data
(** coloca uma lista de endereços (denotados pelas etiquetas) na zona
    de dados *)

val space: int -> data
(** [space n] aloca [n] octetos no segmento de dados *)

val inline: string -> [> ] asm
(** [inline s] copia a cadeia [s] tal e qual como está para o ficheiro assembly *)

val ( ++ ) : ([< `text|`data ] asm as 'a)-> 'a -> 'a
(** concatena duas extremidades de códigos (quer texto com text, quer dados com
    dados) *)

(** {1 Manipulação da pilha} *)

val push : register -> text
(** [push r] coloca o conteúdo de [r] no topo da pilha.
    Nota : $sp aponta para o endereço do úlimto bloco ocupado *)

val pop : register -> text
(** [pop r] coloca a palavra no topo da pilha em [r] removendo-a da pilha *)

val popn: int -> text
(** [popn n] remove [n] octetos do topo da pilha *)

val peek : register -> text
(** [peek r] coloca a palavra no topo da pilha em [r] sem a remover da pilha *)