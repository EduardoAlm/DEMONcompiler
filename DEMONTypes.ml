open DEMONAst
open Format
open List

let var = Hashtbl.create 12 

(* Excepção levantada para assinalar um erro durante a interpretação *)

type t = Int | Float | Bool 

exception Type_exception of t * t;; 
exception Type_exception2 of t ;;

let type_to_string e = 
     if e = Int then "int" else if e = Float then "float" else if e=Bool then "bool" else "not a recognised type"   

let rec type_expr = function
     Econst (I _) -> Int
    | Econst (F _) -> Float
    | Econst (B _) -> Bool 
    | Evar ( id ) -> type_expr(Hashtbl.find var id)
    | Unop (op, e) ->
        let t_e = type_expr(e) in
        begin match op, t_e with
             Not, Bool -> Bool
            | Not, _ -> raise(Type_exception (Bool , t_e) )
            | Minus, Int -> Int
            | Minus, Float -> Float
            | Times, _ -> raise(Type_exception2 (t_e))
            | Plus, _ -> raise(Type_exception2 (t_e))
            | Minus, _ -> raise(Type_exception2 (t_e))
            | Div, _ -> raise(Type_exception2 (t_e))
            | And, _ -> raise(Type_exception2 (t_e))
            | Or, _ -> raise(Type_exception2 (t_e))
            | Larger, _ -> raise(Type_exception2 (t_e))
            | Smaller, _ -> raise(Type_exception2 (t_e))
            | Lequal, _ -> raise(Type_exception2 (t_e))
            | Sequal, _ -> raise(Type_exception2 (t_e))
            | Equals, _ -> raise(Type_exception2 (t_e))
            | Notequal, _ -> raise(Type_exception2 (t_e))
        end    
    | Binop (op, e1, e2) ->         
        let t_e1 = type_expr e1 in
        let t_e2 = type_expr e2 in
        begin match t_e1, op, t_e2 with     
             Int, Plus, Int -> Int
            | Int, Plus, _ -> raise(Type_exception(t_e1, t_e2))
            | _ , Plus, Int -> raise(Type_exception(t_e1, t_e2))
            | Float, Plus, Float -> Float
            | Float, Plus, _ -> raise (Type_exception (t_e1, t_e2))
            | _ , Plus, Float -> raise (Type_exception (t_e1, t_e2))
            | _ , Plus, _ -> raise(Type_exception (t_e1, t_e2))
            | Int, Minus, Int -> Int
            | Int, Minus, _ -> raise (Type_exception (t_e1, t_e2))
            | _, Minus, Int -> raise (Type_exception (t_e1, t_e2))
            | Float, Minus, Float -> Float
            | Float, Minus, _ -> raise (Type_exception (t_e1, t_e2))
            | _ , Minus, Float -> raise (Type_exception (t_e1, t_e2))
            | _ , Minus, _ -> raise(Type_exception (t_e1, t_e2))
            | Int, Times, Int -> Int
            | Int, Times, _ -> raise (Type_exception (t_e1, t_e2))
            | _ , Times, Int -> raise (Type_exception (t_e1, t_e2))
            | Float, Times, Float -> Float
            | Float, Times, _ -> raise (Type_exception (t_e1, t_e2))
            | _ , Times, Float -> raise (Type_exception (t_e1, t_e2))
            | _ , Times, _ -> raise(Type_exception (t_e1, t_e2))
            | Int, Div, Int -> Int
            | Int, Div, _ -> raise (Type_exception (t_e1, t_e2))
            | _ , Div, Int -> raise (Type_exception (t_e1, t_e2))
            | Float, Div, Float -> Float
            | Float, Div, _ -> raise (Type_exception (t_e1, t_e2))
            | _ , Div, Float -> raise (Type_exception (t_e1, t_e2))
            | _ , Div, _ -> raise(Type_exception (t_e1, t_e2))
            | Int, Larger, Int -> Bool
            | Int, Larger, _ -> raise (Type_exception (t_e1, t_e2))
            | _ , Larger, Int -> raise (Type_exception (t_e1, t_e2))
            | Float, Larger, Float -> Bool
            | Float, Larger, _ -> raise (Type_exception (t_e1, t_e2))
            | _ , Larger, Float -> raise (Type_exception (t_e1, t_e2))
            | _ , Larger, _ -> raise(Type_exception (t_e1, t_e2))
            | Int, Smaller, Int -> Bool
            | Int, Smaller, _ -> raise (Type_exception (t_e1, t_e2))
            | _ , Smaller, Int -> raise (Type_exception (t_e1, t_e2))
            | Float, Smaller, Float -> Bool
            | Float, Smaller, _ -> raise (Type_exception (t_e1, t_e2))
            | _ , Smaller, Float -> raise (Type_exception (t_e1, t_e2))
            | _ , Smaller, _ -> raise(Type_exception (t_e1, t_e2))
            | Int, Sequal, Int -> Bool
            | Int, Sequal, _ -> raise (Type_exception (t_e1, t_e2))
            | _ , Sequal, Int -> raise (Type_exception (t_e1, t_e2))
            | Float, Sequal, Float -> Bool
            | Float, Sequal, _ -> raise (Type_exception (t_e1, t_e2))
            | _ , Sequal, Float -> raise (Type_exception (t_e1, t_e2))
            | _ , Sequal, _ -> raise(Type_exception (t_e1, t_e2))
            | Int, Lequal, Int -> Bool
            | Int, Lequal, _ -> raise (Type_exception (t_e1, t_e2))
            | _ , Lequal, Int -> raise (Type_exception (t_e1, t_e2))
            | Float, Lequal, Float -> Bool
            | Float, Lequal, _ -> raise (Type_exception (t_e1, t_e2))
            | _ , Lequal, Float -> raise (Type_exception (t_e1, t_e2))
            | _ , Lequal, _ -> raise(Type_exception (t_e1, t_e2))
            | Int, Equals, Int -> Bool
            | _ , Equals, Int -> raise (Type_exception (t_e1, t_e2))
            | Int , Equals, _ -> raise (Type_exception (t_e1, t_e2))
            | Float , Equals, Float -> Bool
            | _ , Equals, Float -> raise (Type_exception (t_e1, t_e2))
            | Float , Equals, _ -> raise (Type_exception (t_e1, t_e2))
            | _ , Equals, _ -> raise(Type_exception (t_e1, t_e2))
            | Int, Notequal, Int -> Bool
            | _ , Notequal, Int -> raise (Type_exception (t_e1, t_e2))
            | Int , Notequal, _ -> raise (Type_exception (t_e1, t_e2))
            | Float , Notequal, Float -> Bool
            | _ , Notequal, Float -> raise (Type_exception (t_e1, t_e2))
            | Float , Notequal, _ -> raise (Type_exception (t_e1, t_e2))
            | _ , Notequal, _ -> raise(Type_exception (t_e1, t_e2))
            | Bool, And, Bool -> Bool
            | Bool, And, _ -> raise(Type_exception (t_e1, t_e2))
            | _ , And, Bool -> raise(Type_exception (t_e1, t_e2))
            | _ , And, _ -> raise(Type_exception (t_e1, t_e2))
            | Bool, Or, Bool -> Bool
            | Bool, Or, _ -> raise(Type_exception (t_e1, t_e2))
            | _ , Or, Bool -> raise(Type_exception (t_e1, t_e2))
            | _ , Or, _ -> raise(Type_exception (t_e1, t_e2))
            | _ , Not, _ -> raise(Type_exception (t_e1, t_e2))
            
        end
    | Letin (id, e1, e2) -> 
        let t_e1 = type_expr e1 in
        let t_e2 = type_expr e2 in
        begin match id,t_e1, t_e2 with 
             string, Int, Int -> Int
            | string, Float, Float -> Float
            | string, Bool, Bool -> Bool
            | string, Int, Float -> Float
            | string, Float, Int -> Int 
            | string, Bool, Float -> Float
            | string, Bool, Int -> Int
            | string, Int, Bool -> Bool
            | string, Float, Bool -> Bool
            
        end

let rec type_stmt = function
     Setter (id, e) ->
        let t_e = type_expr e in
        begin match t_e with 
             Int -> Hashtbl.add var id e
            | Float -> Hashtbl.add var id e
            | Bool -> Hashtbl.add var id e
           (* | _ -> raise (Type_exception ("Not an Accepted Type")) *)
        end
    | Print (e) ->
        let t_e = type_expr e in 
        begin match t_e with
             Int -> () 
            | Float -> ()
            | Bool -> ()
        end
    | Sif ( e , sl ) ->
        let t_e = type_expr e in  
        let t_sl = type_prog sl in  
        begin match t_e, t_sl with 
            Bool, () -> () 
            | _ , () -> raise(Type_exception (Bool, t_e)) 
        end 
    | Sifelse ( e, sl1, sl2) ->
    let t_e = type_expr e in  
    let t_sl1 = type_prog sl1 in 
    let t_sl2 = type_prog sl2 in  
    begin match t_e, t_sl1, t_sl2 with 
        Bool, (), () -> ()
       | _ , () , () -> raise (Type_exception (Bool, t_e))    
    end 
    | Swhile (e, sl) -> 
    let t_e = type_expr e in  
    let t_sl = type_prog sl in  
    begin match t_e, t_sl with 
        Bool, () -> ()
        | _ , () -> raise(Type_exception (Bool, t_e))   
    end 
    | Sfor (id, e1, e2, sl) ->
    let t_e1 = type_expr e1 in  
    let t_e2 = type_expr e2 in  
    let t_sl = type_prog sl in  
    begin match id, t_e1, t_e2, t_sl with 
        string, Int, Int, () -> ()
        | string, Float, Float, () -> ()
        | string , _ , _ , () -> raise(Type_exception(t_e1, t_e2))   
    end 
    | Sfordt (id, e1, e2, sl) ->
    let t_e1 = type_expr e1 in  
    let t_e2 = type_expr e2 in  
    let t_sl = type_prog sl in  
    begin match id,t_e1, t_e2, t_sl with 
        string, Int, Int, () -> ()
        | string, Float, Float, () -> ()
        | string , _ , _ , () -> raise(Type_exception(t_e1, t_e2))     
    end 

and type_prog sl = 
    match sl with
    |[] -> ()
    | hd::tl -> type_stmt(hd); type_prog tl   
    
   
        