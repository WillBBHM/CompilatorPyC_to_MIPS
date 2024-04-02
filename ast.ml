type type_t =
  | Bool_t
  | Int_t
  | Str_t
  | Void_t
  | Lvar_t
  | Func_t of type_t * type_t list

let rec string_of_type_t t =
  match t with
  | Int_t  -> "int"
  | Bool_t -> "bool"
  | Str_t  -> "string"
  | Void_t -> "void"
  | Lvar_t -> "var"
  | Func_t (r, a) ->
     (if (List.length a) > 1 then "(" else "")
     ^ (String.concat ", " (List.map string_of_type_t a))
     ^ (if (List.length a) > 1 then ")" else "")
     ^ " -> " ^ (string_of_type_t r)

module Syntax = struct
  type ident = string
  type value =
    | Void
    | Bool of bool
    | Int  of int
    | Str of string
  type expr =
    | Value of { value: value
               ; pos: Lexing.position }
    | Var   of { name: ident
               ; pos: Lexing.position }
    | Call  of { func: ident
               ; args: expr list
               ; pos: Lexing.position }

  type instr =
    | EmptyInput
    | Decl   of { name: ident
                ; type_t: type_t
                ; pos: Lexing.position }
    | Assign of { var: ident
                ; expr: expr
                ; pos: Lexing.position }
    | Print  of { args: expr list
                ; pos: Lexing.position }
    | Input  of { var : string
                ; pos : Lexing.position }
    | Cond   of { expr : expr; if_b : block
                ; else_b : block
                ; pos : Lexing.position }
    | Expr   of { expr : expr
                ; pos : Lexing.position }     
    | Return of {expr : expr
                ; pos : Lexing.position }
    | Loop   of {cond : expr; block: block
                ; pos : Lexing.position }
  and block = instr list

  type arg =
   | Arg of { type_t : type_t
            ; name : ident }

  type args = arg list

  type def =
    | Func of{ func : ident
             ; type_t : type_t
             ; args : args
             ; code : block
             ; pos : Lexing.position }

  type prog = def list
end

module type Parameters = sig
  type value
end

module V1 = struct
  type value =
    | Void
    | Bool of bool
    | Int  of int
    | Str  of string
end

module V2 = struct
  type value =
    | Void
    | Bool of bool
    | Int  of int
    | Data of string
end

module IR (P : Parameters) = struct
  type ident = string
  type expr =
    | Value of P.value
    | Var   of ident
    | Call  of ident * expr list

  type instr =
    | EmptyInput
    | Decl   of ident * type_t
    | Return of expr
    | Expr   of expr
    | Assign of ident * expr
    | Print  of expr list
    | Input  of string
    | Cond   of expr * block * block
    | Loop   of expr * block
  and block = instr list

  type def =
    | Func of ident * ident list * block

  type prog = def list
end

module IR1 = IR(V1)
module IR2 = IR(V2)
