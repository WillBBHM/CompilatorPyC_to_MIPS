open Ast
open Ast.IR1
open Ast.V1
open Baselib

exception Error of string * Lexing.position

let vars_data : (string, string) Hashtbl.t ref = ref (Hashtbl.create 123456)
let func_arg_types : (string, (string, type_t) Hashtbl.t) Hashtbl.t = Hashtbl.create 123456
let func_type : (string, type_t) Hashtbl.t ref = ref (Hashtbl.create 123456)

let add_func_type func_name func_type_value =
  Hashtbl.add !func_type func_name func_type_value

let get_func_type func_name =
  if Hashtbl.mem !func_type func_name then
    Some (Hashtbl.find !func_type func_name)
  else
    None

let add_func_arg_type func_name arg_name arg_type =
  let arg_types =
    if Hashtbl.mem func_arg_types func_name then
      Hashtbl.find func_arg_types func_name
    else
      let new_arg_types = Hashtbl.create 123456 in
      Hashtbl.add func_arg_types func_name new_arg_types;
      new_arg_types
  in
  Hashtbl.add arg_types arg_name arg_type

let get_arg_type func_name arg_name =
  if Hashtbl.mem func_arg_types func_name then
    let arg_types = Hashtbl.find func_arg_types func_name in
    if Hashtbl.mem arg_types arg_name then
      Some (Hashtbl.find arg_types arg_name)
    else
      None
  else
    None

let expr_pos expr =
  match expr with
  | Syntax.Value v  -> v.pos
  | Syntax.Var v  -> v.pos
  | Syntax.Call c -> c.pos

let analyze_value = function
  | Syntax.Void -> Void, Void_t
  | Syntax.Int n -> Int n, Int_t
  | Syntax.Bool b -> Bool b, Bool_t
  | Syntax.Str s -> Str s, Str_t

let errt expected given pos =
  raise (Error (Printf.sprintf "expected %s but given %s"
                  (string_of_type_t expected)
                  (string_of_type_t given),
                pos))

let rec analyze_expr expr env =
  match expr with
  | Syntax.Value v -> 
    let value, type_t = analyze_value v.value in
    Value value, type_t
  | Syntax.Var v ->
  if (Env.mem v.name env) then
    Var v.name, (Env.find v.name env)
  else
    raise (Error (Printf.sprintf "unbound variable '%s'" v.name,
                  v.pos))
  | Syntax.Call c ->
     match Env.find_opt c.func env with
     | Some (Func_t (rt, at)) ->
        if List.length at != List.length c.args then
          raise (Error (Printf.sprintf "expected %d arguments but given %d"
                          (List.length at) (List.length c.args), c.pos)) ;
        let args = List.map2 (fun eat a -> let aa, at = analyze_expr a env
                                           in if at = eat then aa
                                              else errt eat at (expr_pos a))
                     at c.args in
        Call (c.func, args), rt
     | Some _ -> raise (Error (Printf.sprintf "'%s' is not a function" c.func,
                               c.pos))
     | None -> raise (Error (Printf.sprintf "undefined function '%s'" c.func,
                             c.pos))

let rec analyze_instr instr env =
  match instr with
  | Syntax.EmptyInput -> (EmptyInput, env)
  | Syntax.Expr e ->
     let ae, _ = analyze_expr e.expr env in
     Expr ae, env
  | Syntax.Return r ->
     let ae, _ = analyze_expr r.expr env in
     Return ae, env
  | Syntax.Assign a -> 
    let ae, ae_type = analyze_expr a.expr env in 
    let var_type = Env.find a.var env in
    if var_type = ae_type then
      (Assign (a.var, ae), env)
    else
      raise (Error (Printf.sprintf "Type mismatch: variable '%s' of type '%s' cannot be assigned an expression of type '%s'" a.var (string_of_type_t var_type) (string_of_type_t ae_type), a.pos))
  | Syntax.Decl d ->
     if Env.mem d.name env then
       raise (Error (Printf.sprintf "'%s' is already an existing variable" d.name,
                     d.pos))
     else
       Hashtbl.add !vars_data d.name (string_of_type_t d.type_t);
       Decl (d.name, d.type_t), Env.add d.name d.type_t env
  | Syntax.Print p  -> 
    let ap = List.map (fun expr -> 
      let (expr_res, _) = analyze_expr expr env in 
      expr_res) p.args in 
    (Print ap, env)
  | Syntax.Input i  ->
    let _ = analyze_expr (Syntax.Var { name = i.var; pos = i.pos }) env in
    (Input i.var, env)
  | Syntax.Cond c ->
    let ce, _ = analyze_expr c.expr env in
    let tb = List.map (fun i -> fst (analyze_instr i env)) c.if_b in
    let eb = List.map (fun i -> fst (analyze_instr i env)) c.else_b in
    (Cond (ce, tb, eb), env)
  | Syntax.Loop l ->
      let ce, _ = analyze_expr l.cond env in
      let b = List.map (fun i -> fst (analyze_instr i env)) l.block in
      (Loop (ce, b), env)

and analyze_block block env =
  match block with
  | [] -> []
  | instr :: rest ->
     let ai, new_env = analyze_instr instr env in
     ai :: (analyze_block rest new_env)

let analyze_def def env =
  match def with
  | Syntax.Func f ->
     match Env.find_opt f.func env with
     | Some (Func_t (_, _)) -> raise (Error (Printf.sprintf "'%s' is already an existing function" f.func,
                                               f.pos))
     | Some _ | None ->
        let avs = List.map (fun arg -> match arg with
                               | Syntax.Arg a -> a.name) f.args in
        let ats = List.map (fun arg -> match arg with
                               | Syntax.Arg a -> a.type_t) f.args in
        let new_env1 = List.fold_left2
                         (fun e a t -> Env.add a t e) env avs ats in
        let ab = analyze_block f.code new_env1 in
        let new_env2 = Env.add f.func (Func_t (f.type_t, ats)) env in
        List.iter2 (add_func_arg_type f.func) avs ats;
        add_func_type f.func f.type_t; 
        Func (f.func, avs, ab), new_env2

let rec analyze_prog prog env =
  match prog with
  | [] -> []
  | def :: rest ->
     let ad, new_env = analyze_def def env in
     ad :: (analyze_prog rest new_env)


let analyze parsed =
  analyze_prog parsed Baselib._types_
