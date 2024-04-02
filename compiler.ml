open Mips
open Ast.IR2
open Ast.V2
open Semantics

module Env = Map.Make(String)
let dataEnv = ref Env.empty

let labelCount = ref 0

let newLabel prefix =
  incr labelCount;
  Printf.sprintf "%s%d" prefix !labelCount

let compileStringInput loc =
  let label = newLabel "input_string" in
  dataEnv := Env.add label (Space (string_of_int 1024)) !dataEnv;
  [ Li (V0, 8); La (A0, Lbl label); Li (A1, 100); Syscall;  Sw (A0, loc)], (label, Space (string_of_int 1024))

type var_info = { loc: Mips.loc; typ: Ast.type_t }

type cinfo = { code: Mips.instr list
             ; env: var_info Env.t
             ; fpo: int
             ; mutable counter: int
             ; return: string }

let compile_value v =
  match v with
  | Void    -> [ Li (V0, 0) ]
  | Bool b -> [ Li (V0, if b then 1 else 0) ]
  | Int n  -> [ Li (V0, n) ]
  | Data d  -> [ La (V0, Lbl d) ]

let rec compile_expr e env =
  match e with
  | Value v -> compile_value v
  | Var v   -> [ Lw (V0, (Env.find v env).loc) ]
  | Call (f, args) ->
     let ca = List.rev_map (fun a ->
                  compile_expr a env
                  @ [ Addi (SP, SP, -4)
                    ; Sw (V0, Mem (SP, 0)) ])
                args in
     List.flatten ca
     @ [ Jal f
       ; Addi (SP, SP, 4 * (List.length args)) ]

let rec compile_instr i info =
  match i with
  | Decl (v, t) ->
     { info with
       env = Env.add v { loc = Mem (FP, -info.fpo); typ = t } info.env
     ; fpo = info.fpo + 4 }
  | Return e ->
     { info with
       code = info.code
              @ compile_expr e info.env
              @ [ B info.return ] }
  | Expr e ->
     { info with
       code = info.code
              @ compile_expr e info.env }
  | Assign (lv, e) ->
     { info with
       code = info.code
              @ compile_expr e info.env @ [ Sw (V0, (Env.find lv info.env).loc) ] }
  | Cond (c, t, e) ->
     let uniq = string_of_int info.counter in
     let ct = compile_block t { info with code = []
                                        ; counter = info.counter + 1 } in
     let ce = compile_block e { info with code = []
                                        ; counter = ct.counter + 1 } in
     { info with
       code = info.code
              @ compile_expr c info.env
              @ [ Beqz (V0, "else" ^ uniq) ]
              @ ct.code
              @ [ B ("endif" ^ uniq)
                ; Label ("else" ^ uniq) ]
              @ ce.code
              @ [ Label ("endif" ^ uniq) ]
     ; counter = ce.counter }

  | Loop (c, b) ->
    let uniq = string_of_int info.counter in
    let compiled_expr = compile_expr c info.env in
    let cb = compile_block b { info with code = [] ; counter = info.counter + 1 } in
    { info with 
        code = info.code @ [ Label ("while" ^ uniq) ] @ compiled_expr @ [ Beqz (V0, "endwhile" ^ uniq) ] @ cb.code @ [ B ("while" ^ uniq) ; Label ("endwhile" ^ uniq) ] ; 
        counter = cb.counter 
    }
     
  | Print p -> 
  let instrs = List.concat (List.map (fun expr -> 
    match expr with
    | Value v -> 
      (match v with
      | Int n -> [Li (A0, n); Li (V0, 1); Syscall]
      | Data s -> [La (A0, Lbl s); Li (V0, 4); Syscall]
      | Bool b -> [La (A0, if b then Lbl "true" else Lbl "false"); Li (V0, 4); Syscall]
      | _ -> []
      )
    | Var v ->
      let var_info = Env.find v info.env in
      (match var_info.typ with
      | Ast.Int_t ->
        [ Lw (A0, var_info.loc)
        ; Li (V0, 1)
        ; Syscall ]
      | Ast.Str_t ->
        [ Lw (A0, var_info.loc)
        ; Li (V0, 4)
        ; Syscall ]
      | Ast.Bool_t -> 
          let bool_true = "bool_true" ^ string_of_int info.counter
          and bool_false = "bool_false" ^ string_of_int info.counter
          and bool_return = "bool_return" ^ string_of_int info.counter in
          info.counter <- info.counter + 1;
          [ Lw (T0, var_info.loc); 
            Li (T1, 1); 
            Beq (T0, T1, bool_true); 
            Bne (T0, T1, bool_false); 
            Label bool_true;
            La (A0, Lbl "true"); 
            Li (V0, 4);
            Syscall;
            J bool_return;
            Label bool_false;
            La (A0, Lbl "false"); 
            Li (V0, 4);
            Syscall;
            J bool_return;
            Label bool_return ]
      | _ -> []
      )
    | Call (f, args) -> 
    if not (Hashtbl.mem !func_type f) then
      let ca = List.rev_map (fun a -> 
        let instrs = compile_expr a info.env in 
        instrs @ [ Addi (SP, SP, -4); Sw (V0, Mem (SP, 0))]) args in 
      List.flatten ca @ [ Jal f; Addi (SP, SP, 4 * (List.length args)); Move (A0, V0); Li (V0, 1); Syscall ]
    else
      match Hashtbl.find !func_type f with
      | Ast.Int_t ->
        let ca = List.rev_map (fun a -> 
          let instrs = compile_expr a info.env in 
          instrs @ [ Addi (SP, SP, -4); Sw (V0, Mem (SP, 0))]) args in 
        List.flatten ca @ [ Jal f; Addi (SP, SP, 4 * (List.length args)); Move (A0, V0); Li (V0, 1); Syscall ]
      | Ast.Str_t ->
        let ca = List.rev_map (fun a -> 
          let instrs = compile_expr a info.env in 
          instrs @ [ Addi (SP, SP, -4); Sw (V0, Mem (SP, 0))]) args in 
        List.flatten ca @ [ Jal f; Addi (SP, SP, 4 * (List.length args)); Move (A0, V0); Li (V0, 4); Syscall ]
      | Ast.Bool_t ->
        let ca = List.rev_map (fun a -> 
          let instrs = compile_expr a info.env in 
          instrs @ [ Addi (SP, SP, -4); Sw (V0, Mem (SP, 0))]) args in 
        let bool_true = "bool_true" ^ string_of_int info.counter
        and bool_false = "bool_false" ^ string_of_int info.counter
        and bool_return = "bool_return" ^ string_of_int info.counter in
        info.counter <- info.counter + 1;
        List.flatten ca @ 
        [ Jal f; 
          Addi (SP, SP, 4 * (List.length args)); 
          Move (T0, V0); 
          Li (T1, 1); 
          Beq (T0, T1, bool_true); 
          Bne (T0, T1, bool_false); 
          Label bool_true;
          La (A0, Lbl "true"); 
          Li (V0, 4);
          Syscall;
          J bool_return;
          Label bool_false;
          La (A0, Lbl "false"); 
          Li (V0, 4);
          Syscall;
          J bool_return;
          Label bool_return ]
      | _ -> []
  ) p) in 
  { info with code = info.code @ instrs }

  | Input v -> 
    let var_info = Env.find v info.env in
    let instrs, dataInput = match var_info.typ with
      | Int_t -> [ Li (V0, 5); Syscall; Sw (V0, var_info.loc) ], []
      | Bool_t -> [ Li (V0, 5); Syscall; Li (T0, 1); Seq (V0, V0, T0); Sw (V0, var_info.loc) ], []
      | Str_t -> let instr_list, dataInput = compileStringInput var_info.loc in instr_list, [dataInput]
      | _ -> failwith "Invalid type for input"
    in
    List.iter (fun (k, v) -> dataEnv := Env.add k v !dataEnv) dataInput;
    { info with code = info.code @ instrs }

  | EmptyInput -> 
    let instrs = [ Li (V0, 5); Syscall ] in
    { info with code = info.code @ instrs }


and compile_block b info =
  match b with
  | [] -> info
  | i :: r ->
     compile_block r (compile_instr i info)

let compile_def (Func (name, args, b)) counter =
  let cb = compile_block b
             { code = []
             ; env =  List.fold_left
                        (fun e (i, a) -> 
                          let arg_type = get_arg_type name a in
                          match arg_type with
                          | Some t -> Env.add a { loc = Mem (FP, 4 * i); typ = t } e
                          | None -> e)
                        Env.empty (List.mapi (fun i a -> i + 1, a) args)
             ; fpo = 8
             ; counter = counter + 1
             ; return = "ret" ^ (string_of_int counter) }
  in cb.counter,
     []
     @ [ NewLine
       ; Label name
       ; Addi (SP, SP, -cb.fpo)
       ; Sw (RA, Mem (SP, cb.fpo - 4))
       ; Sw (FP, Mem (SP, cb.fpo - 8))
       ; Addi (FP, SP, cb.fpo - 4) ]
     @ cb.code
     @ [ Label cb.return
       ; Addi (SP, SP, cb.fpo)
       ; Lw (RA, Mem (FP, 0))
       ; Lw (FP, Mem (FP, -4))
       ; Jr (RA) ]

let rec compile_prog p counter =
  match p with
  | [] -> []
  | d :: r ->
     let new_counter, cd = compile_def d counter in
     cd @ (compile_prog r new_counter)

let compile (code, data) =
  let text = Baselib.builtins @ compile_prog code 0 in
  let dataEnv_values = Env.fold (fun k v acc -> (k, v) :: acc) !dataEnv [] in
  { text = text
  ; data = List.map (fun (l, s) -> (l, Asciiz s)) data @ dataEnv_values }

