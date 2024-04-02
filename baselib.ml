open Ast
open Mips

module Env = Map.Make(String)

let _types_ =
  Env.of_seq
    (List.to_seq
       [ "_add", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_sub", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_mul", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_div", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_mod", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_eq", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_neq", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "puti", Func_t (Int_t, [ Int_t ])
       ; "geti", Func_t (Int_t, [ Int_t ])
       ; "puts", Func_t (Int_t, [ Int_t ])
       ; "_and", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_or", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_not", Func_t (Int_t, [ Int_t ])
       ; "_xor", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_gt", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_lt", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_ge", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_le", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_shr", Func_t (Int_t, [ Int_t ; Int_t ])
       ; "_shl", Func_t (Int_t, [ Int_t ; Int_t ])
    ])

let builtins =
  [
    Label "_add";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Add (V0, T0, T1);
    Jr RA;

    Label "_sub";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Sub (V0, T0, T1);
    Jr RA;

    Label "_mul";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Mul (V0, T0, T1);
    Jr RA;

    Label "_div";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Div (V0, T0, T1);
    Jr RA;

    Label "_mod";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Div (V0, T0, T1);
    Mfhi V0;
    Jr RA;

    Label "_eq";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Beq (T0, T1, "equal");
    Li (V0, 0);
    Jr RA;
    Label "equal";
    Li (V0, 1);
    Jr RA;

    Label "_neq";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Beq (T0, T1, "notequal");
    Li (V0, 1);
    Jr RA;
    Label "notequal";
    Li (V0, 0);
    Jr RA;

    Label "puti";
    Lw (A0, Mem (SP, 0));
    Li (V0, Syscall.print_int);
    Syscall;
    Jr RA;

    Label "geti";
    Lw (A0, Mem (SP, 0));
    Li (V0, Syscall.read_int);
    Syscall;
    Jr RA;

    Label "puts";
    Lw (A0, Mem (SP, 0));
    Li (V0, Syscall.print_str);
    Syscall;
    Jr RA;

    Label "_and";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    And (V0, T0, T1);
    Jr RA;

    Label "_or";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Or (V0, T0, T1);
    Jr RA;

    Label "_not";
    Lw (T0, Mem (SP, 0));
    Li (T1, 1);
    Xor (V0, T0, T1);
    Jr RA;

    Label "_xor";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Xor (V0, T0, T1);
    Jr RA;

    Label "_gt";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Bgt (T0, T1, "greater");
    Li (V0, 0);
    Jr RA;
    Label "greater";
    Li (V0, 1);
    Jr RA;

    Label "_lt";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Blt (T0, T1, "less");
    Li (V0, 0);
    Jr RA;
    Label "less";
    Li (V0, 1);
    Jr RA;

    Label "_ge";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Bge (T0, T1, "greater_equal");
    Li (V0, 0);
    Jr RA;
    Label "greater_equal";
    Li (V0, 1);
    Jr RA;

    Label "_le";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Ble (T0, T1, "less_equal");
    Li (V0, 0);
    Jr RA;
    Label "less_equal";
    Li (V0, 1);
    Jr RA;

    Label "_shr";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Srl (V0, T0, T1);
    Jr RA;

    Label "_shl";
    Lw (T0, Mem (SP, 0));
    Lw (T1, Mem (SP, 4));
    Sll (V0, T0, T1);
    Jr RA;
  ]
