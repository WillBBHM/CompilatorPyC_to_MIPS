type reg =
  | Zero
  | V0
  | V1
  | A0
  | A1
  | A2
  | A3
  | T0
  | T1
  | T2
  | T3
  | T4
  | T5
  | T6
  | T7
  | T8
  | T9
  | S0
  | S1
  | S2
  | S3
  | S4
  | S5
  | S6
  | S7
  | GP
  | SP
  | FP
  | RA

type label = string

type loc =
  | Lbl of label
  | Reg of reg
  | Mem of reg * int

type instr =
  | Label of label
  | Nop
  | Syscall
  | B     of label
  | Add   of reg * reg * reg
  | Sub   of reg * reg * reg
  | Addi  of reg * reg * int
  | Addu  of reg * reg * reg
  | Subu  of reg * reg * reg
  | Addiu of reg * reg * int
  | Mult  of reg * reg
  | Div   of reg * reg * reg
  | Multu of reg * reg
  | Divu  of reg * reg
  | Mfhi  of reg
  | Mflo  of reg
  | Mul   of reg * reg * reg
  | Not   of reg * reg
  | And   of reg * reg * reg
  | Or    of reg * reg * reg
  | Nor   of reg * reg * reg
  | Xor   of reg * reg * reg
  | Andi  of reg * reg * int
  | Ori   of reg * reg * int
  | Xori  of reg * reg * int
  | Sll   of reg * reg * reg
  | Srl   of reg * reg * reg
  | Sra   of reg * reg * int
  | Sllv  of reg * reg * reg
  | Srlv  of reg * reg * reg
  | Srav  of reg * reg * reg
  | Slt   of reg * reg * reg
  | Sltu  of reg * reg * reg
  | Slti  of reg * reg * int
  | Sltiu of reg * reg * int
  | Seq   of reg * reg * reg
  | J     of label
  | Jal   of label
  | Jr    of reg
  | Jalr  of reg
  | Lui   of reg * int
  | Lb    of reg * loc
  | Lbu   of reg * loc
  | Lh    of reg * loc
  | Lhu   of reg * loc
  | Lw    of reg * loc
  | Sb    of reg * loc
  | Sh    of reg * loc
  | Sw    of reg * loc
  | Ll    of reg * loc
  | Sc    of reg * loc
  | Beq   of reg * reg * label
  | Bge   of reg * reg * label
  | Bgt   of reg * reg * label
  | Ble   of reg * reg * label
  | Blt   of reg * reg * label
  | Bne   of reg * reg * label
  | Bnez  of reg * label
  | Beqz  of reg * label
  | Bgez  of reg * label
  | Bgtz  of reg * label
  | Blez  of reg * label
  | Bltz  of reg * label
  | La    of reg * loc
  | Li    of reg * int
  | Move  of reg * reg
  | Lhi   of reg * int
  | Lho   of reg * int
  | Mthi  of reg
  | Mtlo  of reg
  | Main
  | NewLine

type directive =
  | Asciiz of string
  | Space  of string

type decl = label * directive

type asm = { text: instr list ; data: decl list }

module Syscall = struct
  let print_int    = 1
  let print_float  = 2
  let print_double = 3
  let print_str    = 4
  let read_int     = 5
  let read_float   = 6
  let read_double  = 7
  let read_str     = 8
  let sbrk         = 9
  let exit         = 10
  let print_char   = 11
  let read_char    = 12
  let open_file    = 13
  let read_file    = 14
  let write_file   = 15
  let close_file   = 16
  let exit2        = 17
end

let ps = Printf.sprintf (* alias raccourci *)

let fmt_reg = function
  | Zero -> "$zero"
  | V0   -> "$v0"
  | V1   -> "$v1"
  | A0   -> "$a0"
  | A1   -> "$a1"
  | A2   -> "$a2"
  | A3   -> "$a3"
  | T0   -> "$t0"
  | T1   -> "$t1"
  | T2   -> "$t2"
  | T3   -> "$t3"
  | T4   -> "$t4"
  | T5   -> "$t5"
  | T6   -> "$t6"
  | T7   -> "$t7"
  | T8   -> "$t8"
  | T9   -> "$t9"
  | S0   -> "$s0"
  | S1   -> "$s1"
  | S2   -> "$s2"
  | S3   -> "$s3"
  | S4   -> "$s4"
  | S5   -> "$s5"
  | S6   -> "$s6"
  | S7   -> "$s7"
  | GP   -> "$gp"
  | SP   -> "$sp"
  | FP   -> "$fp"
  | RA   -> "$ra"

let fmt_loc = function
  | Lbl (l)    -> l
  | Reg (r)    -> fmt_reg r
  | Mem (r, o) -> ps "%d(%s)" o (fmt_reg r)

let fmt_instr = function
  | Label (l)         -> ps "%s:" l
  | Nop               -> ps "  nop"
  | Main              -> ps "\n\nmain:"
  | NewLine           -> ps "\n\n"
  | Syscall           -> ps "  syscall"
  | B (l)             -> ps "  b %s" l
  | Add (rd, rs, rt)  -> ps "  add %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Sub (rd, rs, rt)  -> ps "  sub %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Addi (rt, rs, i)  -> ps "  addi %s, %s, %d" (fmt_reg rt) (fmt_reg rs) i
  | Addu (rd, rs, rt) -> ps "  addu %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Subu (rd, rs, rt) -> ps "  subu %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Addiu (rt, rs, i) -> ps "  addiu %s, %s, %d" (fmt_reg rt) (fmt_reg rs) i
  | Mult (rs, rt)     -> ps "  mult %s, %s" (fmt_reg rs) (fmt_reg rt)
  | Div (rd, rs, rt)  -> ps "  div %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Multu (rs, rt)    -> ps "  multu %s, %s" (fmt_reg rs) (fmt_reg rt)
  | Not (rs, rt)      -> ps "  not %s, %s" (fmt_reg rs) (fmt_reg rt)
  | Mul (rd, rs, rt)  -> ps "  mul %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Divu (rs, rt)     -> ps "  divu %s, %s" (fmt_reg rs) (fmt_reg rt)
  | Mfhi (rd)         -> ps "  mfhi %s" (fmt_reg rd)
  | Mflo (rd)         -> ps "  mflo %s" (fmt_reg rd)
  | And (rd, rs, rt)  -> ps "  and %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Or (rd, rs, rt)   -> ps "  or %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Nor (rd, rs, rt)  -> ps "  nor %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Xor (rd, rs, rt)  -> ps "  xor %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Andi (rt, rs, i)  -> ps "  andi %s, %s, %d" (fmt_reg rt) (fmt_reg rs) i
  | Ori (rt, rs, i)   -> ps "  ori %s, %s, %d" (fmt_reg rt) (fmt_reg rs) i 
  | Xori (rd, rs, i)  -> ps "  xori %s, %s, %d" (fmt_reg rd) (fmt_reg rs) i
  | Sll (rd, rt, rs)  -> ps "  sll %s, %s, %s" (fmt_reg rd) (fmt_reg rt) (fmt_reg rs)
  | Srl (rd, rt, rs)  -> ps "  srl %s, %s, %s" (fmt_reg rd) (fmt_reg rt) (fmt_reg rs)
  | Sra (rd, rt, i)   -> ps "  sra %s, %s, %d" (fmt_reg rd) (fmt_reg rt) i
  | Sllv (rd, rt, rs) -> ps "  sllv %s, %s, %s" (fmt_reg rd) (fmt_reg rt) (fmt_reg rs)
  | Srlv (rd, rt, rs) -> ps "  srlv %s, %s, %s" (fmt_reg rd) (fmt_reg rt) (fmt_reg rs)
  | Seq (rd, rt, rs)  -> ps "  seq %s, %s, %s" (fmt_reg rd) (fmt_reg rt) (fmt_reg rs)
  | Srav (rd, rt, rs) -> ps "  srav %s, %s, %s" (fmt_reg rd) (fmt_reg rt) (fmt_reg rs)
  | Slt (rd, rs, rt)  -> ps "  slt %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Sltu (rd, rs, rt) -> ps "  sltu %s, %s, %s" (fmt_reg rd) (fmt_reg rs) (fmt_reg rt)
  | Slti (rt, rs, i)  -> ps "  slti %s, %s, %d" (fmt_reg rt) (fmt_reg rs) i
  | Sltiu (rt, rs, i) -> ps "  sltiu %s, %s, %d" (fmt_reg rt) (fmt_reg rs) i
  | J (l)             -> ps "  j %s" l
  | Jal (l)           -> ps "  jal %s" l
  | Jr (rs)           -> ps "  jr %s" (fmt_reg rs)
  | Jalr (rs)         -> ps "  jalr %s" (fmt_reg rs)
  | Lui (rt, i)       -> ps "  lui %s, %d" (fmt_reg rt) i 
  | Lb (rt, a)        -> ps "  lb %s, %s" (fmt_reg rt) (fmt_loc a)
  | Lbu (rt, a)       -> ps "  lbu %s, %s" (fmt_reg rt) (fmt_loc a)
  | Lh (rt, a)        -> ps "  lh %s, %s" (fmt_reg rt) (fmt_loc a)
  | Lhu (rt, a)       -> ps "  lhu %s, %s" (fmt_reg rt) (fmt_loc a)
  | Lw (rt, a)        -> ps "  lw %s, %s" (fmt_reg rt) (fmt_loc a)
  | Sb (rt, a)        -> ps "  sb %s, %s" (fmt_reg rt) (fmt_loc a)
  | Sh (rt, a)        -> ps "  sh %s, %s" (fmt_reg rt) (fmt_loc a)
  | Sw (rt, a)        -> ps "  sw %s, %s" (fmt_reg rt) (fmt_loc a)
  | Ll (rt, a)        -> ps "  ll %s, %s" (fmt_reg rt) (fmt_loc a)
  | Sc (rt, a)        -> ps "  sc %s, %s" (fmt_reg rt) (fmt_loc a)
  | Beq (rt, rs, l)   -> ps "  beq %s, %s, %s" (fmt_reg rt) (fmt_reg rs) l
  | Bge (rt, rs, l)   -> ps "  bge %s, %s, %s" (fmt_reg rt) (fmt_reg rs) l
  | Bgt (rt, rs, l)   -> ps "  bgt %s, %s, %s" (fmt_reg rt) (fmt_reg rs) l
  | Ble (rt, rs, l)   -> ps "  ble %s, %s, %s" (fmt_reg rt) (fmt_reg rs) l
  | Blt (rt, rs, l)   -> ps "  blt %s, %s, %s" (fmt_reg rt) (fmt_reg rs) l
  | Bne (rt, rs, l)   -> ps "  bne %s, %s, %s" (fmt_reg rt) (fmt_reg rs) l
  | Bnez (rt, l)      -> ps "  bnez %s, %s" (fmt_reg rt) l
  | Beqz (rt, l)      -> ps "  beqz %s, %s" (fmt_reg rt) l
  | Bgez (rt, l)      -> ps "  bgez %s, %s" (fmt_reg rt) l
  | Bgtz (rt, l)      -> ps "  bgtz %s, %s" (fmt_reg rt) l
  | Blez (rt, l)      -> ps "  blez %s, %s" (fmt_reg rt) l
  | Bltz (rt, l)      -> ps "  bltz %s, %s" (fmt_reg rt) l
  | Li (rd, i)        -> ps "  li %s, %d" (fmt_reg rd) i
  | La (rd, a)        -> ps "  la %s, %s" (fmt_reg rd) (fmt_loc a)
  | Move (rd, rs)     -> ps "  move %s, %s" (fmt_reg rd) (fmt_reg rs)
  | Lhi (rt, i)       -> ps "  lhi %s, %d" (fmt_reg rt) i
  | Lho (rt, i)       -> ps "  lho %s, %d" (fmt_reg rt) i
  | Mthi (rs)         -> ps "  mthi %s" (fmt_reg rs)
  | Mtlo (rs)         -> ps "  mtlo %s" (fmt_reg rs)


let fmt_dir = function
  | Asciiz (s) -> ps ".asciiz \"%s\"" s
  | Space (s)  -> ps ".space %s" s

let emit oc asm =
  Printf.fprintf oc ".text\n.globl main\n\n" ;
  List.iter (fun i -> Printf.fprintf oc "%s\n" (fmt_instr i)) asm.text ;
  Printf.fprintf oc "  li $v0, 10\n  syscall" ;
  Printf.fprintf oc "\n\n.data\ntrue: .asciiz \"True\"\nfalse: .asciiz \"False\"\n";
  List.iter (fun (l, d) -> Printf.fprintf oc "%s: %s\n" l (fmt_dir d)) asm.data
