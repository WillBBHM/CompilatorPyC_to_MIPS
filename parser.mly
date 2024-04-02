%{
  (*open Ast*)
  open Ast.Syntax
%}

%token <int> Lint
%token <bool> Lbool
%token <string> Lstr
%token <Ast.type_t> Ltype
%token <string> Lvar
%token Lassign Lreturn
%token Lparen Rparen
%token Lprint Linput
%token Lvirgule
%token Lif Lelse Lwhile
%token Laccolad Raccolad
%token Lgt Llt Lge Lle
%token Lnoteq Liseq Lxor Lshr Lshl Lnot Lor Land Lmod Ldiv Lmult Lsub Ladd Lsc Lend

%left Lor
%left Lxor Land Lshr Lshl
%left Liseq Lnoteq Lgt Llt Lge Lle
%left Ladd Lsub
%left Lmult Ldiv Lmod
%right Lnot

%start prog

%type <Ast.Syntax.prog> prog

%%
prog:
  | f = def ; b = prog { f @ b }
  | Lend { [] }

def:
  | t = Ltype
  ; f = Lvar
  ; Lparen
  ; a = separated_list(Lvirgule, arg)
  ; Rparen
  ; b = block {
    [ Func { func = f ; type_t = t ; args = a ; code = b ; pos = $startpos(f) } ]
  }

arg:
  | t = Ltype ; v = Lvar { Arg { type_t = t ; name = v } }

block:
  | Laccolad ; b = block { b }
  | i = instr ; b = block { i @ b }
  | Raccolad { [] }
;

instr:
| e = expr; Lsc { [Expr { expr = e; pos = $startpos }] }

| Lreturn ; e = expr ; Lsc { [ Return { expr = e ; pos = $startpos } ] }

| Lreturn ; Lsc {
  [ Return { expr = Value { value = Void; pos = $startpos($2) }
            ; pos = $startpos } ]
  }

| t = Ltype ; v = Lvar ; Lsc {
  [ Decl { name = v ; type_t = t ; pos = $startpos } ]
}

| t = Ltype ; v = Lvar ; Lassign ; e = expr ; Lsc
  { [ Decl { name = v ; type_t = t ; pos = $startpos(v) }
  ; Assign { var = v ; expr = e ; pos = $startpos(e) } ]
  }

| v = Lvar ; Lassign ; e = expr ; Lsc {
  [ Assign { var = v ; expr = e ; pos = $startpos($2) } ]
}

| Lif ; Lparen ; e = expr ; Rparen ; b1 = block ; Lelse ; b2 = block {
  [ Cond { expr = e ; if_b = b1 ; else_b = b2 ; pos = $startpos } ]
}

| Lif ; Lparen ; e = expr ; Rparen ; b = block {
  [ Cond { expr = e ; if_b = b ; else_b = [] ; pos = $startpos } ]
}

| Lprint; Lparen; el = expr_list; Rparen; Lsc { [Print { args = el; pos = $startpos }] }
| Lprint; Lparen; Rparen; Lsc { [Print { args = []; pos = $startpos }] }
| Linput; Lparen; v = Lvar; Rparen; Lsc { [Input { var = v; pos = $startpos }] } 
| Linput; Lparen; Rparen; Lsc { [ EmptyInput ] }
| Lwhile; Lparen ; e = expr; Rparen ; b = block { [Loop { cond = e; block = b; pos = $startpos(e) }] }


expr_list:
| e = expr { [e] }
| e = expr; Lvirgule; el = expr_list { e :: el }
;

expr:
| n = Lint {
  Value { value = Int n; pos = $startpos(n) }
}
| b = Lbool {
  Value { value = Bool b; pos = $startpos(b) }
}
| s = Lstr {
  Value { value = Str s; pos = $startpos(s) }
}
| v = Lvar {
  Var { name = v ; pos = $startpos }
}
| Lparen; e = expr; Rparen { e }

| left = expr; Ladd; right = expr { Call { func = "_add"; args = [left; right]; pos = $startpos($2) } }
| left = expr; Lsub; right = expr { Call { func = "_sub"; args = [left; right]; pos = $startpos($2) } }
| left = expr; Lmult; right = expr { Call { func = "_mul"; args = [left; right]; pos = $startpos($2) } }
| left = expr; Ldiv; right = expr { Call { func = "_div"; args = [left; right]; pos = $startpos($2) } }
| left = expr; Lmod; right = expr { Call { func = "_mod"; args = [left; right]; pos = $startpos($2) } }
| left = expr; Land; right = expr { Call { func = "_and"; args = [left; right]; pos = $startpos($2) } }
| left = expr; Lor; right = expr { Call { func = "_or"; args = [left; right]; pos = $startpos($2) } }
| left = expr; Lxor; right = expr { Call { func = "_xor"; args = [left; right]; pos = $startpos($2) } }
| left = expr; Liseq; right = expr { Call { func = "_eq"; args = [left; right]; pos = $startpos($2) } }
| left = expr; Lnoteq; right = expr { Call { func = "_neq"; args = [left; right]; pos = $startpos($2) } }
| left = expr; Lgt; right = expr { Call { func = "_gt"; args = [left; right]; pos = $startpos($2) } }
| left = expr; Llt; right = expr { Call { func = "_lt"; args = [left; right]; pos = $startpos($2) } }
| left = expr; Lge; right = expr { Call { func = "_ge"; args = [left; right]; pos = $startpos($2) } }
| left = expr; Lle; right = expr { Call { func = "_le"; args = [left; right]; pos = $startpos($2) } }
| left = expr; Lshr; right = expr { Call { func = "_shr"; args = [left; right]; pos = $startpos($2) } }
| left = expr; Lshl; right = expr { Call { func = "_shl"; args = [left; right]; pos = $startpos($2) } }
| Lnot; right = expr { Call { func = "_not"; args = [right]; pos = $startpos($1) } }


| f = Lvar ; Lparen ; a = separated_list(Lvirgule, expr) ; Rparen {
    Call { func = f ; args = a ; pos = $startpos }
  }
;
