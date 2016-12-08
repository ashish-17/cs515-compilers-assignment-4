open LibUtil

(* 
 * Parse and AST from a lexbuf 
 * - the filename is used to generate error messages
 *)
let parse (filename : string) (buf : Lexing.lexbuf) : Ast.prog =
  try
    Lexer.reset_lexbuf filename buf;
    Parser.toplevel Lexer.token buf
  with Parsing.Parse_error ->
    failwithf  "Parse error at %s." (Range.string_of_range (Lexer.lex_range buf))


(* 
 * Compile a source binop in to an LL instruction.
 *)
let compile_binop (b : Ast.binop) : Ll.uid -> Ll.operand -> Ll.operand -> Ll.insn  =
  let ib b id op1 op2 = (Ll.Binop (id, b, op1, op2)) in
  let ic c id op1 op2 = (Ll.Icmp (id, c, op1, op2)) in
  match b with
  | Ast.Plus  -> ib Ll.Add
  | Ast.Times -> ib Ll.Mul
  | Ast.Minus -> ib Ll.Sub
  | Ast.And   -> ib Ll.And
  | Ast.Or    -> ib Ll.Or
  | Ast.Shl   -> ib Ll.Shl
  | Ast.Shr   -> ib Ll.Lshr
  | Ast.Sar   -> ib Ll.Ashr

  | Ast.Eq    -> ic Ll.Eq
  | Ast.Neq   -> ic Ll.Ne
  | Ast.Lt    -> ic Ll.Slt
  | Ast.Lte   -> ic Ll.Sle
  | Ast.Gt    -> ic Ll.Sgt
  | Ast.Gte   -> ic Ll.Sge

let compile_unop (u : Ast.unop) (uid : Ll.uid) (op : Ll.operand) : Ll.insn list = 
    match u with
    | Ast.Neg -> [Ll.Binop(uid, Ll.Mul, Ll.Const(-1l), op)]
    | Ast.Lognot -> [Ll.Icmp(uid, Ll.Eq,  Ll.Const(0l), op)]
    | Ast.Not -> [Ll.Binop(uid, Ll.Mul, Ll.Const(-1l), op); Ll.Binop(uid, Ll.Sub, Ll.Const(-1l), Ll.Local(uid))]


let rec compile_exp (exp : Ast.exp) (uid : Ll.uid) (c : Ctxt.t) : Ll.insn list = 
    match exp with
    | Ast.Cint i -> [Ll.Load(uid, Ll.Const(i))]
    | Ast.Id id ->
            let var = Ctxt.lookup id c in
            [Ll.Load(uid, Ll.Local(var))]
    | Ast.Binop (b, exp1, exp2) ->
            let uid_exp1 = Ll.gen_sym() in
            let insn_exp1 = compile_exp exp1 uid_exp1 c in
            let uid_exp2 = Ll.gen_sym() in
            let insn_exp2 = compile_exp exp2 uid_exp2 c in
            let func = compile_binop b in
            let binop_insn = (func uid (Ll.Local uid_exp1) (Ll.Local uid_exp2)) in
            insn_exp1 @ insn_exp2 @ [binop_insn]
    | Ast.Unop (u, exp1) ->
            let uid_exp1 = Ll.gen_sym() in
            let insn_exp1 = compile_exp exp1 uid_exp1 c in
            insn_exp1 @ (compile_unop u uid (Ll.Local(uid_exp1)))  


let compile_var_decl (decl : Ast.var_decl) (c: Ctxt.t) : (Ctxt.t * Ll.insn list) =
    let new_var_data = Ctxt.alloc decl.Ast.v_id c in
    let exp_insn = compile_exp decl.Ast.v_init (snd new_var_data) c in
    (fst(new_var_data), [(Ll.Alloca (snd new_var_data))] @ exp_insn)

let compile_prog ((block, ret):Ast.prog) : Ll.prog =
failwith "unimplemented"
