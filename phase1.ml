open LibUtil

type elt =
  | I of Ll.insn
  | J of Ll.terminator
  | L of Ll.lbl

type stream = elt list

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

let rec compile_var_decls (decls : Ast.var_decl list) (c: Ctxt.t) (insns : Ll.insn list) : (Ctxt.t * Ll.insn list) = 
    match decls with
    | [] -> (c, insns)
    | decl :: rest -> 
            begin
                let ret = compile_var_decl decl c in
                compile_var_decls rest (fst ret) (insns @ snd ret)
            end

let rec compile_stmt (s : Ast.stmt) (c : Ctxt.t) : (Ctxt.t * stream) = 
    match s with
    | Ast.Assign (lhs, exp) ->
            begin
             match lhs with
             | Ast.Var id ->
                let uid_lhs = Ctxt.lookup id c in
                let insn_exp = compile_exp exp uid_lhs c in
                (c, List.fold_left (fun acc x -> acc @ [I(x)]) [] insn_exp)
            end
    | Ast.If (exp, stmt1, stmt2) ->
            begin
                let uid_exp1 = Ll.gen_sym() in
                let insn_exp = compile_exp exp uid_exp1 c in
                let if_stmt1_lbl = Ll.mk_lbl() in 
                let if_stmt2_lbl = Ll.mk_lbl() in 
                let if_exit_lbl = Ll.mk_lbl() in 
                let c_stmt1 = compile_stmt stmt1 c in
                let stream_stmt1 = snd c_stmt1 in
                let insn_cmp =
                    match stmt2 with
                    | None -> Ll.Cbr(Ll.Local(uid_exp1), if_stmt1_lbl, if_exit_lbl)
                    | Some x -> Ll.Cbr(Ll.Local(uid_exp1), if_stmt1_lbl, if_stmt2_lbl)
                in
                let if_exit_insn = Ll.Br(if_exit_lbl) in
                let stream_exp = List.fold_left (fun acc x -> acc @ [I(x)]) [] insn_exp in
                let stream_stmt = stream_exp @ [J(insn_cmp)] @ [L(if_stmt1_lbl)] @ stream_stmt1 @ [J(if_exit_insn)] in
                match stmt2 with
                    | None -> (c, stream_stmt @ [L(if_exit_lbl)])
                    | Some s2 -> 
                            let c_stmt2 = compile_stmt s2 c in
                            let stream_stmt2 = snd c_stmt2 in
                            (c, stream_stmt @ [L(if_stmt2_lbl)] @ stream_stmt2 @ [J(if_exit_insn)] @ [L(if_exit_lbl)])
            end
    | Ast.While (exp, stmt) ->
            begin
                let uid_exp = Ll.gen_sym() in
                let insn_exp = compile_exp exp uid_exp c in
                let exp_lbl = Ll.mk_lbl() in 
                let stmt_lbl = Ll.mk_lbl() in 
                let exit_lbl = Ll.mk_lbl() in 
                let c_stmt = compile_stmt stmt c in
                let stream_stmt = snd c_stmt in
                let entry_insn = Ll.Br(exp_lbl) in
                let insn_cmp = Ll.Cbr(Ll.Local(uid_exp), stmt_lbl, exit_lbl) in
                let stream_exp = List.fold_left (fun acc x -> acc @ [I(x)]) [] insn_exp in
                let stream_stmt = [J(entry_insn)] @ [L(exp_lbl)] @ stream_exp @ [J(insn_cmp)] @ [L(stmt_lbl)] @ stream_stmt @ [J(entry_insn)] @ [L(exit_lbl)] in
                (c, stream_stmt)
            end
    | Ast.For (vdecls, exp, stmt1, stmt2) ->
            let ret = compile_var_decls vdecls c [] in
            let new_ctxt = fst ret in
            let insn_decls = snd ret in
            let stream_decls = List.fold_left (fun acc x -> acc @ [I(x)]) [] insn_decls in
            let exp_lbl = Ll.mk_lbl() in
            let stmt_lbl = Ll.mk_lbl() in
            let exit_lbl = Ll.mk_lbl() in
            let uid_exp = Ll.gen_sym() in
            let insn_exp = 
                match exp with
                | Some e -> compile_exp e uid_exp new_ctxt
                | None -> [Ll.Load(uid_exp, Ll.Const(1l))]
            in
            let stream_exp = List.fold_left (fun acc x -> acc @ [I(x)]) [] insn_exp in
            let exp_cmp_insn = Ll.Cbr(Ll.Local(uid_exp), stmt_lbl, exit_lbl) in
            let stream_stmt1 = 
                match stmt1 with
                | Some s -> snd (compile_stmt s new_ctxt)
                | None -> []
            in
            let stream_stmt2 = snd (compile_stmt stmt2 new_ctxt) in
            let stream_stmt = stream_decls @ [J(Ll.Br exp_lbl)] @ [L(exp_lbl)] @ stream_exp @ [J(exp_cmp_insn)] @ [L(stmt_lbl)] @ stream_stmt1 @ stream_stmt2 @ [J(Ll.Br exp_lbl)] @ [L(exit_lbl)] in
            (c, stream_stmt)
    | Ast.Block block ->
            compile_block block c
    and
    compile_block ((vdecls, stmts) : Ast.block) (c : Ctxt.t) : (Ctxt.t * stream) =
            let ret = compile_var_decls vdecls c [] in
            let insn_decls = snd ret in
            let stream_decls = List.fold_left (fun acc x -> acc @ [I(x)]) [] insn_decls in
            let new_ctxt = fst ret in
            let stream_stmts = List.fold_left (fun acc x -> acc @ (snd (compile_stmt x new_ctxt))) [] stmts in
            (new_ctxt, stream_decls @ stream_stmts)


let rec blockify (s : stream) (bbs : Ll.bblock list) (l : Ll.lbl) (insns : Ll.insn list) : Ll.bblock list =
    match s with
    | [] -> bbs
    | e :: rest ->
            begin
                match e with
                | I i -> blockify rest bbs l (insns @ [i])
                | J j -> 
                        let bb = {Ll.label = l; Ll.insns = insns; Ll.terminator = j} in
                        blockify rest (bbs @ [bb]) l []
                | L newLabel -> blockify rest bbs newLabel []
            end

let compile_prog ((block, ret):Ast.prog) : Ll.prog =
    let ctxt = Ctxt.empty in
    let entry_lbl = Ll.mk_lbl() in
    let c_elt = compile_block block ctxt in
    let stream_elt = snd c_elt in
    let ctxt = fst c_elt in
    let uid_exp = Ll.gen_sym() in
    let exp_insns = compile_exp ret uid_exp ctxt in
    let stream_exp = List.fold_left (fun acc x -> acc @ [I(x)]) [] exp_insns in
    let bbs = blockify ([L(entry_lbl)] @ stream_elt @ stream_exp @ [J(Ll.Ret(Ll.Local(uid_exp)))]) [] entry_lbl [] in
    {Ll.ll_cfg = bbs; Ll.ll_entry = entry_lbl}

