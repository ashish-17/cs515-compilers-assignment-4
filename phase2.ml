open Ll
open Rux86
open LibUtil

let stack_offset_ebp (amt:int32) : Rux86.operand =
    Ind{i_base = Some Ebp;
        i_iscl = None;
        i_disp = Some (DImm amt)} 

let compile_uid (id: Ll.uid) : Rux86.operand = 
    match id with
    | (i, s) -> stack_offset_ebp (Int32.of_int(-4-(4*i)))

let compile_operand (op: Ll.operand) : Rux86.operand =
    begin match op with
        | Ll.Local id -> compile_uid id
        | Ll.Const (x) -> Rux86.Imm x
    end

let compile_insn (i : Ll.insn) : Rux86.insn list = 
    match i with
    | Ll.Binop (uid, bop, op1, op2) ->
            let c_op1 = compile_operand op1 in
            let c_op2 = compile_operand op2 in
            let c_uid = compile_uid uid in
            let movein_insn = [Mov(c_op1, ecx); Mov(c_op2, eax)] in
            let moveout_insn = [Mov(ecx, c_uid)] in
            begin match bop with
            | Add -> movein_insn @ [Rux86.Add(eax, ecx)] @ moveout_insn
            | Sub -> movein_insn @ [Rux86.Sub(eax, ecx)] @ moveout_insn
            | Mul -> movein_insn @ [Rux86.Imul(eax, Ecx)] @ moveout_insn
            | Shl -> movein_insn @ [Rux86.Shl(eax, ecx)] @ moveout_insn
            | Lshr -> movein_insn @ [Rux86.Shr(eax, ecx)] @ moveout_insn
            | Ashr -> movein_insn @ [Rux86.Sar(eax, ecx)] @ moveout_insn
            | And -> movein_insn @ [Rux86.And(eax, ecx)] @ moveout_insn
            | Or -> movein_insn @ [Rux86.Or(eax, ecx)] @ moveout_insn
            | Xor -> movein_insn @ [Rux86.Xor(eax, ecx)] @ moveout_insn
            end
    | Ll.Alloca id ->
            []
    | Ll.Load (id, op) ->
            let c_op = compile_operand op in
            let c_uid = compile_uid id in
            [Mov(c_op, eax); Mov(eax, c_uid)]
    | Ll.Store (op1, op2) ->
            let c_op1 = compile_operand op1 in
            let c_op2 = compile_operand op2 in
            [Mov(c_op1, ecx); Mov(c_op2, eax); Mov(eax, Ind{i_base = Some Ecx; i_iscl = None; i_disp = Some (DImm 0l)})]
    | Ll.Icmp (id, cop, op1, op2) ->
            let c_op1 = compile_operand op1 in
            let c_op2 = compile_operand op2 in
            let c_uid = compile_uid id in
            let movein_insn = [Mov(c_op1, ecx); Mov(c_op2, eax)] in
            let moveout_insn = [Mov(ecx, c_uid)] in
            match cop with
            | Eq -> movein_insn @ [Cmp(eax, ecx); Setb(Rux86.Eq, ecx); Rux86.And(Imm 1l, ecx)] @ moveout_insn
            | Ne -> movein_insn @ [Cmp(eax, ecx); Setb(Rux86.NotEq, ecx); Rux86.And(Imm 1l, ecx)] @ moveout_insn
            | Slt -> movein_insn @ [Cmp(eax, ecx); Setb(Rux86.Slt, ecx); Rux86.And(Imm 1l, ecx)] @ moveout_insn
            | Sle -> movein_insn @ [Cmp(eax, ecx); Setb(Rux86.Sle, ecx); Rux86.And(Imm 1l, ecx)] @ moveout_insn
            | Sgt -> movein_insn @ [Cmp(eax, ecx); Setb(Rux86.Sgt, ecx); Rux86.And(Imm 1l, ecx)] @ moveout_insn
            | Sge -> movein_insn @ [Cmp(eax, ecx); Setb(Rux86.Sge, ecx); Rux86.And(Imm 1l, ecx)] @ moveout_insn


let compile_insns (insns: Ll.insn list) : Rux86.insn list = 
    List.fold_left (fun acc x -> acc @ (compile_insn x)) [] insns

let compile_terminator (t : Ll.terminator) : Rux86.insn list =
    match t with
    | Ret op -> [Mov((compile_operand(op), eax))]
    | Br l -> [Jmp(Lbl l)]
    | Cbr (op, lb1, lb2) ->
            [Cmp(Imm 1l, (compile_operand(op))); J(Eq, lb1); Jmp(Lbl lb2)] 


let compile_bb {Ll.label = l; Ll.insns = i; Ll.terminator = j}  : Cunit.component =
    let term_insns = compile_terminator j in
    let code = Cunit.Code({ 
        label = l; 
        insns = (compile_insns i) @ term_insns; 
        global = false}) in
    code

let compile_prog (prog : Ll.prog) : Cunit.cunit =
    let block_name = (Platform.decorate_cdecl "program") in
    let epi_label = mk_lbl_named (Platform.decorate_cdecl "epilogue") in 
    let insns_prologue = [Push(ebp);Mov(esp, ebp); Sub(Imm 10000l, esp)] in
    let insns_epilogue = [Add(Imm 10000l, esp); Mov(esp, ebp); Pop(ebp); Ret] in
    let code_epilogue = Cunit.Code({
        label = epi_label;
        insns = insns_epilogue;
        global = false}) in
    let code_program = Cunit.Code({ 
        label = mk_lbl_named block_name; 
        insns = insns_prologue; 
        global = true}) in
    [code_program] @ (List.map compile_bb prog.ll_cfg) @ [code_epilogue]
