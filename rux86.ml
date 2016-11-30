
(* Abstract Syntax tree for RUX86 ISA *)

type lbl = string


(* Eax, Ebx, Ecx, Edx are general registers *)
(* Esi, Edi, Ebp, Esp are Index registers and pointers *)

type reg =
  Eax  (* Accumulator register *)
| Ebx  (* Base register. Used as base pointer for memory *)
| Ecx  (* Counter register. Used for loop counters and shifts *)
| Edx  (* Data register. I/O port access, arithmetic operations *)
| Edi  (* Destination index register. Used for string and  memory array copying *)
| Esi  (* Source index register. Used for string and memory array copying *)
| Ebp  (* Stack base pointer register.  Holds the base address of the stack *)
| Esp  (* Stack pointer *)


let string_of_reg (r:reg) : string = 
 "%" ^ (match r with 
        Eax -> "eax" | Ebx -> "ebx" | Ecx -> "ecx" | Edx -> "edx"
       |Edi -> "edi" | Esi -> "esi" | Ebp -> "ebp" | Esp -> "esp" )


let byte_string_of_reg (r:reg) : string = 
  "%" ^ (match r with
         Eax -> "al" | Ebx -> "bl" | Ecx -> "cl" | Edx -> "dl"
        |_ -> failwith "X86 illegal register - not byte addressable")


type disp = 
 | DImm of int32 
 | DLbl of lbl


(* To represent indexed addressing modes, we will use a record type *)

type ind = {
  i_base: reg option;             (* Base *)
  i_iscl : (reg * int32) option;  (* Index must not be ESP *)
  i_disp : disp option            (* Constant displacement (int32 or label). *)
}

(* Operands *)

type operand = 
  Imm of int32 (* Immediate int32 value *)
| Lbl of lbl   (* Immediate label value *)
| Reg of reg   (* Register operand *)
| Ind of ind   (* Indirect operand *)

(* helper function to convert an int32 to an operand type *)
let i32 (x:int32) : operand = Imm x

(* helper variables to return operands *)

let eax :operand = Reg Eax
let ebx :operand = Reg Ebx
let ecx :operand = Reg Ecx
let edx :operand = Reg Edx
let esi: operand = Reg Esi
let edi: operand = Reg Edi
let ebp: operand = Reg Ebp
let esp: operand = Reg Esp

(* Generate a stack offset *)
let stack_offset (amt:int32) : operand =
  Ind{i_base = Some Esp;
      i_iscl = None;
      i_disp = Some (DImm amt)} 

(* Condition codes *)

type ccode = 
 | Sgt 
 | Sge
 | Slt
 | Sle 
 | Eq
 | NotEq
 | Zero (* Same as Eq *)
 | NotZero (* Same as NotEq *)

let string_of_ccode (c:ccode) : string  = 
 match c with 
   Sgt -> "g"
 | Sge -> "ge"
 | Slt -> "l"
 | Sle -> "le"
 | Eq -> "e"
 | NotEq -> "ne"
 | Zero -> "z"
 | NotZero -> "nz"

(* Instructions *)

type insn = 
 | Neg of operand
 | Add of operand * operand
 | Sub of operand * operand
 | Imul of operand * reg
 | Not of operand
 | And of operand * operand
 | Or of operand * operand
 | Xor of operand * operand
 | Sar of operand * operand
 | Shl of operand * operand
 | Shr of operand * operand
 | Setb of ccode * operand
 | Lea of ind * reg
 | Mov of operand * operand
 | Push of operand
 | Pop of operand
 | Cmp of operand * operand
 | Jmp of operand
 | Call of operand
 | Ret 
 | J of ccode * lbl

(* Auxillary functions to print instructions from RUX86 AST *)

let string_of_lbl (l:lbl) : string = l 

let has_iscl_or_base  (i:ind) : bool = 
   match (i.i_base, i.i_iscl) with 
   | (Some _, _) -> true
   | (_, Some _) -> true
   | _ -> false

let string_of_ind (i:ind): string = 
  (match i.i_disp with 
  | Some (DImm x) -> Int32.to_string x 
  | Some (DLbl l) -> string_of_lbl l 
  | None -> "") 
  ^ (if not (has_iscl_or_base i) then "" else 
      "(" ^ (
           match i.i_base with 
           | None -> ""
           | Some r -> string_of_reg r
          ) ^ (
           match i.i_iscl with 
           | Some (r, scl) -> "," ^ (string_of_reg r) ^ "," ^
             (Int32.to_string scl)
           | None -> ""
          ) ^ ")"
    )

let string_of_operand (op:operand): string = 
  match op with 
  | Imm x -> "$" ^ (Int32.to_string x)
  | Lbl l -> "$" ^ (string_of_lbl l)
  | Reg r -> string_of_reg r
  | Ind i -> string_of_ind i 

let generic_to_string name (src, dest) = 
  name ^ "l " ^(string_of_operand src) ^ "," ^ (string_of_operand dest)

let lea_to_string (src, dest) = 
  "leal" ^ " " ^ (string_of_ind src) ^ ", " ^ (string_of_reg dest)   
 
let shift_to_string name (src, dest)  = 
 match src with 
  Imm _ -> name ^ "l " ^ (string_of_operand src) ^ ", " ^ (string_of_operand dest)
| Reg Ecx -> name ^ "l " ^ "%cl" ^ ", " ^ (string_of_operand dest)
| _ -> failwith "Shift amount not ECX or immediate"


let oneop_to_string name op =  
  name ^ "l " ^ (string_of_operand op)

let regdest_to_string name (src, dest) = 
  name ^ "l " ^ (string_of_operand src) ^ ", " ^ (string_of_reg dest)


let string_of_operand_low_byte (op: operand) : string = 
 match op with
 | Imm x -> failwith "X86 illegal operand - not byte size"
 | Lbl l -> failwith "X86 illegal operand - not byte size"
 | Reg r -> byte_string_of_reg r 
 | Ind i -> string_of_ind i

let setb_to_string (cc, dest) = 
  "set" ^ (string_of_ccode cc) ^ " " ^ (string_of_operand_low_byte dest)

(* Handle control flow instructions *)
let cfstring_of_operand (op:operand) : string = 
 match op with 
  | Imm i -> failwith "Bad x86: PC-relative jump with non-symbolic offset"
  | Lbl l -> string_of_lbl l 
  | Reg r -> "*" ^ (string_of_reg r)
  | Ind i -> "*" ^ (string_of_ind i)

let cfop_to_string name op = 
   name ^ " " ^ (cfstring_of_operand op)

let j_to_string (cond, lbl) = 
  "j" ^ (string_of_ccode cond) ^ " " ^ (string_of_lbl lbl)

let rec string_of_insn (i:insn) : string = 
  match i with 
  | Add (s, d) -> generic_to_string "add" (s, d)
  | Sub (s, d) -> generic_to_string "sub" (s, d)
  | Mov (s, d) -> generic_to_string "mov" (s, d)
  | And (s, d) -> generic_to_string "and" (s, d)
  | Or (s, d)  -> generic_to_string "or" (s, d)
  | Xor (s, d) -> generic_to_string "xor" (s, d)
  | Cmp (c1, c2) -> generic_to_string "cmp" (c1, c2)
  | Ret -> "ret"
  | Lea (s, d) -> lea_to_string (s, d)
  | Shl (s, d) -> shift_to_string "shl" (s, d)
  | Sar (s, d) -> shift_to_string "sar" (s, d)
  | Shr (s, d) -> shift_to_string "shr" (s, d)
  | Neg o      -> oneop_to_string "neg" o
  | Not o      -> oneop_to_string "not" o
  | Push o     -> oneop_to_string "push" o
  | Pop o      -> oneop_to_string "pop" o
  | Imul (src, dest) -> regdest_to_string "imul" (src, dest)
  | Setb (cc, dest) -> setb_to_string (cc, dest)
  | Jmp o      -> cfop_to_string "jmp" o
  | Call o     -> cfop_to_string "call" o
  | J (cond, lbl) -> j_to_string (cond, lbl) 

(* Auxillary functions for labels *)
let lbl_ctr = ref 0

(* Generate a unique label (except for abuses of mk_lbl_named) *)
let mk_lbl () : lbl = 
  let ctr = !lbl_ctr in
  let _ = lbl_ctr := ctr + 1 in
    "__" ^ (string_of_int ctr)
      
	
(* Generate a label containing the given string *)
let mk_lbl_hint (s:string) : lbl =
  let ctr = !lbl_ctr in
  let _ = lbl_ctr := ctr + 1 in
    "__" ^ s ^ (string_of_int ctr)
      
(* Force a label to have a particular string representation -- dangerous! *)
let mk_lbl_named (s:string) : lbl = s

(* A insn block (with a label and a list of instructions). *)
type insn_block = {
  global : bool;
  label : lbl;
  insns : insn list;
}

(* Make an non-global insn block with a particular label. *)
let mk_insn_block (l:lbl) (body:insn list) : insn_block =
  {label = l;
   insns = body;
   global = false}

(* Make a non-global anonymous insn block, generating a fresh label for it. *)
let mk_ainsn_block (body:insn list) : (lbl * insn_block) =
  let l = mk_lbl () in
    (l, mk_insn_block l body)

(* Make an explicitly named non-global insn block. *)
let mk_block (s:string) (insns:insn list) =
  mk_insn_block (mk_lbl_named s) insns

(* Write an insn block using the provided printing function. *)
let serialize_insn_block (blk : insn_block) (pfunc : string -> unit) : unit =
  if blk.global then (
    pfunc (Printf.sprintf ".globl %s\n" (string_of_lbl blk.label))
  ) else ();
  pfunc (Printf.sprintf "%s:\n" (string_of_lbl blk.label));
  List.iter (fun insn -> pfunc (Printf.sprintf "\t%s\n" (string_of_insn insn)))
    blk.insns
			
let string_of_insn_block (blk:insn_block) : string =
  List.fold_left (fun x i -> x ^ (string_of_insn i)) 
    (if blk.global 
     then (Printf.sprintf ".global %s\n" (string_of_lbl blk.label))
     else (Printf.sprintf "%s\n" (string_of_lbl blk.label))) 
    blk.insns
