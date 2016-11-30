(* A simple 32-bit subset of X86. See the RUX86 specification. 
 * Author: Prof. Santosh Nagarkatte
 *)

type lbl

type reg =
  Eax  (* Accumulator register *)
| Ebx  (* Base register. Used as base pointer for memory *)
| Ecx  (* Counter register. Used for loop counters and shifts *)
| Edx  (* Data register. I/O port access, arithmetic operations *)
| Edi  (* Destination index register. Used for string and  memory array copying *)
| Esi  (* Source index register. Used for string and memory array copying *)
| Ebp  (* Stack base pointer register.  Holds the base address of the stack *)
| Esp  (* Stack pointer *)

(** X86 supports several different kinds of instruction operands.*)

(** A displacement is either an [int32] or a [lbl] *)
type disp =
  | DImm of int32
  | DLbl of lbl

(** An indirect offset is calculated as

    [Base + (Index * Scale) + Displacement] 
*)
type ind = {
  i_base : reg option;           (** Base. *)
  i_iscl : (reg * int32) option; (** Index must not be ESP *)
  i_disp : disp option           (** Constant displacement *)
}

(** Instruction Operands *)
type operand =
  | Imm of int32 (** Immediate int32 value. *)
  | Lbl of lbl   (** Immediate label value. *)
  | Reg of reg   (** Register operand. *)
  | Ind of ind   (** Indirect operand*)


val eax : operand
val ebx : operand
val ecx : operand
val edx : operand
val esi : operand
val edi : operand
val ebp : operand
val esp : operand

type ccode = 
  | Sgt 
  | Sge 
  | Slt 
  | Sle
  | Eq 
  | NotEq 
  | Zero    (** same as Eq *) 
  | NotZero (** same as NotEq *)


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

(** An insn block (with a label and a list of instructions). *)
type insn_block = {
  global : bool;
  label : lbl;
  insns : insn list;
}


(** Make a non-global instruction block with a particular label. *)
val mk_insn_block : lbl -> insn list -> insn_block

(** Make a non-global anonymous instruction block, generating a fresh label for it. *)
val mk_ainsn_block : insn list -> (lbl * insn_block)

(** Make an explicitly named non-global insn block. *)
val mk_block : string -> insn list -> insn_block

(** {2 Pretty Printing} *)

(** Returns the string corresponding to a given label *)
val string_of_lbl : lbl -> string

(** Returns the string corresponding to a given index *)
val string_of_ind  : ind -> string

(** Returns the string corresponding to a given register *)
val string_of_reg : reg -> string

(** Returns the string corresponding to a given operand *)
val string_of_operand : operand -> string

(** Returns the string corresponding to a given condition code *)
val string_of_ccode : ccode -> string

(** Returns the string corresponding to a given instruction *)
val string_of_insn : insn -> string

(** Returns the string corresponding to a given instruction block *)
val string_of_insn_block : insn_block -> string

(** Writes an instruction block using the provided printing function. *)
val serialize_insn_block : insn_block -> (string -> unit) -> unit

(** Stack offset **)

val stack_offset : int32 -> operand

val mk_lbl: unit-> lbl

val mk_lbl_named: string -> lbl