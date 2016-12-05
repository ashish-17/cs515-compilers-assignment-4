open LibUtil

(* 
 * One possible implementation of a context is:
 * an association list of the form: (string, uid) list
 *)
type t = (string * Ll.uid) list
exception Scope_error of string 

let dump (c : t) : unit =
failwith "dump not implemented"
      
let empty : t = [] 

(* try to reuse freed, otherwise create new *)
let alloc (sym : string) (c : t) : (t * Ll.uid) =
    let id = Ll.mk_uid sym in
    let new_ctxt = ((sym, id) :: c) in
    (new_ctxt, id)

let lookup (sym : string) (c : t) : Ll.uid =
    try
        List.assoc sym c
    with Not_found ->
        raise (Scope_error sym)
