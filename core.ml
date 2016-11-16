open Batteries

module type First_order_logic_kernel =
sig
  type conn = private
    | CDisj
    | CConj
    | CImply
    | CIff

  type quan = private
    | QForall
    | QExists

  type term = private
    | TmConst of string
    | TmVar of string
    | TmFuncApp of string * term list

  type formula = private
    | FmVar of string
    | FmPredApp of string * term list
    | FmNot of formula
    | FmConn of conn * formula * formula
    | FmQuan of quan * string * formula

  type thm

  val get_consts : unit -> string list
  val new_const : string -> unit
  val get_funcs : unit -> (string * int) list
  val new_func : string * int -> unit
  val get_preds : unit -> (string * int) list
  val new_pred : string * int -> unit

  val dest_thm : thm -> formula list * formula
  val get_hyps : thm -> formula list
  val get_concl : thm -> formula

  val mk_tm_const : string -> term
  val mk_tm_var : string -> term
  val mk_tm_func_app : string * term list -> term
  val mk_fm_var : string -> formula
  val mk_fm_pred_app : string * term list -> formula
  val mk_fm_not : formula -> formula
  val mk_fm_disj : formula * formula -> formula
  val mk_fm_conj : formula * formula -> formula
  val mk_fm_imply : formula * formula -> formula
  val mk_fm_iff : formula * formula -> formula
  val mk_fm_forall : string * formula -> formula
  val mk_fm_exists : string * formula -> formula
end

module First_order_logic : First_order_logic_kernel =
struct
  type conn =
    | CDisj
    | CConj
    | CImply
    | CIff

  type quan =
    | QForall
    | QExists

  type term =
    | TmConst of string
    | TmVar of string
    | TmFuncApp of string * term list

  type formula =
    | FmVar of string
    | FmPredApp of string * term list
    | FmNot of formula
    | FmConn of conn * formula * formula
    | FmQuan of quan * string * formula

  type thm = Theorem of formula list * formula

  let the_consts = ref []

  let get_consts () = !the_consts

  let new_const name =
    if List.mem name !the_consts then
      failwith ("new_const: duplicate constants")
    else
      the_consts := name :: !the_consts

  let the_funcs = ref []

  let get_funcs () = !the_funcs

  let new_func (name, arity) =
    if List.mem_assoc name !the_funcs then
      failwith ("new_func: duplicate functions")
    else
      the_funcs := (name, arity) :: !the_funcs

  let the_preds = ref []

  let get_preds () = !the_preds

  let new_pred (name, arity) =
    if List.mem_assoc name !the_preds then
      failwith ("new_pred: duplicate predicates")
    else
      the_preds := (name, arity) :: !the_preds

  let dest_thm (Theorem (hyps, concl)) = (hyps, concl)

  let get_hyps (Theorem (hyps, _)) = hyps

  let get_concl (Theorem (_, concl)) = concl

  let mk_tm_const name =
    if List.mem name !the_consts then
      TmConst name
    else
      failwith "mk_tm_const: undeclared constant"

  let mk_tm_var var = TmVar var

  let mk_tm_func_app (name, args) =
    try
      let arity = List.assoc name !the_funcs in
      if arity = List.length args then
        TmFuncApp (name, args)
      else
        failwith "mk_tm_func: arity mismatch"
    with Not_found -> failwith "mk_tm_func: undeclared function"

  let mk_fm_var var = FmVar var

  let mk_fm_pred_app (name, args) =
    try
      let arity = List.assoc name !the_preds in
      if arity = List.length args then
        FmPredApp (name, args)
      else
        failwith "mk_fm_pred_app: arity mismatch"
    with Not_found -> failwith "mk_fm_pred_app: undeclared predicate"

  let mk_fm_not fm = FmNot fm

  let mk_fm_conn c (fm1, fm2) = FmConn (c, fm1, fm2)

  let mk_fm_disj = mk_fm_conn CDisj

  let mk_fm_conj = mk_fm_conn CConj

  let mk_fm_imply = mk_fm_conn CImply

  let mk_fm_iff = mk_fm_conn CIff

  let mk_fm_quan q fm = FmQuan (q, fm)

  let mk_fm_forall = mk_fm_quan QForall

  let mk_fm_exists = mk_fm_quan QExists
end

include First_order_logic
