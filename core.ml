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
      failwith ("new_const: constant " ^ name ^ " has already been declared")
    else
      the_consts := name :: !the_consts

  let the_funcs = ref []

  let get_funcs () = !the_funcs

  let new_func (name, arity) =
    if List.mem_assoc name !the_funcs then
      failwith ("new_func: function " ^ name ^ " has already been declared")
    else
      the_funcs := (name, arity) :: !the_funcs

  let the_preds = ref []

  let get_preds () = !the_preds

  let new_pred (name, arity) =
    if List.mem_assoc name !the_preds then
      failwith ("new_pred: predicate " ^ name ^ " has already been declared")
    else
      the_preds := (name, arity) :: !the_preds

  let dest_thm (Theorem (hyps, concl)) = (hyps, concl)

  let get_hyps (Theorem (hyps, _)) = hyps

  let get_concl (Theorem (_, concl)) = concl
end

include First_order_logic
