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

  val dest_tm_const : term -> string
  val dest_tm_var : term -> string
  val dest_tm_func_app : term -> string * term list
  val dest_fm_var : formula -> string
  val dest_fm_pred_app : formula -> string * term list
  val dest_fm_not : formula -> formula
  val dest_fm_disj : formula -> formula * formula
  val dest_fm_conj : formula -> formula * formula
  val dest_fm_imply : formula -> formula * formula
  val dest_fm_iff : formula -> formula * formula
  val dest_fm_forall : formula -> string * formula
  val dest_fm_exists : formula -> string * formula

  val _INCL : formula list -> formula -> thm
  val _NOT_ELIM : formula -> thm -> thm -> thm
  val _IMPLY_ELIM : thm -> thm -> thm
  val _IMPLY_INTRO : formula -> thm -> thm
  val _DISJ_ELIM : formula -> formula -> thm -> thm -> thm
  val _DISJ_INTRO_INL : formula -> thm -> thm
  val _DISJ_INTRO_INR : formula -> thm -> thm
  val _CONJ_ELIM_LEFT : thm -> thm
  val _CONJ_ELIM_RIGHT : thm -> thm
  val _CONJ_INTRO : thm -> thm -> thm
  val _IFF_ELIM_LEFT : thm -> thm -> thm
  val _IFF_ELIM_RIGHT : thm -> thm -> thm
  val _IFF_INTRO : thm -> thm -> thm
  val _ADD_PREM : formula -> thm -> thm
  val _FORALL_ELIM : term -> thm -> thm
  val _FORALL_INTRO : string -> thm -> thm
  val _EXISTS_ELIM : string -> formula -> thm -> thm
  val _EXISTS_INTRO : string -> term -> formula -> thm -> thm
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

  let mk_fm_quan q (x, fm) = FmQuan (q, x, fm)

  let mk_fm_forall = mk_fm_quan QForall

  let mk_fm_exists = mk_fm_quan QExists

  let dest_tm_const tm =
    match tm with
    | TmConst name -> name
    | _ -> failwith "dest_tm_const"

  let dest_tm_var tm =
    match tm with
    | TmVar x -> x
    | _ -> failwith "dest_tm_var"

  let dest_tm_func_app tm =
    match tm with
    | TmFuncApp (name, args) -> (name, args)
    | _ -> failwith "dest_tm_func_app"

  let dest_fm_var fm =
    match fm with
    | FmVar a -> a
    | _ -> failwith "dest_fm_var"

  let dest_fm_pred_app fm =
    match fm with
    | FmPredApp (name, args) -> (name, args)
    | _ -> failwith "dest_fm_pred_app"

  let dest_fm_not fm =
    match fm with
    | FmNot fm -> fm
    | _ -> failwith "dest_fm_not"

  let dest_fm_disj fm =
    match fm with
    | FmConn (CDisj, fm1, fm2) -> (fm1, fm2)
    | _ -> failwith "dest_fm_disj"

  let dest_fm_conj fm =
    match fm with
    | FmConn (CConj, fm1, fm2) -> (fm1, fm2)
    | _ -> failwith "dest_fm_conj"

  let dest_fm_imply fm =
    match fm with
    | FmConn (CImply, fm1, fm2) -> (fm1, fm2)
    | _ -> failwith "dest_fm_imply"

  let dest_fm_iff fm =
    match fm with
    | FmConn (CIff, fm1, fm2) -> (fm1, fm2)
    | _ -> failwith "dest_fm_iff"

  let dest_fm_forall fm =
    match fm with
    | FmQuan (QForall, x, fm) -> (x, fm)
    | _ -> failwith "dest_fm_forall"

  let dest_fm_exists fm =
    match fm with
    | FmQuan (QExists, x, fm) -> (x, fm)
    | _ -> failwith "dest_fm_exists"

  let rec is_var_free_in_tm x tm =
    match tm with
    | TmConst _ -> false
    | TmVar y -> x = y
    | TmFuncApp (_, args) -> List.exists (is_var_free_in_tm x) args

  let rec is_var_free_in_fm x fm =
    match fm with
    | FmVar _ -> false
    | FmPredApp (_, args) -> List.exists (is_var_free_in_tm x) args
    | FmNot fm -> is_var_free_in_fm x fm
    | FmConn (_, fm1, fm2) -> is_var_free_in_fm x fm1 || is_var_free_in_fm x fm2
    | FmQuan (_, y, fm) -> x <> y && is_var_free_in_fm x fm

  let subst_tm_in_tm x t s =
    let rec inner s =
      match s with
      | TmConst _ -> s
      | TmVar y -> if x = y then t else s
      | TmFuncApp (name, args) -> TmFuncApp (name, List.map inner args)
    in
    inner s

  let subst_tm_in_fm x t s =
    let rec inner s =
      match s with
      | FmVar _ -> s
      | FmPredApp (name, args) -> FmPredApp (name, List.map (subst_tm_in_tm x t) args)
      | FmNot fm -> FmNot (inner fm)
      | FmConn (c, fm1, fm2) -> FmConn (c, inner fm1, inner fm2)
      | FmQuan (q, y, fm) ->
        if x = y then s
        else
          if is_var_free_in_fm x fm && is_var_free_in_tm y t then
            failwith "subst_tm_in_fm"
          else
            FmQuan (q, y, inner fm)
    in
    inner s

  let _INCL hyps concl =
    if List.mem concl hyps then
      Theorem (List.sort compare hyps, concl)
    else
      failwith "INCL"

  let _NOT_ELIM dis (Theorem (hyps1, concl1)) (Theorem (hyps2, concl2)) =
    if mk_fm_not concl1 = concl2 && List.mem (mk_fm_not dis) hyps1 && List.mem (mk_fm_not dis) hyps2 then
      let hyps1' = List.remove hyps1 (mk_fm_not dis) in
      let hyps2' = List.remove hyps2 (mk_fm_not dis) in
      if hyps1' = hyps2' then
        Theorem (hyps1', dis)
      else
        failwith "NOT_ELIM"
    else
      failwith "NOT_ELIM"

  let _IMPLY_ELIM (Theorem (hyps1, concl1)) (Theorem (hyps2, concl2)) =
    try
      let (fm11, fm12) = dest_fm_imply concl1 in
      if fm11 = concl2 && hyps1 = hyps2 then
        Theorem (hyps1, fm12)
      else
        failwith "IMPLY_ELIM"
    with Failure _ -> failwith "IMPLY_ELIM"

  let _IMPLY_INTRO prem (Theorem (hyps, concl)) =
    if List.mem prem hyps then
      let hyps' = List.remove hyps prem in
      Theorem (hyps', mk_fm_imply (prem, concl))
    else
      failwith "IMPLY_INTRO"

  let _DISJ_ELIM inl inr (Theorem (hyps1, concl1)) (Theorem (hyps2, concl2)) =
    if concl1 = concl2 && List.mem inl hyps1 && List.mem inr hyps2 then
      let hyps1' = List.remove hyps1 inl in
      let hyps2' = List.remove hyps2 inr in
      if hyps1' = hyps2' then
        Theorem (List.merge compare hyps1' [mk_fm_disj (inl, inr)], concl1)
      else
        failwith "DISJ_ELIM"
    else
      failwith "DISJ_ELIM"

  let _DISJ_INTRO_INL inr (Theorem (hyps, concl)) = Theorem (hyps, mk_fm_disj (concl, inr))

  let _DISJ_INTRO_INR inl (Theorem (hyps, concl)) = Theorem (hyps, mk_fm_disj (inl, concl))

  let _CONJ_ELIM_LEFT (Theorem (hyps, concl)) =
    try
      let (left, right) = dest_fm_conj concl in
      Theorem (hyps, left)
    with Failure _ -> failwith "CONJ_ELIM_LEFT"

  let _CONJ_ELIM_RIGHT (Theorem (hyps, concl)) =
    try
      let (left, right) = dest_fm_conj concl in
      Theorem (hyps, right)
    with Failure _ -> failwith "CONJ_ELIM_RIGHT"

  let _CONJ_INTRO (Theorem (hyps1, concl1)) (Theorem (hyps2, concl2)) =
    if hyps1 = hyps2 then
      Theorem (hyps1, mk_fm_conj (concl1, concl2))
    else
      failwith "CONJ_INTRO"

  let _IFF_ELIM_LEFT (Theorem (hyps1, concl1)) (Theorem (hyps2, concl2)) =
    try
      let (tm11, tm12) = dest_fm_iff concl1 in
      if tm11 = concl2 && hyps1 = hyps2 then
        Theorem (hyps1, tm12)
      else
        failwith "IFF_ELIM_LEFT"
    with Failure _ -> failwith "IFF_ELIM_LEFT"

  let _IFF_ELIM_RIGHT (Theorem (hyps1, concl1)) (Theorem (hyps2, concl2)) =
    try
      let (tm11, tm12) = dest_fm_iff concl1 in
      if tm12 = concl2 && hyps1 = hyps2 then
        Theorem (hyps1, tm11)
      else
        failwith "IFF_ELIM_RIGHT"
    with Failure _ -> failwith "IFF_ELIM_RIGHT"

  let _IFF_INTRO (Theorem (hyps1, concl1)) (Theorem (hyps2, concl2)) =
    if List.mem concl2 hyps1 && List.mem concl1 hyps2 then
      let hyps1' = List.remove hyps1 concl2 in
      let hyps2' = List.remove hyps2 concl1 in
      if hyps1' = hyps2' then
        Theorem (hyps1', mk_fm_iff (concl2, concl1))
      else
        failwith "IFF_INTRO"
    else
      failwith "IFF_INTRO"

  let _ADD_PREM prem (Theorem (hyps, concl)) = Theorem (List.merge compare hyps [prem], concl)

  let _FORALL_ELIM tm (Theorem (hyps, concl)) =
    try
      let (x, fm) = dest_fm_forall concl in
      Theorem (hyps, subst_tm_in_fm x tm fm)
    with Failure _ -> failwith "FORALL_ELIM"

  let _FORALL_INTRO x (Theorem (hyps, concl)) =
    if not (List.exists (is_var_free_in_fm x) hyps) then
      Theorem (hyps, mk_fm_forall (x, concl))
    else
      failwith "FORALL_INTRO"

  let _EXISTS_ELIM x fm (Theorem (hyps, concl)) =
    if List.mem fm hyps then
      let hyps' = List.remove hyps fm in
      if not (List.exists (is_var_free_in_fm x) (concl :: hyps')) then
        Theorem (List.merge compare hyps' [mk_fm_exists (x, fm)], concl)
      else
        failwith "EXISTS_ELIM"
    else
      failwith "EXISTS_ELIM"

  let _EXISTS_INTRO x tm fm (Theorem (hyps, concl)) =
    try
      if subst_tm_in_fm x tm fm = concl then
        Theorem (hyps, mk_fm_exists (x, fm))
      else
        failwith "EXISTS_INTRO"
    with Failure _ -> failwith "EXISTS_INTRO"
end

include First_order_logic
