open Batteries
open Core

type goal = thm list * formula

type continuation = thm list -> thm

type goal_state = goal list * continuation

type goal_stack = goal_state list

type refinement = goal_state -> goal_state

type tactic = goal -> goal_state

let by (tac : tactic) : refinement =
  fun (gls, cont) ->
    match gls with
    | [] -> failwith "no subgoals"
    | gl :: rest_gls ->
      let (sub_gls, sub_cont) = tac gl in
      let gls' = sub_gls @ rest_gls in
      let cont' thms =
        let (sub_thms, rest_thms) = List.split_at (List.length sub_gls) thms in
        let thms' = sub_cont sub_thms :: rest_thms in
        cont thms'
      in
      (gls', cont')

let mk_goal_state ((hyp_thms, concl) : goal) : goal_state = ([(hyp_thms, concl)], fun [thm] -> thm)

let the_goal_stack = ref ([] : goal_stack)

let refine (r : refinement) =
  let gstack = !the_goal_stack in
  match gstack with
  | [] -> failwith "no current goal"
  | gstate :: _ ->
    let gstack' = r gstate :: gstack in
    the_goal_stack := gstack';
    gstack'

let e tac = refine (by tac)

let set_goal (hyps, concl) =
  the_goal_stack :=
    [mk_goal_state (List.map (fun hyp -> _ASSUME hyp) hyps, concl)];
  !the_goal_stack

let g hyps concl = set_goal (hyps, concl)

let b () =
  let gstack = !the_goal_stack in
  if List.length gstack <= 1 then
    failwith "cannot back up"
  else
    (the_goal_stack := List.tl gstack;
     !the_goal_stack)

let p () = !the_goal_stack

let top_thm () =
  let ([], cont) :: _ = !the_goal_stack in
  cont []

let _EXACT_TAC n ((hyp_thms, concl) : goal) : goal_state =
  if List.length hyp_thms <= n then
    failwith "EXACT_TAC"
  else
    let the_hyp_thm = List.nth hyp_thms n in
    if get_concl the_hyp_thm = concl then
      ([], fun [] -> _INCL (List.map get_concl hyp_thms) concl)
    else
      failwith "EXACT_TAC"

let _SPLIT_CONJ_TAC ((hyp_thms, concl) : goal) : goal_state =
  try
    let (left_fm, right_fm) = dest_fm_conj concl in
    ([(hyp_thms, left_fm); (hyp_thms, right_fm)], fun [left_thm; right_thm] -> _CONJ_INTRO left_thm right_thm)
  with Failure _ -> failwith "SPLIT_CONJ_TAC"

let _DESTRUCT_CONJ_TAC n ((hyp_thms, concl) : goal) : goal_state =
  if List.length hyp_thms <= n then
    failwith "DESTRUCT_CONJ_TAC"
  else
    try
      let the_hyp_thm = List.nth hyp_thms n in
      let the_left_thm = _CONJ_ELIM_LEFT the_hyp_thm in
      let the_right_thm = _CONJ_ELIM_RIGHT the_hyp_thm in
      ([(the_left_thm :: the_right_thm :: (List.remove hyp_thms the_hyp_thm), concl)], fun [thm] -> _TRANS [the_left_thm; the_right_thm] thm)
    with Failure _ -> failwith "DESTRUCT_CONJ_TAC"

let _DESTRUCT_DISJ_TAC n ((hyp_thms, concl) : goal) : goal_state =
  if List.length hyp_thms <= n then
    failwith "DESTRUCT_DISJ_TAC"
  else
    try
      let the_hyp_thm = List.nth hyp_thms n in
      let the_concl = get_concl the_hyp_thm in
      let (the_inl_fm, the_inr_fm) = dest_fm_disj the_concl in
      let hyp_thms' = List.remove hyp_thms the_hyp_thm in
      let the_inl_thm = _ASSUME the_inl_fm in
      let the_inl_goal = (the_inl_thm :: hyp_thms', concl) in
      let the_inr_thm = _ASSUME the_inr_fm in
      let the_inr_goal = (the_inr_thm :: hyp_thms', concl) in
      ([the_inl_goal; the_inr_goal], fun [inl_thm; inr_thm] -> _DISJ_ELIM the_inl_fm the_inr_fm inl_thm inr_thm)
    with Failure _ -> failwith "DESTRUCT_DISJ_TAC"

let _INL_DISJ_TAC ((hyp_thms, concl) : goal) : goal_state =
  try
    let (inl_fm, inr_fm) = dest_fm_disj concl in
    ([(hyp_thms, inl_fm)], fun [thm] -> _DISJ_INTRO_INL inr_fm thm)
  with Failure _ -> failwith "INL_DISJ_TAC"

let _INR_DISJ_TAC ((hyp_thms, concl) : goal) : goal_state =
  try
    let (inl_fm, inr_fm) = dest_fm_disj concl in
    ([(hyp_thms, inr_fm)], fun [thm] -> _DISJ_INTRO_INR inl_fm thm)
  with Failure _ -> failwith "INR_DISJ_TAC"
