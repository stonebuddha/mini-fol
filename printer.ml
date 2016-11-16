open Core
open Tactics

let string_of_conn c =
  match c with
  | CDisj -> "\\/"
  | CConj -> "/\\"
  | CImply -> "->"
  | CIff -> "<->"

let string_of_quan q =
  match q with
  | QForall -> "!"
  | QExists -> "?"

let rec string_of_term tm =
  match tm with
  | TmConst name -> name
  | TmVar x -> x
  | TmFuncApp (name, args) -> name ^ "(" ^ BatString.join ", " (List.map string_of_term args) ^ ")"

let printer_formula = Eprinter.make "formula"

EXTEND_PRINTER
  printer_formula:
    [ [ FmConn (c, fm1, fm2) -> Printf.sprintf "%s %s %s" (next pc fm1) (string_of_conn c) (curr pc fm2) ]
    | [ FmNot fm -> Printf.sprintf"~ %s" (next pc fm) ]
    | [ FmQuan (q, x, fm) -> Printf.sprintf "%s%s. %s" (string_of_quan q) x (next pc fm) ]
    | [ FmVar a -> a
      | FmPredApp (name, args) -> Printf.sprintf "%s(%s)" name (BatString.join ", " (List.map string_of_term args))
      | fm -> Printf.sprintf "(%s)" (Eprinter.apply printer_formula pc fm) ] ]
  ;
END

let string_of_formula fm = Eprinter.apply printer_formula Pprintf.empty_pc fm

let string_of_thm thm =
  let (hyps, concl) = (get_hyps thm, get_concl thm) in
  (BatString.join ", " (List.map string_of_formula hyps)) ^ " |- " ^ string_of_formula concl

let string_of_goal_stack gstack =
  match gstack with
  | [] -> "no current goal"
  | (gls, _) :: _ ->
    let n = List.length gls in
    if n = 0 then "no subgoals"
    else
      (if n = 1 then "1 subgoal" else string_of_int n ^ " subgoals") ^ "\n" ^
      (let (hyp_thms, concl) = List.hd gls in
       (BatString.join "\n" (List.mapi (fun i thm -> "  " ^ string_of_int i ^ " " ^ "`" ^ string_of_formula (get_concl thm) ^ "`") hyp_thms)) ^ "\n\n  " ^ "`" ^ string_of_formula concl ^ "`")

let print_formula ppf fm = Format.fprintf ppf "`%s`" (string_of_formula fm)

let print_thm ppf thm = Format.fprintf ppf "%s" (string_of_thm thm)

let print_goal_stack ppf gstack = Format.fprintf ppf "%s" (string_of_goal_stack gstack)
