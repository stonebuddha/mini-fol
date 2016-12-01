open Fol_lexer

let g = Grammar.gcreate fol_lex
let expr_formula_eos = Grammar.Entry.create g "formula"
let patt_formula_eos = Grammar.Entry.create g "formula"
let expr_term_eos = Grammar.Entry.create g "term"
let patt_term_eos = Grammar.Entry.create g "term"

EXTEND
  GLOBAL: expr_formula_eos expr_term_eos;
  expr_formula_eos:
    [ [ fm = expr_formula; EOS -> fm ] ]
  ;
  expr_formula:
    [ RIGHTA
      [ fm1 = SELF; DISJ; fm2 = SELF -> <:expr< mk_fm_disj ($fm1$, $fm2$) >>
      | fm1 = SELF; CONJ; fm2 = SELF -> <:expr< mk_fm_conj ($fm1$, $fm2$) >>
      | fm1 = SELF; IMPLY; fm2 = SELF -> <:expr< mk_fm_imply ($fm1$, $fm2$) >>
      | fm1 = SELF; IFF; fm2 = SELF -> <:expr< mk_fm_iff ($fm1$, $fm2$) >> ]
    | [ NOT; fm = SELF -> <:expr< mk_fm_not $fm$ >> ]
    | [ FORALL; l = LIDENT; "."; fm = SELF -> <:expr< mk_fm_forall ($str:l$, $fm$) >>
      | EXISTS; l = LIDENT; "."; fm = SELF -> <:expr< mk_fm_exists ($str:l$, $fm$) >> ]
    | [ s = SYMBOL -> <:expr< mk_fm_var $str:s$ >>
      | u = UIDENT; "("; al = arg_list0; ")" -> <:expr< mk_fm_pred_app ($str:u$, $al$) >>
      | "("; fm = SELF; ")" -> fm ] ]
  ;
  expr_term_eos:
    [ [ tm = expr_term; EOS -> tm ] ]
  ;
  expr_term:
    [
      [ u = UIDENT -> <:expr< mk_tm_const $str:u$ >>
      | l = LIDENT -> <:expr< mk_tm_var $str:l$ >>
      | l = LIDENT; "("; al = arg_list0; ")" -> <:expr< mk_tm_func_app ($str:l$, $al$) >> ] ]
  ;
  arg_list0:
    [
      [ -> <:expr< [] >>
      | al = arg_list1 -> <:expr< $al$ >> ] ]
  ;
  arg_list1:
    [
      [ tm = expr_term -> <:expr< [$tm$] >>
      | tm = expr_term; ","; al = arg_list1 -> <:expr< $tm$ :: $al$ >> ] ]
  ;
END

let expand_expr_formula s = Grammar.Entry.parse expr_formula_eos (Stream.of_string s)
let expand_patt_formula s = Grammar.Entry.parse patt_formula_eos (Stream.of_string s)
let expand_expr_term s = Grammar.Entry.parse expr_term_eos (Stream.of_string s)
let expand_patt_term s = Grammar.Entry.parse patt_term_eos (Stream.of_string s)

let () = Quotation.add "formula" (Quotation.ExAst (expand_expr_formula, expand_patt_formula))
let () = Quotation.add "term" (Quotation.ExAst (expand_expr_term, expand_patt_term))
let () = Quotation.default := "formula"
