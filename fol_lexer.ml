let rec ident =
  lexer
    [ [ 'a'-'z' | 'A'-'Z' | '0'-'9' | '_' ]
      ident!
    | ]

let empty _ = parser [< _ = Stream.empty >] -> []

let rec next_tok =
  lexer
    [ 'a'-'z' ident! -> ("LIDENT", $buf)
    | 'A'-'Z' ident! -> ("UIDENT", $buf)
    | '\'' 'a'-'z' ident! -> ("SYMBOL", $buf)
    | "(" -> ("", "(")
    | ")" -> ("", ")")
    | "," -> ("", ",")
    | "." -> ("", ".")
    | "~" -> ("NOT", "")
    | "\\/" -> ("DISJ", "")
    | "/\\" -> ("CONJ", "")
    | "->" -> ("IMPLY", "")
    | "<->" -> ("IFF", "")
    | "!" -> ("FORALL", "")
    | "?" -> ("EXISTS", "")
    | empty -> ("EOS", "")
    | -> raise (Stream.Error "lexing error: bad character") ]

let rec skip_spaces =
  lexer
    [ " "/ skip_spaces!
    | "\n"/ skip_spaces!
    | "\r"/ skip_spaces!
    | ]

let record_loc loct i (bp, ep) =
  if i >= Array.length !loct then
    let newt =
      Array.init (2 * Array.length !loct + 1)
        (fun i ->
           if i < Array.length !loct then !loct.(i)
           else Ploc.dummy) in
    loct := newt
  else ();
  !loct.(i) <- Ploc.make_unlined (bp, ep)

let lex_func cs =
  let loct = ref [| |] in
  let ts =
    Stream.from
      (fun i ->
         ignore (skip_spaces $empty cs : Plexing.Lexbuf.t);
         let bp = Stream.count cs in
         let r = next_tok $empty cs in
         let ep = Stream.count cs in
         record_loc loct i (bp, ep);
         Some r) in
  let locf i =
    if i < Array.length !loct then !loct.(i) else Ploc.dummy in
  (ts, locf)

let fol_lex =
  {
    Plexing.tok_func = lex_func;
    Plexing.tok_using = (fun _ -> ());
    Plexing.tok_removing = (fun _ -> ());
    Plexing.tok_match = Plexing.default_match;
    Plexing.tok_text = Plexing.lexer_text;
    Plexing.tok_comm = None
  }
