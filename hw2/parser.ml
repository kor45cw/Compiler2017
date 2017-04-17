type token =
  | NUM of (int)
  | ID of (string)
  | INT
  | PLUS
  | MINUS
  | STAR
  | SLASH
  | EQUAL
  | EQUALEQUAL
  | LE
  | LT
  | GE
  | GT
  | NOT
  | AND
  | OR
  | IF
  | ELSE
  | WHILE
  | DO
  | READ
  | PRINT
  | SEMICOLON
  | LBRACE
  | RBRACE
  | LBLOCK
  | RBLOCK
  | LPAREN
  | RPAREN
  | EOF

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"

# 38 "parser.ml"
let yytransl_const = [|
  259 (* INT *);
  260 (* PLUS *);
  261 (* MINUS *);
  262 (* STAR *);
  263 (* SLASH *);
  264 (* EQUAL *);
  265 (* EQUALEQUAL *);
  266 (* LE *);
  267 (* LT *);
  268 (* GE *);
  269 (* GT *);
  270 (* NOT *);
  271 (* AND *);
  272 (* OR *);
  273 (* IF *);
  274 (* ELSE *);
  275 (* WHILE *);
  276 (* DO *);
  277 (* READ *);
  278 (* PRINT *);
  279 (* SEMICOLON *);
  280 (* LBRACE *);
  281 (* RBRACE *);
  282 (* LBLOCK *);
  283 (* RBLOCK *);
  284 (* LPAREN *);
  285 (* RPAREN *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  257 (* NUM *);
  258 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\003\000\004\000\000\000"

let yylen = "\002\000\
\002\000\004\000\000\000\000\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\003\000\005\000\000\000\004\000\001\000\000\000\
\002\000"

let yydgoto = "\002\000\
\004\000\005\000\006\000\008\000"

let yysindex = "\255\255\
\233\254\000\000\000\000\000\000\002\000\000\000\000\000\234\254\
\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000"

let yygindex = "\000\000\
\000\000\000\000\000\000\000\000"

let yytablesize = 3
let yytable = "\001\000\
\003\000\007\000\009\000"

let yycheck = "\001\000\
\024\001\000\000\025\001"

let yynames_const = "\
  INT\000\
  PLUS\000\
  MINUS\000\
  STAR\000\
  SLASH\000\
  EQUAL\000\
  EQUALEQUAL\000\
  LE\000\
  LT\000\
  GE\000\
  GT\000\
  NOT\000\
  AND\000\
  OR\000\
  IF\000\
  ELSE\000\
  WHILE\000\
  DO\000\
  READ\000\
  PRINT\000\
  SEMICOLON\000\
  LBRACE\000\
  RBRACE\000\
  LBLOCK\000\
  RBLOCK\000\
  LPAREN\000\
  RPAREN\000\
  EOF\000\
  "

let yynames_block = "\
  NUM\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'block) in
    Obj.repr(
# 15 "parser.mly"
              ( _1 )
# 149 "parser.ml"
               : S.program))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 2 : 'decls) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'stmts) in
    Obj.repr(
# 19 "parser.mly"
                              ( (_2,_3) )
# 157 "parser.ml"
               : 'block))
; (fun __caml_parser_env ->
    Obj.repr(
# 23 "parser.mly"
      ( [] )
# 163 "parser.ml"
               : 'decls))
; (fun __caml_parser_env ->
    Obj.repr(
# 27 "parser.mly"
      ( [] )
# 169 "parser.ml"
               : 'stmts))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : S.program)
