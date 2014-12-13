local
type t__1__ = (int*int)
type t__2__ = (int*int)
type t__3__ = (int*int)
type t__4__ = bool*(int*int)
type t__5__ = (int*int)
type t__6__ = char*(int*int)
type t__7__ = (int*int)
type t__8__ = (int*int)
type t__9__ = (int*int)
type t__10__ = (int*int)
type t__11__ = (int*int)
type t__12__ = (int*int)
type t__13__ = (int*int)
type t__14__ = (int*int)
type t__15__ = (int*int)
type t__16__ = (int*int)
type t__17__ = string*(int*int)
type t__18__ = (int*int)
type t__19__ = (int*int)
type t__20__ = (int*int)
type t__21__ = (int*int)
type t__22__ = (int*int)
type t__23__ = (int*int)
type t__24__ = (int*int)
type t__25__ = (int*int)
type t__26__ = (int*int)
type t__27__ = (int*int)
type t__28__ = (int*int)
type t__29__ = (int*int)
type t__30__ = (int*int)
type t__31__ = (int*int)
type t__32__ = int*(int*int)
type t__33__ = (int*int)
type t__34__ = (int*int)
type t__35__ = (int*int)
type t__36__ = (int*int)
type t__37__ = (int*int)
type t__38__ = (int*int)
type t__39__ = (int*int)
type t__40__ = (int*int)
type t__41__ = (int*int)
type t__42__ = (int*int)
type t__43__ = string*(int*int)
type t__44__ = (int*int)
type t__45__ = (int*int)
in
datatype token =
    ALL of t__1__
  | AND of t__2__
  | BOOL of t__3__
  | BOOLLIT of t__4__
  | CHAR of t__5__
  | CHARLIT of t__6__
  | COMMA of t__7__
  | DEQ of t__8__
  | DIV of t__9__
  | ELSE of t__10__
  | EOF of t__11__
  | EQ of t__12__
  | FILTER of t__13__
  | FN of t__14__
  | FUN of t__15__
  | GTH of t__16__
  | ID of t__17__
  | IF of t__18__
  | IN of t__19__
  | INT of t__20__
  | IOTA of t__21__
  | LBRACKET of t__22__
  | LCURLY of t__23__
  | LET of t__24__
  | LPAR of t__25__
  | LTH of t__26__
  | MAP of t__27__
  | MINUS of t__28__
  | MULT of t__29__
  | NEG of t__30__
  | NOT of t__31__
  | NUM of t__32__
  | OP of t__33__
  | OR of t__34__
  | PLUS of t__35__
  | RBRACKET of t__36__
  | RCURLY of t__37__
  | READ of t__38__
  | REDUCE of t__39__
  | REPLICATE of t__40__
  | RPAR of t__41__
  | SCAN of t__42__
  | STRINGLIT of t__43__
  | THEN of t__44__
  | WRITE of t__45__
end;

open Obj Parsing;
prim_val vector_ : int -> 'a -> 'a Vector.vector = 2 "make_vect";
prim_val update_ : 'a Vector.vector -> int -> 'a -> unit = 3 "set_vect_item";


(* A parser definition for Fasto, for use with mosmlyac. *)

open Fasto
open Fasto.UnknownTypes

(* Line 12, file Parser.sml *)
val yytransl = #[
  257 (* ALL *),
  258 (* AND *),
  259 (* BOOL *),
  260 (* BOOLLIT *),
  261 (* CHAR *),
  262 (* CHARLIT *),
  263 (* COMMA *),
  264 (* DEQ *),
  265 (* DIV *),
  266 (* ELSE *),
  267 (* EOF *),
  268 (* EQ *),
  269 (* FILTER *),
  270 (* FN *),
  271 (* FUN *),
  272 (* GTH *),
  273 (* ID *),
  274 (* IF *),
  275 (* IN *),
  276 (* INT *),
  277 (* IOTA *),
  278 (* LBRACKET *),
  279 (* LCURLY *),
  280 (* LET *),
  281 (* LPAR *),
  282 (* LTH *),
  283 (* MAP *),
  284 (* MINUS *),
  285 (* MULT *),
  286 (* NEG *),
  287 (* NOT *),
  288 (* NUM *),
  289 (* OP *),
  290 (* OR *),
  291 (* PLUS *),
  292 (* RBRACKET *),
  293 (* RCURLY *),
  294 (* READ *),
  295 (* REDUCE *),
  296 (* REPLICATE *),
  297 (* RPAR *),
  298 (* SCAN *),
  299 (* STRINGLIT *),
  300 (* THEN *),
  301 (* WRITE *),
    0];

val yylhs = "\255\255\
\\001\000\002\000\002\000\003\000\003\000\004\000\004\000\004\000\
\\004\000\005\000\005\000\009\000\009\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\\006\000\006\000\006\000\006\000\007\000\007\000\008\000\000\000";

val yylen = "\002\000\
\\002\000\003\000\002\000\007\000\006\000\001\000\001\000\001\000\
\\003\000\004\000\002\000\001\000\001\000\001\000\001\000\001\000\
\\001\000\001\000\003\000\003\000\003\000\003\000\003\000\002\000\
\\004\000\003\000\003\000\003\000\003\000\003\000\006\000\004\000\
\\003\000\004\000\004\000\004\000\006\000\008\000\006\000\009\000\
\\003\000\006\000\004\000\004\000\003\000\001\000\001\000\002\000";

val yydefred = "\000\000\
\\000\000\000\000\000\000\048\000\000\000\007\000\008\000\006\000\
\\000\000\000\000\000\000\001\000\000\000\002\000\000\000\009\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\015\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\014\000\000\000\000\000\000\000\018\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\010\000\000\000\000\000\
\\000\000\033\000\000\000\000\000\000\000\000\000\019\000\000\000\
\\041\000\047\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\044\000\043\000\032\000\000\000\036\000\045\000\
\\000\000\000\000\025\000\034\000\013\000\012\000\000\000\000\000\
\\000\000\035\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\039\000\000\000\000\000\037\000\000\000\000\000\
\\000\000\038\000\040\000";

val yydgoto = "\002\000\
\\004\000\005\000\010\000\019\000\020\000\050\000\051\000\083\000\
\\111\000";

val yysindex = "\004\000\
\\247\254\000\000\014\255\000\000\005\255\000\000\000\000\000\000\
\\014\255\247\254\004\255\000\000\246\254\000\000\002\255\000\000\
\\009\255\040\255\041\255\019\255\135\255\054\255\050\255\042\255\
\\000\000\000\000\029\255\135\255\043\255\135\255\048\255\135\255\
\\044\255\135\255\045\255\000\000\047\255\049\255\051\255\000\000\
\\052\255\017\002\014\255\135\255\135\255\135\255\092\255\055\255\
\\135\255\175\001\036\255\066\255\092\000\062\255\000\000\135\255\
\\014\255\243\254\135\255\135\255\135\255\135\255\135\255\135\255\
\\135\255\135\255\135\255\135\255\135\255\000\000\017\002\108\000\
\\145\001\000\000\025\255\135\255\130\000\135\255\000\000\135\255\
\\000\000\000\000\079\255\146\000\039\255\236\254\080\255\191\001\
\\168\000\000\000\170\255\000\000\073\255\170\255\001\255\000\000\
\\001\255\001\255\000\000\000\000\000\000\206\001\000\000\000\000\
\\228\001\135\255\000\000\000\000\000\000\000\000\081\255\135\255\
\\135\255\000\000\135\255\135\255\184\000\135\255\243\001\206\000\
\\017\002\017\002\000\000\002\002\135\255\000\000\135\255\222\000\
\\244\000\000\000\000\000";

val yyrindex = "\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\089\255\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\064\255\000\000\000\000\
\\000\000\000\000\174\255\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\007\255\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\252\254\000\000\000\000\000\000\000\000\212\255\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\038\255\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\250\255\077\001\032\000\101\001\115\001\017\001\070\000\
\\031\001\063\001\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\121\001\153\001\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000";

val yygindex = "\000\000\
\\000\000\093\000\000\000\255\255\068\000\235\255\210\255\054\000\
\\000\000";

val YYTABLESIZE = 820;
val yytable = "\042\000\
\\075\000\011\000\061\000\082\000\001\000\003\000\048\000\013\000\
\\109\000\063\000\053\000\006\000\055\000\007\000\110\000\012\000\
\\006\000\005\000\007\000\086\000\015\000\005\000\071\000\072\000\
\\073\000\016\000\017\000\077\000\008\000\067\000\009\000\104\000\
\\046\000\008\000\084\000\009\000\046\000\088\000\089\000\090\000\
\\091\000\092\000\093\000\094\000\095\000\096\000\097\000\098\000\
\\004\000\018\000\046\000\021\000\004\000\047\000\102\000\085\000\
\\061\000\022\000\105\000\023\000\043\000\044\000\062\000\063\000\
\\052\000\101\000\045\000\049\000\054\000\056\000\064\000\057\000\
\\079\000\058\000\061\000\059\000\060\000\080\000\082\000\108\000\
\\065\000\063\000\066\000\067\000\117\000\106\000\112\000\118\000\
\\068\000\069\000\119\000\120\000\024\000\121\000\122\000\025\000\
\\124\000\026\000\076\000\003\000\066\000\067\000\014\000\128\000\
\\011\000\129\000\068\000\069\000\027\000\028\000\070\000\087\000\
\\029\000\000\000\030\000\031\000\032\000\000\000\033\000\000\000\
\\000\000\034\000\035\000\036\000\000\000\000\000\000\000\000\000\
\\000\000\037\000\038\000\039\000\074\000\000\000\040\000\024\000\
\\041\000\000\000\025\000\000\000\026\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\027\000\
\\028\000\000\000\000\000\029\000\000\000\030\000\031\000\032\000\
\\000\000\033\000\000\000\000\000\034\000\035\000\036\000\000\000\
\\000\000\000\000\000\000\061\000\037\000\038\000\039\000\017\000\
\\000\000\040\000\063\000\041\000\017\000\017\000\017\000\017\000\
\\017\000\064\000\000\000\000\000\017\000\017\000\000\000\000\000\
\\017\000\000\000\000\000\000\000\000\000\066\000\067\000\017\000\
\\000\000\017\000\017\000\068\000\069\000\000\000\000\000\017\000\
\\017\000\017\000\017\000\000\000\000\000\024\000\017\000\000\000\
\\000\000\017\000\024\000\024\000\024\000\024\000\024\000\000\000\
\\000\000\000\000\024\000\024\000\000\000\000\000\024\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\024\000\000\000\024\000\
\\024\000\000\000\000\000\000\000\000\000\024\000\024\000\024\000\
\\024\000\000\000\000\000\026\000\024\000\000\000\000\000\024\000\
\\026\000\026\000\026\000\026\000\026\000\000\000\000\000\000\000\
\\026\000\026\000\000\000\000\000\026\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\026\000\000\000\026\000\026\000\000\000\
\\000\000\000\000\000\000\026\000\026\000\026\000\026\000\000\000\
\\000\000\023\000\026\000\000\000\000\000\026\000\023\000\023\000\
\\023\000\023\000\023\000\000\000\000\000\000\000\023\000\023\000\
\\000\000\000\000\023\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\023\000\000\000\023\000\023\000\000\000\000\000\000\000\
\\000\000\023\000\023\000\023\000\023\000\000\000\000\000\022\000\
\\023\000\000\000\000\000\023\000\022\000\022\000\022\000\022\000\
\\022\000\000\000\000\000\000\000\022\000\022\000\000\000\000\000\
\\022\000\000\000\000\000\000\000\000\000\061\000\000\000\022\000\
\\000\000\022\000\022\000\062\000\063\000\000\000\000\000\022\000\
\\022\000\022\000\022\000\064\000\000\000\061\000\022\000\000\000\
\\000\000\022\000\000\000\062\000\063\000\065\000\000\000\066\000\
\\067\000\000\000\000\000\064\000\000\000\068\000\069\000\000\000\
\\000\000\000\000\000\000\061\000\081\000\065\000\000\000\066\000\
\\067\000\062\000\063\000\000\000\000\000\068\000\069\000\000\000\
\\000\000\064\000\000\000\061\000\099\000\000\000\000\000\000\000\
\\000\000\062\000\063\000\065\000\000\000\066\000\067\000\000\000\
\\000\000\064\000\000\000\068\000\069\000\000\000\000\000\000\000\
\\000\000\061\000\103\000\065\000\000\000\066\000\067\000\062\000\
\\063\000\000\000\000\000\068\000\069\000\000\000\000\000\064\000\
\\000\000\061\000\107\000\000\000\000\000\000\000\000\000\062\000\
\\063\000\065\000\000\000\066\000\067\000\000\000\000\000\064\000\
\\000\000\068\000\069\000\000\000\000\000\000\000\000\000\061\000\
\\114\000\065\000\000\000\066\000\067\000\062\000\063\000\000\000\
\\000\000\068\000\069\000\000\000\000\000\064\000\000\000\061\000\
\\123\000\000\000\000\000\000\000\000\000\062\000\063\000\065\000\
\\000\000\066\000\067\000\000\000\000\000\064\000\000\000\068\000\
\\069\000\000\000\000\000\000\000\000\000\061\000\126\000\065\000\
\\000\000\066\000\067\000\062\000\063\000\000\000\000\000\068\000\
\\069\000\000\000\000\000\064\000\000\000\000\000\130\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\065\000\000\000\066\000\
\\067\000\000\000\000\000\000\000\000\000\068\000\069\000\021\000\
\\021\000\000\000\021\000\021\000\131\000\000\000\000\000\021\000\
\\021\000\000\000\000\000\021\000\000\000\027\000\027\000\000\000\
\\027\000\027\000\021\000\000\000\021\000\027\000\027\000\000\000\
\\000\000\027\000\021\000\021\000\021\000\021\000\000\000\000\000\
\\027\000\021\000\027\000\000\000\021\000\000\000\000\000\000\000\
\\027\000\027\000\027\000\027\000\000\000\020\000\020\000\027\000\
\\020\000\020\000\027\000\000\000\000\000\020\000\020\000\000\000\
\\000\000\020\000\000\000\028\000\028\000\000\000\028\000\028\000\
\\020\000\000\000\020\000\028\000\000\000\000\000\000\000\028\000\
\\020\000\020\000\020\000\020\000\000\000\000\000\028\000\020\000\
\\000\000\000\000\020\000\030\000\030\000\000\000\030\000\030\000\
\\028\000\028\000\000\000\030\000\000\000\028\000\000\000\030\000\
\\028\000\029\000\029\000\000\000\029\000\029\000\030\000\031\000\
\\000\000\029\000\031\000\031\000\000\000\029\000\000\000\031\000\
\\030\000\030\000\000\000\031\000\029\000\030\000\000\000\000\000\
\\030\000\000\000\061\000\000\000\000\000\000\000\029\000\029\000\
\\062\000\063\000\000\000\029\000\031\000\031\000\029\000\042\000\
\\064\000\031\000\042\000\042\000\031\000\000\000\000\000\042\000\
\\000\000\000\000\065\000\042\000\066\000\067\000\000\000\000\000\
\\061\000\000\000\068\000\069\000\100\000\078\000\062\000\063\000\
\\000\000\000\000\000\000\000\000\042\000\042\000\064\000\000\000\
\\061\000\042\000\000\000\000\000\042\000\113\000\062\000\063\000\
\\065\000\000\000\066\000\067\000\000\000\000\000\064\000\061\000\
\\068\000\069\000\000\000\000\000\000\000\062\000\063\000\115\000\
\\065\000\000\000\066\000\067\000\000\000\064\000\000\000\000\000\
\\068\000\069\000\000\000\000\000\000\000\061\000\000\000\065\000\
\\000\000\066\000\067\000\062\000\063\000\000\000\000\000\068\000\
\\069\000\000\000\000\000\064\000\061\000\000\000\116\000\000\000\
\\000\000\125\000\062\000\063\000\000\000\065\000\000\000\066\000\
\\067\000\000\000\064\000\061\000\000\000\068\000\069\000\000\000\
\\127\000\062\000\063\000\000\000\065\000\000\000\066\000\067\000\
\\000\000\064\000\061\000\000\000\068\000\069\000\000\000\000\000\
\\062\000\063\000\000\000\065\000\000\000\066\000\067\000\000\000\
\\064\000\000\000\000\000\068\000\069\000\000\000\000\000\000\000\
\\000\000\000\000\065\000\000\000\066\000\067\000\000\000\000\000\
\\000\000\000\000\068\000\069\000";

val yycheck = "\021\000\
\\047\000\003\000\002\001\017\001\001\000\015\001\028\000\009\000\
\\029\001\009\001\032\000\003\001\034\000\005\001\035\001\011\001\
\\003\001\011\001\005\001\033\001\017\001\015\001\044\000\045\000\
\\046\000\036\001\025\001\049\000\020\001\029\001\022\001\078\000\
\\037\001\020\001\056\000\022\001\041\001\059\000\060\000\061\000\
\\062\000\063\000\064\000\065\000\066\000\067\000\068\000\069\000\
\\011\001\041\001\022\001\012\001\015\001\025\001\076\000\057\000\
\\002\001\017\001\080\000\041\001\007\001\012\001\008\001\009\001\
\\017\001\041\001\025\001\025\001\025\001\025\001\016\001\025\001\
\\037\001\025\001\002\001\025\001\025\001\012\001\017\001\041\001\
\\026\001\009\001\028\001\029\001\106\000\007\001\007\001\007\001\
\\034\001\035\001\112\000\113\000\001\001\115\000\116\000\004\001\
\\118\000\006\001\044\001\011\001\028\001\029\001\010\000\125\000\
\\041\001\127\000\034\001\035\001\017\001\018\001\043\000\058\000\
\\021\001\255\255\023\001\024\001\025\001\255\255\027\001\255\255\
\\255\255\030\001\031\001\032\001\255\255\255\255\255\255\255\255\
\\255\255\038\001\039\001\040\001\041\001\255\255\043\001\001\001\
\\045\001\255\255\004\001\255\255\006\001\255\255\255\255\255\255\
\\255\255\255\255\255\255\255\255\255\255\255\255\255\255\017\001\
\\018\001\255\255\255\255\021\001\255\255\023\001\024\001\025\001\
\\255\255\027\001\255\255\255\255\030\001\031\001\032\001\255\255\
\\255\255\255\255\255\255\002\001\038\001\039\001\040\001\002\001\
\\255\255\043\001\009\001\045\001\007\001\008\001\009\001\010\001\
\\011\001\016\001\255\255\255\255\015\001\016\001\255\255\255\255\
\\019\001\255\255\255\255\255\255\255\255\028\001\029\001\026\001\
\\255\255\028\001\029\001\034\001\035\001\255\255\255\255\034\001\
\\035\001\036\001\037\001\255\255\255\255\002\001\041\001\255\255\
\\255\255\044\001\007\001\008\001\009\001\010\001\011\001\255\255\
\\255\255\255\255\015\001\016\001\255\255\255\255\019\001\255\255\
\\255\255\255\255\255\255\255\255\255\255\026\001\255\255\028\001\
\\029\001\255\255\255\255\255\255\255\255\034\001\035\001\036\001\
\\037\001\255\255\255\255\002\001\041\001\255\255\255\255\044\001\
\\007\001\008\001\009\001\010\001\011\001\255\255\255\255\255\255\
\\015\001\016\001\255\255\255\255\019\001\255\255\255\255\255\255\
\\255\255\255\255\255\255\026\001\255\255\028\001\029\001\255\255\
\\255\255\255\255\255\255\034\001\035\001\036\001\037\001\255\255\
\\255\255\002\001\041\001\255\255\255\255\044\001\007\001\008\001\
\\009\001\010\001\011\001\255\255\255\255\255\255\015\001\016\001\
\\255\255\255\255\019\001\255\255\255\255\255\255\255\255\255\255\
\\255\255\026\001\255\255\028\001\029\001\255\255\255\255\255\255\
\\255\255\034\001\035\001\036\001\037\001\255\255\255\255\002\001\
\\041\001\255\255\255\255\044\001\007\001\008\001\009\001\010\001\
\\011\001\255\255\255\255\255\255\015\001\016\001\255\255\255\255\
\\019\001\255\255\255\255\255\255\255\255\002\001\255\255\026\001\
\\255\255\028\001\029\001\008\001\009\001\255\255\255\255\034\001\
\\035\001\036\001\037\001\016\001\255\255\002\001\041\001\255\255\
\\255\255\044\001\255\255\008\001\009\001\026\001\255\255\028\001\
\\029\001\255\255\255\255\016\001\255\255\034\001\035\001\255\255\
\\255\255\255\255\255\255\002\001\041\001\026\001\255\255\028\001\
\\029\001\008\001\009\001\255\255\255\255\034\001\035\001\255\255\
\\255\255\016\001\255\255\002\001\041\001\255\255\255\255\255\255\
\\255\255\008\001\009\001\026\001\255\255\028\001\029\001\255\255\
\\255\255\016\001\255\255\034\001\035\001\255\255\255\255\255\255\
\\255\255\002\001\041\001\026\001\255\255\028\001\029\001\008\001\
\\009\001\255\255\255\255\034\001\035\001\255\255\255\255\016\001\
\\255\255\002\001\041\001\255\255\255\255\255\255\255\255\008\001\
\\009\001\026\001\255\255\028\001\029\001\255\255\255\255\016\001\
\\255\255\034\001\035\001\255\255\255\255\255\255\255\255\002\001\
\\041\001\026\001\255\255\028\001\029\001\008\001\009\001\255\255\
\\255\255\034\001\035\001\255\255\255\255\016\001\255\255\002\001\
\\041\001\255\255\255\255\255\255\255\255\008\001\009\001\026\001\
\\255\255\028\001\029\001\255\255\255\255\016\001\255\255\034\001\
\\035\001\255\255\255\255\255\255\255\255\002\001\041\001\026\001\
\\255\255\028\001\029\001\008\001\009\001\255\255\255\255\034\001\
\\035\001\255\255\255\255\016\001\255\255\255\255\041\001\255\255\
\\255\255\255\255\255\255\255\255\255\255\026\001\255\255\028\001\
\\029\001\255\255\255\255\255\255\255\255\034\001\035\001\007\001\
\\008\001\255\255\010\001\011\001\041\001\255\255\255\255\015\001\
\\016\001\255\255\255\255\019\001\255\255\007\001\008\001\255\255\
\\010\001\011\001\026\001\255\255\028\001\015\001\016\001\255\255\
\\255\255\019\001\034\001\035\001\036\001\037\001\255\255\255\255\
\\026\001\041\001\028\001\255\255\044\001\255\255\255\255\255\255\
\\034\001\035\001\036\001\037\001\255\255\007\001\008\001\041\001\
\\010\001\011\001\044\001\255\255\255\255\015\001\016\001\255\255\
\\255\255\019\001\255\255\007\001\008\001\255\255\010\001\011\001\
\\026\001\255\255\028\001\015\001\255\255\255\255\255\255\019\001\
\\034\001\035\001\036\001\037\001\255\255\255\255\026\001\041\001\
\\255\255\255\255\044\001\007\001\008\001\255\255\010\001\011\001\
\\036\001\037\001\255\255\015\001\255\255\041\001\255\255\019\001\
\\044\001\007\001\008\001\255\255\010\001\011\001\026\001\007\001\
\\255\255\015\001\010\001\011\001\255\255\019\001\255\255\015\001\
\\036\001\037\001\255\255\019\001\026\001\041\001\255\255\255\255\
\\044\001\255\255\002\001\255\255\255\255\255\255\036\001\037\001\
\\008\001\009\001\255\255\041\001\036\001\037\001\044\001\007\001\
\\016\001\041\001\010\001\011\001\044\001\255\255\255\255\015\001\
\\255\255\255\255\026\001\019\001\028\001\029\001\255\255\255\255\
\\002\001\255\255\034\001\035\001\036\001\007\001\008\001\009\001\
\\255\255\255\255\255\255\255\255\036\001\037\001\016\001\255\255\
\\002\001\041\001\255\255\255\255\044\001\007\001\008\001\009\001\
\\026\001\255\255\028\001\029\001\255\255\255\255\016\001\002\001\
\\034\001\035\001\255\255\255\255\255\255\008\001\009\001\010\001\
\\026\001\255\255\028\001\029\001\255\255\016\001\255\255\255\255\
\\034\001\035\001\255\255\255\255\255\255\002\001\255\255\026\001\
\\255\255\028\001\029\001\008\001\009\001\255\255\255\255\034\001\
\\035\001\255\255\255\255\016\001\002\001\255\255\019\001\255\255\
\\255\255\007\001\008\001\009\001\255\255\026\001\255\255\028\001\
\\029\001\255\255\016\001\002\001\255\255\034\001\035\001\255\255\
\\007\001\008\001\009\001\255\255\026\001\255\255\028\001\029\001\
\\255\255\016\001\002\001\255\255\034\001\035\001\255\255\255\255\
\\008\001\009\001\255\255\026\001\255\255\028\001\029\001\255\255\
\\016\001\255\255\255\255\034\001\035\001\255\255\255\255\255\255\
\\255\255\255\255\026\001\255\255\028\001\029\001\255\255\255\255\
\\255\255\255\255\034\001\035\001";

val yyact = vector_ 49 (fn () => ((raise Fail "parser") : obj));
(* Rule 1, file Parser.grm, line 42 *)
val _ = update_ yyact 1
(fn () => repr(let
val d__1__ = peekVal 1 : Fasto.UnknownTypes.FunDec list
val d__2__ = peekVal 0 : (int*int)
in
( (d__1__) ) end : Fasto.UnknownTypes.Prog))
;
(* Rule 2, file Parser.grm, line 45 *)
val _ = update_ yyact 2
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Fasto.UnknownTypes.FunDec
val d__3__ = peekVal 0 : Fasto.UnknownTypes.FunDec list
in
( (d__2__) :: (d__3__) ) end : Fasto.UnknownTypes.FunDec list))
;
(* Rule 3, file Parser.grm, line 46 *)
val _ = update_ yyact 3
(fn () => repr(let
val d__1__ = peekVal 1 : (int*int)
val d__2__ = peekVal 0 : Fasto.UnknownTypes.FunDec
in
( (d__2__) :: [] ) end : Fasto.UnknownTypes.FunDec list))
;
(* Rule 4, file Parser.grm, line 50 *)
val _ = update_ yyact 4
(fn () => repr(let
val d__1__ = peekVal 6 : Fasto.Type
val d__2__ = peekVal 5 : string*(int*int)
val d__3__ = peekVal 4 : (int*int)
val d__4__ = peekVal 3 : Fasto.Param list
val d__5__ = peekVal 2 : (int*int)
val d__6__ = peekVal 1 : (int*int)
val d__7__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( FunDec (#1 (d__2__), (d__1__), (d__4__), (d__7__), #2 (d__2__)) ) end : Fasto.UnknownTypes.FunDec))
;
(* Rule 5, file Parser.grm, line 52 *)
val _ = update_ yyact 5
(fn () => repr(let
val d__1__ = peekVal 5 : Fasto.Type
val d__2__ = peekVal 4 : string*(int*int)
val d__3__ = peekVal 3 : (int*int)
val d__4__ = peekVal 2 : (int*int)
val d__5__ = peekVal 1 : (int*int)
val d__6__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( FunDec (#1 (d__2__), (d__1__), [], (d__6__), #2 (d__2__)) ) end : Fasto.UnknownTypes.FunDec))
;
(* Rule 6, file Parser.grm, line 55 *)
val _ = update_ yyact 6
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Int ) end : Fasto.Type))
;
(* Rule 7, file Parser.grm, line 56 *)
val _ = update_ yyact 7
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Bool ) end : Fasto.Type))
;
(* Rule 8, file Parser.grm, line 57 *)
val _ = update_ yyact 8
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( Char ) end : Fasto.Type))
;
(* Rule 9, file Parser.grm, line 58 *)
val _ = update_ yyact 9
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Fasto.Type
val d__3__ = peekVal 0 : (int*int)
in
( Array (d__2__) ) end : Fasto.Type))
;
(* Rule 10, file Parser.grm, line 61 *)
val _ = update_ yyact 10
(fn () => repr(let
val d__1__ = peekVal 3 : Fasto.Type
val d__2__ = peekVal 2 : string*(int*int)
val d__3__ = peekVal 1 : (int*int)
val d__4__ = peekVal 0 : Fasto.Param list
in
( Param (#1 (d__2__), (d__1__)) :: (d__4__) ) end : Fasto.Param list))
;
(* Rule 11, file Parser.grm, line 62 *)
val _ = update_ yyact 11
(fn () => repr(let
val d__1__ = peekVal 1 : Fasto.Type
val d__2__ = peekVal 0 : string*(int*int)
in
( Param (#1 (d__2__), (d__1__)) :: [] ) end : Fasto.Param list))
;
(* Rule 12, file Parser.grm, line 65 *)
val _ = update_ yyact 12
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( (Lambda
                           (Int, [Param ("x", Int),
                                  Param ("y", Int)],
                            Plus (Var ("x", (d__1__)),
                                  Var ("y", (d__1__)),
                                                (d__1__)) ,(d__1__)))
                        ) end : Fasto.UnknownTypes.FunArg))
;
(* Rule 13, file Parser.grm, line 73 *)
val _ = update_ yyact 13
(fn () => repr(let
val d__1__ = peekVal 0 : (int*int)
in
( (Lambda
                           (Int, [Param ("x", Int),
                                        Param ("y", Int)],
                            Times (Var ("x", (d__1__)),
                                        Var ("y", (d__1__)),
                                        (d__1__)) ,(d__1__)))
                        ) end : Fasto.UnknownTypes.FunArg))
;
(* Rule 14, file Parser.grm, line 82 *)
val _ = update_ yyact 14
(fn () => repr(let
val d__1__ = peekVal 0 : int*(int*int)
in
( Constant (IntVal (#1 (d__1__)), #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 15, file Parser.grm, line 83 *)
val _ = update_ yyact 15
(fn () => repr(let
val d__1__ = peekVal 0 : bool*(int*int)
in
( Constant (BoolVal (#1 (d__1__)), #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 16, file Parser.grm, line 84 *)
val _ = update_ yyact 16
(fn () => repr(let
val d__1__ = peekVal 0 : char*(int*int)
in
( Constant (CharVal (#1 (d__1__)), #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 17, file Parser.grm, line 85 *)
val _ = update_ yyact 17
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( Var (d__1__) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 18, file Parser.grm, line 86 *)
val _ = update_ yyact 18
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( StringLit (d__1__) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 19, file Parser.grm, line 88 *)
val _ = update_ yyact 19
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Fasto.UnknownTypes.Exp list
val d__3__ = peekVal 0 : (int*int)
in
( ArrayLit ((d__2__), (), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 20, file Parser.grm, line 89 *)
val _ = update_ yyact 20
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Plus ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 21, file Parser.grm, line 90 *)
val _ = update_ yyact 21
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Minus((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 22, file Parser.grm, line 91 *)
val _ = update_ yyact 22
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Times ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 23, file Parser.grm, line 92 *)
val _ = update_ yyact 23
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Divide  ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 24, file Parser.grm, line 93 *)
val _ = update_ yyact 24
(fn () => repr(let
val d__1__ = peekVal 1 : (int*int)
val d__2__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Negate  ((d__2__), (d__1__))     ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 25, file Parser.grm, line 94 *)
val _ = update_ yyact 25
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__4__ = peekVal 0 : (int*int)
in
( Not  ((d__3__), (d__1__))  ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 26, file Parser.grm, line 95 *)
val _ = update_ yyact 26
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( And  ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 27, file Parser.grm, line 96 *)
val _ = update_ yyact 27
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Or   ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 28, file Parser.grm, line 97 *)
val _ = update_ yyact 28
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Equal((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 29, file Parser.grm, line 98 *)
val _ = update_ yyact 29
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Less ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 30, file Parser.grm, line 99 *)
val _ = update_ yyact 30
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Less ((d__1__), (d__3__), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 31, file Parser.grm, line 102 *)
val _ = update_ yyact 31
(fn () => repr(let
val d__1__ = peekVal 5 : (int*int)
val d__2__ = peekVal 4 : Fasto.UnknownTypes.Exp
val d__3__ = peekVal 3 : (int*int)
val d__4__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__5__ = peekVal 1 : (int*int)
val d__6__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( If ((d__2__), (d__4__), (d__6__), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 32, file Parser.grm, line 104 *)
val _ = update_ yyact 32
(fn () => repr(let
val d__1__ = peekVal 3 : string*(int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.UnknownTypes.Exp list
val d__4__ = peekVal 0 : (int*int)
in
( Apply (#1 (d__1__), (d__3__), #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 33, file Parser.grm, line 106 *)
val _ = update_ yyact 33
(fn () => repr(let
val d__1__ = peekVal 2 : string*(int*int)
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : (int*int)
in
( Apply (#1 (d__1__), [], #2 (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 34, file Parser.grm, line 109 *)
val _ = update_ yyact 34
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.Type
val d__4__ = peekVal 0 : (int*int)
in
( Read ((d__3__), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 35, file Parser.grm, line 111 *)
val _ = update_ yyact 35
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__4__ = peekVal 0 : (int*int)
in
( Write ((d__3__), (), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 36, file Parser.grm, line 113 *)
val _ = update_ yyact 36
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__4__ = peekVal 0 : (int*int)
in
( Iota ((d__3__), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 37, file Parser.grm, line 115 *)
val _ = update_ yyact 37
(fn () => repr(let
val d__1__ = peekVal 5 : (int*int)
val d__2__ = peekVal 4 : (int*int)
val d__3__ = peekVal 3 : Fasto.UnknownTypes.Exp
val d__4__ = peekVal 2 : (int*int)
val d__5__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__6__ = peekVal 0 : (int*int)
in
( Replicate ((d__3__), (d__5__), (), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 38, file Parser.grm, line 117 *)
val _ = update_ yyact 38
(fn () => repr(let
val d__1__ = peekVal 7 : (int*int)
val d__2__ = peekVal 6 : (int*int)
val d__3__ = peekVal 5 : Fasto.UnknownTypes.FunArg
val d__4__ = peekVal 4 : (int*int)
val d__5__ = peekVal 3 : Fasto.UnknownTypes.Exp
val d__6__ = peekVal 2 : (int*int)
val d__7__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__8__ = peekVal 0 : (int*int)
in
( Reduce ((d__3__), (d__5__), (d__7__), (), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 39, file Parser.grm, line 119 *)
val _ = update_ yyact 39
(fn () => repr(let
val d__1__ = peekVal 5 : (int*int)
val d__2__ = peekVal 4 : (int*int)
val d__3__ = peekVal 3 : Fasto.UnknownTypes.FunArg
val d__4__ = peekVal 2 : (int*int)
val d__5__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__6__ = peekVal 0 : (int*int)
in
( Map ((d__3__), (d__5__), (), (), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 40, file Parser.grm, line 121 *)
val _ = update_ yyact 40
(fn () => repr(let
val d__1__ = peekVal 8 : (int*int)
val d__2__ = peekVal 7 : (int*int)
val d__3__ = peekVal 6 : (int*int)
val d__4__ = peekVal 5 : Fasto.UnknownTypes.FunArg
val d__5__ = peekVal 4 : (int*int)
val d__6__ = peekVal 3 : Fasto.UnknownTypes.Exp
val d__7__ = peekVal 2 : (int*int)
val d__8__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__9__ = peekVal 0 : (int*int)
in
( Reduce ((d__4__), (d__6__), (d__8__), (), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 41, file Parser.grm, line 122 *)
val _ = update_ yyact 41
(fn () => repr(let
val d__1__ = peekVal 2 : (int*int)
val d__2__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__3__ = peekVal 0 : (int*int)
in
( (d__2__) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 42, file Parser.grm, line 124 *)
val _ = update_ yyact 42
(fn () => repr(let
val d__1__ = peekVal 5 : (int*int)
val d__2__ = peekVal 4 : string*(int*int)
val d__3__ = peekVal 3 : (int*int)
val d__4__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__5__ = peekVal 1 : (int*int)
val d__6__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( Let (Dec (#1 (d__2__), (d__4__), (d__3__)), (d__6__), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 43, file Parser.grm, line 126 *)
val _ = update_ yyact 43
(fn () => repr(let
val d__1__ = peekVal 3 : string*(int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__4__ = peekVal 0 : (int*int)
in
( Index (#1 (d__1__), (d__3__), (), (d__2__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 44, file Parser.grm, line 128 *)
val _ = update_ yyact 44
(fn () => repr(let
val d__1__ = peekVal 3 : (int*int)
val d__2__ = peekVal 2 : (int*int)
val d__3__ = peekVal 1 : Fasto.UnknownTypes.Exp
val d__4__ = peekVal 0 : (int*int)
in
( All ((d__3__), (d__1__)) ) end : Fasto.UnknownTypes.Exp))
;
(* Rule 45, file Parser.grm, line 131 *)
val _ = update_ yyact 45
(fn () => repr(let
val d__1__ = peekVal 2 : Fasto.UnknownTypes.Exp
val d__2__ = peekVal 1 : (int*int)
val d__3__ = peekVal 0 : Fasto.UnknownTypes.Exp list
in
( (d__1__) :: (d__3__) ) end : Fasto.UnknownTypes.Exp list))
;
(* Rule 46, file Parser.grm, line 132 *)
val _ = update_ yyact 46
(fn () => repr(let
val d__1__ = peekVal 0 : Fasto.UnknownTypes.Exp
in
( (d__1__) :: [] ) end : Fasto.UnknownTypes.Exp list))
;
(* Rule 47, file Parser.grm, line 135 *)
val _ = update_ yyact 47
(fn () => repr(let
val d__1__ = peekVal 0 : string*(int*int)
in
( FunName (#1 (d__1__)) ) end : Fasto.UnknownTypes.FunArg))
;
(* Entry Prog *)
val _ = update_ yyact 48 (fn () => raise yyexit (peekVal 0));
val yytables : parseTables =
  ( yyact,
    yytransl,
    yylhs,
    yylen,
    yydefred,
    yydgoto,
    yysindex,
    yyrindex,
    yygindex,
    YYTABLESIZE,
    yytable,
    yycheck );
fun Prog lexer lexbuf = yyparse yytables 1 lexer lexbuf;
