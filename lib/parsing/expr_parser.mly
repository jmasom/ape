%{open Expr_ast%}

%token <string> LITERAL
%token <string> ID
%token <float> FLOAT
%token <int> INT

(* feature specification bookends and values *)
%token OPEN_BRACK
%token CLOSE_BRACK
%token PLUS
%token MINUS

(* group bookends *)
%token OPEN_PAREN
%token CLOSE_PAREN

(* label delimiter *)
%token COLON

(* optional marker *)
%token Q_MARK

(* repeat bookends and delimiter *)
%token OPEN_BRACE
%token CLOSE_BRACE
%token COMMA

(* weight delimiter *)
%token HASH

(* choice delimiter *)
%token PIPE

(* wildcard for unrecognized characters *)
%token <string> UNKNOWN_LEXEME

%token EOF

%start main
%type <t> main
%type <t> expression
%type <t> choice_expression
%type <t> sequence_expression
%type <t> labeled_expression
%type <t> optional_expression
%type <t> repeated_expression
%type <repeat_bounds> repeat_bounds
%type <t> primary_expression
%%

let main :=
  ~ = expression; EOF; <>

let expression :=
  ~ = choice_expression; <>

let choice_expression :=
  | ~ = sequence_expression; <>
  | ~ = separated_nonempty_list(PIPE, weighted_expression); < Choice  >

let weighted_expression :=
  | exp = sequence_expression; { (exp, None) }
  | exp = primary_expression; HASH; weight = float; { (exp, Some weight) }

let sequence_expression :=
  | ~ = labeled_expression; <>
  | head = labeled_expression; tail = labeled_expression+; { Seq (head :: tail) }

let labeled_expression :=
  | ~ = optional_expression; <>
  | label = ID; COLON; exp = optional_expression; { Labeled (label, exp) }

let optional_expression :=
  | ~ = repeated_expression; <>
  | exp = primary_expression; Q_MARK; prob = option(float); { Optional (exp, prob) }

let repeated_expression :=
  | ~ = primary_expression; <>
  | exp = primary_expression; OPEN_BRACE; bounds = repeat_bounds; CLOSE_BRACE;
    { Repeated (exp, bounds) }

let repeat_bounds :=
  | min = option(INT); COMMA; max = INT; { Range (min, max) }
  | count = INT; { Count count }

let primary_expression :=
  | ~ = LITERAL; < Literal >
  | ~ = ID; < Rule_ref >
  | OPEN_BRACK; ~ = feature*; CLOSE_BRACK; < Feature_spec >
  | OPEN_PAREN; exp = expression; CLOSE_PAREN; { exp }

let feature ==
  | PLUS; id = ID;  { (id, true) }
  | MINUS; id = ID; { (id, false) }

let float ==
  | ~ = FLOAT; <>
  | n = INT; { float_of_int n }