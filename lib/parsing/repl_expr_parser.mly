%{open Repl_expr_ast%}

%token <string> LITERAL
%token <string> ID
%token <int> INT

(* feature specification application infix, bookends, and values *)
%token ARROW
%token OPEN_BRACK
%token CLOSE_BRACK
%token PLUS
%token MINUS

(* group bookends *)
%token OPEN_PAREN
%token CLOSE_PAREN

(* repeat bookends *)
%token OPEN_BRACE
%token CLOSE_BRACE

%token EOF

%start main
%type <t> main
%type <t> expression
%type <t> sequence_expression
%type <t> repeated_expression
%type <t> primary_expression
%%

let main :=
  ~ = expression; EOF; <>

let expression :=
  ~ = sequence_expression; <>

let sequence_expression :=
  | ~ = repeated_expression; <>
  | head = repeated_expression; tail = repeated_expression+; { Seq (head :: tail) }

let repeated_expression :=
  | ~ = feat_transformation_expression; <>
  | exp = feat_transformation_expression; OPEN_BRACE; count = INT; CLOSE_BRACE;
    { Repeated (exp, count) }

let feat_transformation_expression :=
  | ~ = primary_expression; <>
  | exp = primary_expression; ARROW; feature_spec = feature_spec;
    { Feature_spec_appl (exp, feature_spec) }

let primary_expression :=
  | ~ = LITERAL; < Literal >
  | ~ = ID; < Label_ref >
  | OPEN_PAREN; exp = expression; CLOSE_PAREN; { exp }

let feature_spec ==
  | OPEN_BRACK; ~ = feature*; CLOSE_BRACK; <>

let feature ==
  | PLUS; id = ID;  { (id, true) }
  | MINUS; id = ID; { (id, false) }