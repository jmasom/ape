%{open Types%}

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
%type <repl_ast> main
%type <repl_ast> expression
%type <repl_ast> sequence_expression
%type <repl_ast> repeated_expression
%type <repl_ast> primary_expression
%%

let main :=
  ~ = expression; EOF; <>

let expression :=
  ~ = sequence_expression; <>

let sequence_expression :=
  | ~ = repeated_expression; <>
  | head = repeated_expression; tail = repeated_expression+; { Trepl_seq (head :: tail) }

let repeated_expression :=
  | ~ = feat_transformation_expression; <>
  | exp = feat_transformation_expression; OPEN_BRACE; count = INT; CLOSE_BRACE;
    { Trepl_repeated (exp, count) }

let feat_transformation_expression :=
  | ~ = primary_expression; <>
  | exp = primary_expression; ARROW; feature_spec = feature_spec;
    { Trepl_feature_spec_appl (exp, feature_spec) }

let primary_expression :=
  | ~ = LITERAL; < Trepl_literal >
  | ~ = ID; < Trepl_label_ref >
  | OPEN_PAREN; exp = expression; CLOSE_PAREN; { exp }

let feature_spec ==
  | OPEN_BRACK; ~ = feature*; CLOSE_BRACK; <>

let feature ==
  | PLUS; id = ID;  { (id, true) }
  | MINUS; id = ID; { (id, false) }
