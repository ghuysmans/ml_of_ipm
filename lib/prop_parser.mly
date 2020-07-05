%token <char> ID
%token AND
%token LPAREN
%token RPAREN
%token ARROW
%token OR
%token FALSE
%token FORALL
%token EXISTS
%token DOT
%token <string> VAR
%token COMMA
%token EOF
%right ARROW
%nonassoc OR
%nonassoc AND
%start <Prop.t> parse
%%
arg:
  | VAR { Prop.Var $1 }
  | VAR LPAREN args RPAREN { Prop.App ($1, $3) }
args:
  | arg { [$1] }
  | arg COMMA args { $1 :: $3 }
prop:
  | ID { Prop.Prop $1 }
  | LPAREN prop RPAREN { $2 }
  | FALSE { Prop.False }
  | prop AND prop { Prop.Conj ($1, $3) }
  | prop OR prop { Prop.Disj ($1, $3) }
  | prop ARROW prop { Prop.Imp ($1, $3) }
  | FORALL VAR DOT prop { Prop.Forall ($2, $4) }
  | EXISTS VAR DOT prop { Prop.Exists ($2, $4) }
  | ID LPAREN args RPAREN { Prop.Pred (String.make 1 $1, $3) }
  | VAR LPAREN args RPAREN { Prop.Pred ($1, $3) }
parse: prop EOF { $1 }
