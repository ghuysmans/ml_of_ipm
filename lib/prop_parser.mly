%token <char> ID
%token AND
%token LPAREN
%token RPAREN
%token ARROW
%token OR
%token FALSE
%token EOF
%right ARROW
%nonassoc OR
%nonassoc AND
%start <Prop.t> parse
%%
prop:
  | ID { Prop.Prop $1 }
  | LPAREN prop RPAREN { $2 }
  | FALSE { Prop.False }
  | prop AND prop { Prop.Conj ($1, $3) }
  | prop OR prop { Prop.Disj ($1, $3) }
  | prop ARROW prop { Prop.Imp ($1, $3) }
parse: prop EOF { $1 }
