rule top = parse
| ' ' { top lexbuf }
| ['A'-'Z'] as l { Prop_parser.ID l }
| "∧" | "/\\" { Prop_parser.AND }
| '(' { Prop_parser.LPAREN }
| ')' { Prop_parser.RPAREN }
| "→" | "->" { Prop_parser.ARROW }
| "∨" | "\\/" { Prop_parser.OR }
| "∀" | "forall" { Prop_parser.FORALL }
| "∃" | "exists" { Prop_parser.EXISTS }
| '.' { Prop_parser.DOT }
| (['a'-'z']|"₁")+ as n { Prop_parser.VAR n }
| ',' { Prop_parser.COMMA }
| "⊥" | "False" { Prop_parser.FALSE }
| eof { Prop_parser.EOF }
