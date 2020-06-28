let id = ['A'-'Z']

rule top = parse
| ' ' { top lexbuf }
| id as l { Prop_parser.ID l }
| "∧" | "/\\" { Prop_parser.AND }
| '(' { Prop_parser.LPAREN }
| ')' { Prop_parser.RPAREN }
| "→" | "->" { Prop_parser.ARROW }
| "∨" | "\\/" { Prop_parser.OR }
| "⊥" | "False" { Prop_parser.FALSE }
| eof { Prop_parser.EOF }
