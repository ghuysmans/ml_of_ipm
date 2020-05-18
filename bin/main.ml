open Compiler

let swap = let open Graph in [
  "H", Assumption 1;
  "C", Conclusion 1;
  "E", ConjE;
  "I", ConjI;
], [
  ("H", 1) --> ("E", 1);
  ("E", 1) --> ("I", 2);
  ("E", 2) --> ("I", 1);
  ("I", 1) --> ("C", 1);
]

let () =
  print_endline @@ Expr.(show @@ of_graph swap "C")
