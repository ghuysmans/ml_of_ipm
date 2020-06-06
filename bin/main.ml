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

let assoc = let open Graph in [
  "H", Assumption 1;
  "C", Conclusion 1;
  "E", ConjE;
  "E'", ConjE;
  "I", ConjI;
  "I'", ConjI;
], [
  ("H", 1) --> ("E", 1);
  ("E", 1) --> ("E'", 1);
  ("E", 2) --> ("I", 2);
  ("E'", 2) --> ("I", 1);
  ("E'", 1) --> ("I'", 1);
  ("I", 1) --> ("I'", 2);
  ("I'", 1) --> ("C", 1);
]

let mp = let open Graph in [
  "H", Assumption 1;
  "H'", Assumption 2;
  "C", Conclusion 1;
  "A", ArrowE;
], [
  ("H", 1) --> ("A", 2);
  ("H'", 1) --> ("A", 1);
  ("A", 1) --> ("C", 1);
]

let mp2 = let open Graph in [
  "H", Assumption 1;
  "H'", Assumption 2;
  "H''", Assumption 3;
  "C", Conclusion 1;
  "A", ArrowE;
  "A'", ArrowE;
], [
  ("H", 1) --> ("A", 2);
  ("H'", 1) --> ("A", 1);
  ("A", 1) --> ("A'", 2);
  ("H''", 1) --> ("A'", 1);
  ("A'", 1) --> ("C", 1);
]

let () =
  print_endline @@ Expr.(show @@ of_graph swap "C");
  print_endline @@ Expr.(show @@ of_graph assoc "C");
  print_endline @@ Expr.(show @@ of_graph mp "C");
  print_endline @@ Expr.(show @@ of_graph mp2 "C");
