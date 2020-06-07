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

let id = let open Graph in [
  "C", Conclusion 1;
  "F", ArrowI;
], [
  ("F", 1) --> ("F", 1);
  ("F", 2) --> ("C", 1);
]

let const = let open Graph in [
  "H", Assumption 1;
  "C", Conclusion 1;
  "F", ArrowI;
], [
  ("H", 1) --> ("F", 1);
  ("F", 2) --> ("C", 1);
]

let left = let open Graph in [
  "H", Assumption 1;
  "C", Conclusion 1;
  "L", DisjIL;
], [
  ("H", 1) --> ("L", 1);
  ("L", 1) --> ("C", 1);
]

let light = let open Graph in [
  "H", Assumption 1;
  "C", Conclusion 1;
  "M", DisjE;
], [
  ("H", 1) --> ("M", 1);
  ("M", 1) --> ("M", 2);
  ("M", 2) --> ("M", 3);
  ("M", 3) --> ("C", 1);
]

let () =
  print_endline @@ Expr.(show @@ of_graph swap "C");
  print_endline @@ Expr.(show @@ of_graph assoc "C");
  print_endline @@ Expr.(show @@ of_graph mp "C");
  print_endline @@ Expr.(show @@ of_graph mp2 "C");
  print_endline @@ Expr.(show @@ of_graph id "C");
  print_endline @@ Expr.(show @@ of_graph const "C");
  print_endline @@ Expr.(show @@ of_graph left "C");
  print_endline @@ Expr.(show @@ of_graph light "C");
