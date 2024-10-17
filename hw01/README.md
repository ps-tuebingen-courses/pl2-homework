# Lab 00 (before first homework)

- abstract / concrete grammars & -STs (parenthesis!)
   - example arithmetic with +,-,*,/
     `` grammar
     atom ::= number | '(' term ')'
     factor ::= factor '*' atom | factor '/' atom
     expr ::= expr '+' factor | expr '-' factor
     term ::= 'ifzero' expr 'then' expr 'else' expr
     ``
     vs
     `` grammar
     e ::= number | t '+' t | t '-' t | t '*' t | t '/' t | 'ifzero' t 'then' t 'else' t
     ``

- Transform between different styles discussed in the lecture
  - Namely:
    - BNF
    - Inference rules
      - Next time: Draw derivation tree
    - explicit inductive definition ("smallest set T closed under... \in/\subseteq T")
    - limit of a series of sets ("Terms, concretely", see slide 27, S_0, S_i+1), 
         - show S_i \subseteq S_i+1
  - also: representation in code?

`` grammar
p ::= p above  p
    | p beside p
    | framed p
    | circle
    | triangle
    | square
    | line
``

- Induction on terms
  - TODO example?
  - Induction as induction on peano terms
    - other inter-derviations

- Inductively define #vertices, #edges
- Show #edges < #vertices
   - Convert to different styles of induction
