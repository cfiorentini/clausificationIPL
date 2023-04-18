%% === EXAMPLE 2	===
%%  Encoding of the sequent Delta => g where
%%    Delta =  a | ~c ,  b | ~c , d | g , g | a-/->b , g | b-/->a , ~d | c-/->false

%% Set an index for every atom occurring in a clause
indexOfAtom(a,0).
indexOfAtom(b,1).
indexOfAtom(c,2).
indexOfAtom(d,3).
indexOfAtom(g,4).


% c1 = a | ~c
clClause(c1).
litOf(a, c1).
litOf(neg(c), c1).

% c2 = b | ~c
clClause(c2).
litOf(b, c2).
litOf(neg(c), c2).


% c3 = d | g
clClause(c3).
litOf(d, c3).
litOf(g, c3).


% gc1 = g | a-/->b
genClause(gc1).
litOf(g, gc1).
litOf(negImp(a, b), gc1).

% gc2 = g | b-/->a
genClause(gc2).
litOf(g, gc2).
litOf(negImp(b, a), gc2).

% gc3 = ~d | c-/->false
genClause(gc3).
litOf(neg(d), gc3).
litOf(negImp(c, false), gc3).

%% Set the right atom
rightAtom(g).

%% atomSet's representation
atomSet( w(0..1, 0..1, 0..1, 0..1, 0..1) ).

% atomSetComponent(K,W,B)  
% the component K of atomSet W is B

atomSetComponent( 0, w(B0, B1, B2, B3, B4), B0) :-  atomSet( w(B0, B1, B2, B3, B4) ).
atomSetComponent( 1, w(B0, B1, B2, B3, B4), B1) :-  atomSet( w(B0, B1, B2, B3, B4) ).
atomSetComponent( 2, w(B0, B1, B2, B3, B4), B2) :-  atomSet( w(B0, B1, B2, B3, B4) ).
atomSetComponent( 3, w(B0, B1, B2, B3, B4), B3) :-  atomSet( w(B0, B1, B2, B3, B4) ).
atomSetComponent( 4, w(B0, B1, B2, B3, B4), B4) :-  atomSet( w(B0, B1, B2, B3, B4) ).
