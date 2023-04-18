%% == EXAMPLE  ==
%% Let alpha = (a -> b) | (b -> a)
%% We have
%% Theta = ClauIPL(alpha) = {  (a->b)->g ,  (b->a)->g }
%% Delta = Psi(Theta) = { g | a-/->b , g | b-/->a }
%% We encode the sequent Delta => g

%% index of atoms
indexOfAtom(a,0).
indexOfAtom(b,1).
indexOfAtom(g,2).

%% non-classical clauses (2)
% gc1 = g | a-/->b
genClause(gc1).
litOf(g, gc1).
litOf(negImp(a, b), gc1).

% gc2 = g | b-/->a
genClause(gc2).
litOf(g, gc2).
litOf(negImp(b, a), gc2).

%% right atom
rightAtom(g).

%% atomSet's representation
atomSet( w(0..1, 0..1, 0..1) ).

% atomSetComponent(K,W,B)  
% the component K of atomSet W is B

atomSetComponent( 0, w(B0, B1, B2), B0) :- 
   atomSet( w(B0, B1, B2) ).
atomSetComponent( 1, w(B0, B1, B2), B1) :- 
   atomSet( w(B0, B1, B2) ).
atomSetComponent( 2, w(B0, B1, B2), B2) :- 
   atomSet( w(B0, B1, B2) ).
