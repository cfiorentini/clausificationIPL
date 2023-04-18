%%%%%%%%%%%%%%%%%%%
% clingo -n 0  inputFile.lp 


%%  Literals
%%  --------
%%
%% A *literal* has the form
%%
%%      Atom |  neg(Atom)  | negImp(Atom1,Atom2)  | negImp(Atom1,false)
%%
%%  where
%%  -  Atom, Atom1, Atom2  denote propositional variables
%%  -  'false' denotes the constant false
%%  -  neg is the classical negation
%%  -  negImp is the negated implications
%%  
%%   Literals of the kind 'Atom' and 'neg(Atom)' are called *classical literals*.
%%
%%  General clauses
%%  ---------------
%% 
%%  A *classical clause (clClause)* is a set of classical literals.
%%  A *general clause (genClause)*  is a set of literals, containing at least a negImp-literal.
%%
%%  To encode general clauses, we introduce the following predicates clClause/1, genClause/1 and litOf/2:
%% 
%%    clClause(C):      C is the name of a  classical clause
%%    genClause(C):     C is the name of a  general   clause
%%    litOf(Lit,C):     Lit is a literal of the clause C 
%%
%%  The atoms occurring in the clauses must be indexed  by natural numbers, starting from 0.
%%  To set the index of an atom, use the predicate  indexOfAtom/2:
%%
%%    indexOfAtom(Atom,I):   the index of atom Atom is I (I >= 0)
%%
%%  General sequents
%%  ----------------
%%
%%  A *general sequent* is a sequent of the kind
%%
%% 
%%     GenClauses ==> rigthAtom
%%
%%  where 
%%  - GenClauses is a set of general clauses
%%  - rightAtom is an atom or the constant 'false'
%%
%% The general clauses are defined by the predicates  clClause/1, genClause/1 described above.
%% The right atom is specified by the predicate rightAtom/!: 
%%
%%  rightAtom(Atom):  Atom is the right atom
%%
%%  Atom sets
%%  --------- 
%%
%%  The worlds of a countermodel are represented by  sets of atoms. 
%%  An atom set W represent the classical interpratation I such that:
%%
%%   I |= a   IFF  a \in W   for every atom a 
%%
%%  Let N be the number of atoms occurring in the general sequent, excluding the constant 'false'.
%%
%%  An *atom set (atomSet)* W is represented by a term of thr form
%%
%%       w(b0, b1, ... , b(N-1) )
%%
%%   Each component  bk (where 0 <= k < N-1) is a booelan value (0 or 1).
%%   Let k \in {0 .. (N-1)} and let Atom be the atom of index k (as specified by the predicate indexOfAtom/2). .
%%   -  if bk = 1, then Atom \in W
%%   -  if bk = 0, then Atom \not\in W
%%
%%  Note that the arity of the term w depends on the number of atoms.
%%
%%  To specify the atomSet's, use the  predicates atomSet/1 and atomSetComponent/3:
%%
%%                atomSet(W):    W is an atomSet  
%%   atomSetComponent(K,W,B):   the component K of atomSet W is B (B = 0 or B = 1).




%% predicates used  to encode a  problem
#defined litOf/2.  
#defined clClause/1.
#defined genClause/1.
#defined atomSet/1.
#defined  atomSetComponent/3.

% atom(Atom)
% Atom is an atom

atom(Atom) :- indexOfAtom(Atom,_).

% numberOfAtoms(N).
% N is the number of  atoms
numberOfAtoms(N) :- #count{ Atom : atom(Atom) } = N. 


% member(Atom,W)
% The atom Atom belongs to the atomSet W

member(Atom, W ) :-
					 indexOfAtom(Atom,K), atomSetComponent(K,W,1).




%% == CLASSICAL VALIDITY ==



% satisfies_lit(W,Lit)
% the atomSet W satisfies the classical literal Lit (classical validity)

satisfies_lit(W,Atom) :-
							 atomSet(W), atom(Atom), member(Atom,W).


satisfies_lit(W,neg(Atom)) :-
							 atomSet(W), atom(Atom), not member(Atom,W).


% satisfies_clClause(W,Lit)
% The atomSet W satisfies the classical clause C (classical validity)
% -- The number of literals Lit such that Lit is a literal of C and W satisfies Lit is > 0

satisfies_clClause(W,C) :-
	atomSet(W),clClause(C),
	#count{ Lit : litOf(Lit,C) , satisfies_lit(W,Lit) } > 0.


% satisfies_all_clClauses(W)
% The atomSet W satisfies all the  classical clauses.
% --  The number of classical clauses C such that W does not satisfies C is 0

satisfies_all_clClauses(W) :-
    atomSet(W), #count{ C : clClause(C), not satisfies_clClause(W,C) } = 0.
											


%%% == WORLD GENERATION ==
% A world is an atomSet satisfying all the classical clauses


%{ world(W) : satisfies_all_clClauses(W) } :- atomSet(W).

{ world(W) : atomSet(W), satisfies_all_clClauses(W) }.

%% Note that  every world W satisfy all the classical clauses.
%% We have to check that all the general clauses are satisfied (see the constraints)


% numberOfWorlds(N)
% N is the number of generared worlds
numberOfWorlds(N) :-  #count{ W : world(W) } = N.


% leq(W1,W2)  -- less or equal
% The world W1 is less or equal to the world  W2 (W1 is a subset of W2).

leq(W,W) :- world(W).

% W1 is a subset of W2
% -- The set of K such that the component of index K of W1 is 1 and the component of index K of W2 is 0 is empty
leq(W1,W2) :-
					 world(W1),world(W2),
					 #count{ K : atomSetComponent(K,W1,1), atomSetComponent(K,W2,0) } = 0.


% le(W1,w2)  -- less 
%  W1 != W2 and leq(W1,W2) 

le(W1, W2) :-  W1 != W2, leq(W1, W2).



% realizes_genLit(W,Lit)
% The world W realizes the literal Lit

realizes_genLit(W,Atom) :- 
							 world(W),atom(Atom), member(Atom,W).



realizes_genLit(W,neg(Atom)) :-
							 world(W), atom(Atom), not member(Atom,W).

% W realises  negImp(Atom1,Atom2) 
% --  The number of worlds W1 such that W <= W1 and Atom1 \in W1 and  Atom2 \not\in W1  is > 0

% Atom2 = false
realizes_genLit( W, negImp(Atom1,false) ) :-
	 world(W), atom(Atom1), atom(Atom2),
   #count{ W1 : world(W1), leq(W,W1), member(Atom1,W1)  } > 0.

% Atom2 != false
realizes_genLit( W, negImp(Atom1,Atom2) ) :-
	 world(W), atom(Atom1), atom(Atom2),
   #count{ W1 : world(W1), leq(W,W1), member(Atom1,W1) , not member(Atom2, W1) } > 0.


% realizes_genClause(W,C) :
% The world W realizes the general clause C.
%  -- The number of literals Lit such that Lit is a literal of C and W realizes Lit is > 0

realizes_genClause(W,C) :-
	world(W), genClause(C),
	#count{ Lit : litOf(Lit,C) , realizes_genLit(W,Lit) } > 0.




%% == CONSTRAINTS ==

% Exactly one world falsifies  rightAtom (right atom).
% -- It is false that G is the rightAtom and the number of worlds W such that  G \not\in W  is different from 1.
:-   rightAtom(G) , #count{ W : world(W), not member(G,W) } != 1.  



% Every world realizes every general clause.
% -- It is false that W is a world and  the number of general  clauses C such that W does not realize C is > 0
:- world(W), #count{ C : genClause(C), not realizes_genClause(W,C) } > 0.


% We call root the only world not satisfying rightAtom

% root(W)
% W is the root
root(W) :- world(W),  rightAtom(G), not member(G,W).


% The root is the minimum element
% -- It is false that W0 is the root, W1 is a world and leq(W0,W1) does not hold
:- root(W0),  world(W1),  not leq(W0,W1).








%% === PRETTY PRINT ===



%% succ(W1,W2)
%% world W2 is an immediate successor of world W1.
%% -- W1 < W2  and the number of worlds W such that W1 <= W <= W2 is 2. 

succ(W1,W2) :- world(W1), world(W2), le(W1,W2) ,
						#count{ W : world(W), leq(W1,W), leq(W,W2)  } = 2.

%% memberWorld(At,W)
%% atom At belomgs to world W
memberOfWorld(At,W) :- atom(At), world(W),  member(At,W).




%%% SHOW RESULT
%#show member/2.
#show memberOfWorld/2.
%#show leq/2.
%#show le/2.
#show world/1.
%#show satisfies_all_clClauses/1.
%#show realizes_genClause/2.
#show succ/2.
%#show numberOfWorlds/1.
%#show numberOfAtoms/1.
%#show root/1.
%#show atomSet/1.
