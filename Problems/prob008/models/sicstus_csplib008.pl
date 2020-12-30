/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Vessel Loading
 * Author    : Mats Carlsson
 *
 * Supply vessels transport containers from site to site. The deck area
 * is rectangular. Containers are cuboid, and are laid out in a single
 * layer. All containers are positioned parallel to the sides of the
 * deck. The contents of the containers determine their class. Certain
 * classes of containers are constrained to be separated by minimum
 * distances either along the deck or across the deck.
 *
 * Example problem:
 * 
 * The deck measures 16 units by 16 units. There are 10 containers, in three classes:
 * 
 * A          B         C
 * 6x8        4x4       4x8
 * 4x6        4x4       4x8
 * 4x4        4x6       4x6
 *                      4x6
 * 
 * Containers of classes B and C are constrained to be at least 4 units apart
 * in at least one of the north-south or east-west directions.
 * 
 * | ?- vessels(Origins, Shapes).
 */

:- module(vessels, [vessels/2]).
:- use_module(library(lists)).
:- use_module(library(clpfd)).

vessels(Origins, Shapes) :-
	length(Origins, 10),
	(   foreach([X,Y],Origins),
	    foreach(object(I,S,[X,Y]), Objects),
	    foreach(S,Sids),
	    foreach(Rank-[S,X,Y],KL1),
	    foreach(_-Triple,KL2),
	    foreach(Triple,Triples),
	    for(I,1,10)
	do  X in {0,2,4,6,8,10,12},
	    Y in {0,2,4,6,8,10,12},
	    findall(S, shape(I, sbox(S,_,_)), Ss),
	    findall(R, (shape(I, sbox(_,_,[W,H])), R is -W*H), [Rank|_]),
	    list_to_fdset(Ss, Set),
	    S in_set Set	    
	),
	findall(Sbox, shape(_,Sbox), Sboxes1),
	sort(Sboxes1, Sboxes2),
	rules(Rules, []),
	geost(Objects, Sboxes2,
	      [lex([4,5]),lex([7,8]),lex([9,10]),
	       bounding_box([0,0],[16,16]),
	       polymorphism(true),
	       dynamic_programming(true),
	       cumulative(true)],
	      Rules),
	keysort(KL1, KL2),
	append(Triples, Vars),
	labeling([bisect], Vars),
	(   foreach(S1,Sids),
	    foreach(Sh,Shapes),
	    param(Sboxes2)
	do  memberchk(sbox(S1,_,Sh), Sboxes2)
	).

rules -->
	[(xorigin(O1,S1) ---> O1^x(1)+S1^t(1)),
	 (xend(O1,S1) ---> O1^x(1)+S1^t(1)+S1^l(1)),
         (yorigin(O1,S1) ---> O1^x(2)+S1^t(2)),
	 (yend(O1,S1) ---> O1^x(2)+S1^t(2)+S1^l(2)),
         (apart(O1,S1,O2,S2) --->
	     (xend(O1,S1) + 4 #=< xorigin(O2,S2) #\/
	      xend(O2,S2) + 4 #=< xorigin(O1,S1) #\/
	      yend(O1,S1) + 4 #=< yorigin(O2,S2) #\/
	      yend(O2,S2) + 4 #=< yorigin(O1,S1))),
	forall(O1,objects([4,5,6]),
	       forall(S1,sboxes([O1^sid]),
		      forall(O2,objects([7,8,9,10]),
			     forall(S2,sboxes([O2^sid]), 
				    apart(O1,S1,O2,S2)))))
	].
	

shape( 1, sbox(6,[0,0],[6,8])).
shape( 1, sbox(7,[0,0],[8,6])).
shape( 2, sbox(2,[0,0],[4,6])).
shape( 2, sbox(3,[0,0],[6,4])).
shape( 3, sbox(1,[0,0],[4,4])).
shape( 4, sbox(1,[0,0],[4,4])).
shape( 5, sbox(1,[0,0],[4,4])).
shape( 6, sbox(2,[0,0],[4,6])).
shape( 6, sbox(3,[0,0],[6,4])).
shape( 7, sbox(4,[0,0],[4,8])).
shape( 7, sbox(5,[0,0],[8,4])).
shape( 8, sbox(4,[0,0],[4,8])).
shape( 8, sbox(5,[0,0],[8,4])).
shape( 9, sbox(2,[0,0],[4,6])).
shape( 9, sbox(3,[0,0],[6,4])).
shape(10, sbox(2,[0,0],[4,6])).
shape(10, sbox(3,[0,0],[6,4])).
