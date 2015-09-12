/**************************
Posting in comp.constraints and sci.op-research
describing this problem

warwick@cs.mu.oz.au (Warwick HARVEY) wrote:
>In sci.op-research, bigwind777@aol.com (Bigwind777) writes:
>
>>Please help with this problem.
>
>>I have 32 golfers, individual play.
>
>>We will golf for 16 weeks.
>
>>I want to set up the foursomes so each person only golfs
>>with the same person once.
>
>>How many weeks can we do this before it starts to duplicate ?
>.......
>It seems to be a generalisation of the problem of constructing a
>round-robin tournament schedule, where the number players in a "game" is
>more than two.
>
>Has anybody had any experience with this kind of problem?  Any ideas on
>good ways to model it?

*************************/

:- lib(conjunto).

% For a 9-week solution to the above problem, call golf(9, 8, X).

golf(RoundNum,RoundSize,Rounds) :-

	% MODEL PART

	( for(I,1,4*RoundSize), foreach(I,PlayerList) do true ),
	list2set(PlayerList,SetUB),

	( count(_,1,RoundNum),
	  foreach(GroupsInRound,Rounds),
	  param(SetUB),
	  param(RoundSize)
	do
	    ( foreach(S,GroupsInRound),
	      count(_,1,RoundSize),
	      param(SetUB)
	    do
		    S `:: {} .. SetUB,
		    #(S,4)
	    ),
	    % all_union(GroupsInRound,SetUB),
	    all_disjoint(GroupsInRound)
	),
	( fromto(Rounds,[R|Rest0],Rest0,[])
	do
		flatten(Rest0,Rest),
		( foreach(Group,R),
		  param(Rest)
		do
			( param(Group),
			  foreach(Group1,Rest)
			do
				ISize :: 0..1,
				#(Group /\ Group1,ISize)
			)
		)
	),

	% SEARCH PART

	( for(Player,1,4*RoundSize),
	  param(Rounds)
	do
	    writeln(player = (Player)),
	    ( foreach(R,Rounds),
	      count(Round,1,_),
	      param(Player)
	    do
		writeln(Round),
	    	member(Group,R),
		Player in Group
	    )
	),

	% PRINT OUT SOLUTION

	( foreach(R,Rounds)
	do
		writeln(R)
	).
