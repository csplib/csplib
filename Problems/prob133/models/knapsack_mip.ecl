%
% ECLiPSe SAMPLE CODE
% Licenced under CC-BY-4.0 : http://creativecommons.org/licenses/by/4.0/
%
% Bounded-Knapsack problem from
% http://rosettacode.org/wiki/Knapsack_problem/
% Pack 0 or more items of each type, obeying the knapsack weight constraint.
% There is a limited supply of each item type.
%
% Using eplex as a mixed-integer solver.
%
% Sample run:
%
%     % eclipse -f knapsack_mip.ecl -e main
%     loading OSI clpcbc ... done
%     1 of map
%     1 of compass
%     1 of water
%     2 of glucose
%     3 of banana
%     1 of cheese
%     1 of suntan cream
%     1 of waterproof overclothes
%     1 of note-case
%     1 of sunglasses
%     1 of socks
%     weight = 396.0 ; value = 1010.0
%

:- lib(eplex).

model(Weights, Values, Bounds, Capacity, Amounts, Weight, Value) :-
	( foreach(Bound,Bounds), foreach(Amount,Amounts) do
	    Amount $:: 0..Bound
	),
	integers(Amounts),
	Amounts*Weights $= Weight,
	Weight $=< Capacity,
	Amounts*Values $= Value.

solve(Weights, Values, Bounds, Capacity, Amounts, Weight, Value) :-
	model(Weights, Values, Bounds, Capacity, Amounts, Weight, Value),
	optimize(max(Value), _Value).

main :-
    	data(Names, Weights, Values, Bounds, Capacity),
	solve(Weights, Values, Bounds, Capacity, Amounts, Weight, Value),
	( foreach(Amount,Amounts), foreach(Name,Names) do
	    ( Amount >0 -> printf("%d of %w%n", [Amount,Name]) ; true )
	),
	writeln(weight=Weight;value=Value).


% data(Names, Weights, Values, Bounds, Capacity)
data(
    [map,compass,water,sandwich,glucose,tin,banana,apple,cheese,beer,
       'suntan cream',camera,'t-shirt',trousers,umbrella,'waterproof trousers',
       'waterproof overclothes','note-case',sunglasses,towel,socks,book],
    [  9,13,153, 50,15,68,27,39,23,52,11,32,24,48,73,42,43,22, 7,18, 4,30],
    [150,35,200, 60,60,45,60,40,30,10,70,30,15,10,40,70,75,80,20,12,50,10],
    [  1, 1,  2,  2, 2, 3, 3, 3, 1, 3, 1, 1, 2, 2, 1, 1, 1, 1, 1, 2, 1, 2], 
    400
).

