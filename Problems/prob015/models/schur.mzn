%% schurs numbers
%%
%% determine if n balls labelled 1..n 
%% can be placed in c boxes with no box containing a triple {x,y,z} where x+y=z

int: n; %% number of balls
int: c; %% number of boxes

array[1..n] of var 1..c: box;

constraint forall(i in 1..n-1, j in i+1 .. n - i)(
	   	    box[i] != box[j] \/ 
		    box[i] != box[i+j] \/ 
		    box[j] != box[i+j]);

solve satisfy;

output ["n = ", show(n), ";\nc = ", show(c), ";\nbox = ",show(box),";\n"];
