% bibd.mzn
% vim: ft=zinc ts=4 sw=4 et tw=0
% Ralph Becket <rafe@csse.unimelb.edu.au>
% Tue Oct 23 11:28:06 EST 2007
%
% Balanced incomplete block designs.  See the following:
% http://mathworld.wolfram.com/BlockDesign.html
% http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob028/spec.html
%
%
%
% A BIBD (v, b, r, k, lambda) problem is to find a binary matrix of v rows
% of b columns such that each row sums to r, each column sums to k, and
% the dot product beween any pair of distinct rows is lambda.

include "lex_lesseq.mzn";

int: v;
int: k;
int: lambda;

int: b = (lambda * v * (v - 1)) div (k * (k - 1));
int: r = (lambda * (v - 1)) div (k - 1);

set of int: rows = 1..v;
set of int: cols = 1..b;

array [rows, cols] of var bool: m;

    % Every row must sum to r.
    %
constraint forall (i in rows) (sum (j in cols) (bool2int(m[i, j])) = r);

    % Every column must sum to k.
    %
constraint forall (j in cols) (sum (i in rows) (bool2int(m[i, j])) = k);

    % The dot product of every pair of distinct rows must be lambda.
    %
constraint
    forall (i_a, i_b in rows where i_a < i_b) (
        sum (j in cols) (bool2int(m[i_a, j] /\ m[i_b, j])) = lambda
    );

    % Break row symmetry in the incidence matrix.
    %
constraint forall(i in rows diff {max(rows)})(
        lex_lesseq([m[i, j] | j in cols], [m[i+1, j] | j in cols])
    );

    % Break column symmetry in the incidence matrix.
    %
constraint forall(j in cols diff {max(cols)})(
        lex_lesseq([m[i, j] | i in rows], [m[i, j+1] | i in rows])
    );

solve :: bool_search([m[i, j] | i in rows, j in cols],
    input_order, indomain_min, complete)
    satisfy;

output  ["bibd: (v = ", show(v), ", b = ", show(b), ", r = ", show(r),
        ", k = ", show(k), ", lambda = ", show(lambda), ")\n\n"] ++
        [ ( if j > b then "\n" else show(bool2int(m[i, j])) endif )
        | i in rows, j in 1..(b + 1)
        ];

%----------------------------------------------------------------------------%
%----------------------------------------------------------------------------%
