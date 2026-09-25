% 1 for filled blocks, 0 for gaps
% CLP(FD) nonogram solver using a regular-language (automaton) constraint.
% Each row and column clue is compiled into a small DFA whose accepted words
% are exactly the 0/1 sequences whose runs of 1s match the clue. The automaton
% constraints propagate during search, so labeling rarely explores dead grids.
:- use_module(library(clpfd)).

% Given constraints: for each row and column, a list of block lengths is stored.
% This is just the example instance (a 5x5 plus sign); the solver takes the
% clues as arguments, so the same relation solves any instance.
row_constraints([[1], [3], [5], [3], [1]]).
col_constraints([[1], [3], [5], [3], [1]]).

% nonogram(+RowCounts, +ColCounts, -Solution): Solution is a grid of 0/1 rows
% (one row per row clue) whose rows and columns satisfy all block clues.
nonogram(RowCounts, ColCounts, Solution) :-
    length(RowCounts, NumRows),
    length(ColCounts, NumCols),

    % Build the NumRows x NumCols matrix of fresh 0/1 cell variables.
    length(Solution, NumRows),
    maplist(flip_length(NumCols), Solution),
    append(Solution, Vars),
    Vars ins 0..1,

    % Post a regular constraint per row, then per column (via the transpose).
    % Posting before labeling is what makes this propagate instead of test.
    maplist(line_automaton, RowCounts, Solution),
    transpose(Solution, Columns),
    maplist(line_automaton, ColCounts, Columns),

    % Search what little remains after propagation.
    label(Vars).

% flip_length(+Length, ?List): List has the given Length
flip_length(Length, List) :- length(List, Length).

% line_automaton(+Clue, +Cells): constrain the cell list Cells so its runs of
% 1s match Clue, by compiling Clue into a DFA and posting automaton/3.
line_automaton(Clue, Cells) :-
    line_dfa(Clue, Source, Sink, Arcs),
    automaton(Cells, [source(Source), sink(Sink)], Arcs).

% line_dfa(+Clue, -Source, -Sink, -Arcs): build a DFA from the block clue.
% States are named by meaning: g(I) is the gap state after block I (g(0) is the
% leading gap and the source), f(I,J) is the state after reading J cells of
% block I. Gap states loop on 0 to absorb extra zeros.
% Empty clue: one state that accepts only zeros.
line_dfa([], g(0), g(0), [arc(g(0),0,g(0))]).
% Non-empty clue: the leading gap g(0) loops on 0, then the blocks add arcs.
line_dfa([B|Bs], g(0), Sink, [arc(g(0),0,g(0))|Arcs]) :-
    clue_arcs([B|Bs], 1, g(0), Sink, Arcs).

% clue_arcs(+Blocks, +I, +FromGap, -Sink, -Arcs): arcs for block I (entered from
% gap state FromGap) and everything after it. I numbers the blocks so the state
% names stay unique.
% Last block: read its ones, then the completion state loops on 0 (it is the sink).
clue_arcs([B], I, FromGap, f(I,B), [arc(f(I,B),0,f(I,B))|BlockArcs]) :-
    block_arcs(B, I, FromGap, BlockArcs).
% More blocks follow: read this block, take a mandatory 0 to the next gap g(I)
% (which loops on 0), then build the rest of the clue from there.
clue_arcs([B|Bs], I, FromGap, Sink, Arcs) :-
    Bs = [_|_],
    block_arcs(B, I, FromGap, BlockArcs),
    I1 is I + 1,
    clue_arcs(Bs, I1, g(I), Sink, RestArcs),
    append(BlockArcs, RestArcs, Joined),
    Arcs = [arc(f(I,B),0,g(I)), arc(g(I),0,g(I)) | Joined].

% block_arcs(+B, +I, +FromGap, -Arcs): arcs that read exactly B consecutive 1s
% as block I. The first 1 leaves FromGap; the last leads to f(I,B).
block_arcs(B, I, FromGap, [arc(FromGap,1,f(I,1))|Arcs]) :-
    ones_arcs(I, 1, B, Arcs).

% ones_arcs(+I, +J, +B, -Arcs): a 1-arc f(I,J) -> f(I,J+1) for each J up to B.
ones_arcs(_, B, B, []).
ones_arcs(I, J, B, [arc(f(I,J),1,f(I,J1))|Arcs]) :-
    J < B,
    J1 is J + 1,
    ones_arcs(I, J1, B, Arcs).
