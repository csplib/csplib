
plan(Start,End,Plan):-
  generate_plan(Start,End,Plan),
  df_plan(Plan).

generate_plan(Start,End,[Start,End]).
generate_plan(Start,End,[Start|Plan]):-
  generate_plan(_,End,Plan).

df_plan([_]).
df_plan([X,Y|Plan]):-
  pour(X,Y),
  df_plan([Y|Plan]).

pour(A-B-C,A1-B1-C):-
  A>0,B<5,
  B1 is min(5,B+A),
  A1 is A-(B1-B).

pour(A-B-C,A1-B-C1):-
  A>0,C<3,
  C1 is min(3,C+A),
  A1 is A-(C1-C).

pour(A-B-C,A1-0-C):-
  B>0,
  A1 is A+B.

pour(A-B-C,A-B1-C1):-
  B>0,C<3,
  C1 is min(3,C+B),
  B1 is B-(C1-C).

pour(A-B-C,A1-B-0):-
  C>0,
  A1 is A+C.

pour(A-B-C,A-B1-C1):-
  C>0,
  B1 is min(5,B+C),
  C1 is C-(B1-B).




