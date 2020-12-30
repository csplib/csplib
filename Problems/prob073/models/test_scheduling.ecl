%
% ECLiPSe sample code
% Author: Joachim Schimpf
% Licenced under CC-BY-4.0 : http://creativecommons.org/licenses/by/4.0/
%
% This is a simple model for the Test Scheduling Problem from the CP2015
% Modelling Competition, described at http://csplib.org/Problems/prob073
%
% Uses straightforward no-overlap constraints and a simple search heuristic.
%


% Data structure declarations
:- local struct(task(name,dur,machs,nmach,res,nres,start,mach)).
:- local struct(res(name,cap)).
:- local struct(mach(name,id)).

% Libraries used
:- lib(ic).
:- lib(ic_edge_finder).
:- lib(branch_and_bound).
:- lib(timeout).


main :-
%        Instance = "instances/t40m10r3-2.pl",        % opt=1725
        Instance = "instances/t50m10r3-9.pl",         % opt=7279
%        Instance = "instances/t100m50r10-11.pl",     % opt=4970
%        Instance = "instances/t500m50r5-5.pl",       % opt=33848
        get_data(Instance, Resources, Machines, Tasks),
        model(Resources, Machines, Tasks, MakeSpan),
        solve(Tasks, MakeSpan),
        writeln(makespan=MakeSpan).


model(Resources, Machines, Tasks, MakeSpan) :-

        % Initialize machine variables, define task ends and makespan
        (
            foreach(task{start:S,dur:D,machs:Ms,mach:M},Tasks),
            foreach(S,Starts), foreach(E,Ends), foreach(D,Durs)
        do
            M #:: Ms,           % machine variable
            E #= S+D            % end time
        ),
        MakeSpan #= max(Ends),
        LowerBound is integer(ceiling(sum(Durs)/length(Machines))),
        MakeSpan #>= LowerBound,

        % Initialize start times
        Starts #:: 0..sum(Durs)-min(Durs),

        % Non-overlap on global resources
        ( foreach(res{name:RN},Resources), param(Tasks) do
            (
                foreach(task{res:Rs,start:S,dur:D},Tasks),
                fromto(Starts,Starts1,Starts2,[]),
                fromto(Durs,Durs1,Durs2,[]),
                param(RN)
            do
                ( memberchk(RN,Rs) ->
                    Starts1 = [S|Starts2],
                    Durs1 = [D|Durs2]
                ;
                    Starts1 = Starts2,
                    Durs1 = Durs2
                )
            ),
            ( Starts==[] -> true ; disjunctive(Starts, Durs) )
        ),

        % Non-overlap on machines
        ( fromto(Tasks,[T1|Ts],Ts,[]) >> (foreach(T2,Ts),param(T1)) do
            task{start:S1,dur:D1,mach:M1} = T1,
            task{start:S2,dur:D2,mach:M2} = T2,
            M1#=M2 => (S1+D1#=<S2 or S2+D2#=<S1)
        ).


solve(Tasks, MakeSpan) :-

        % Order tasks with many resources/few machines/long duration first
        sort(dur of task, >=, Tasks, Tasks1),
        sort(nmach of task, =<, Tasks1, Tasks2),
        sort(nres of task, >=, Tasks2, OrderedTasks),

        % branch-and-bound, allowing 10s for each improvement
        bb_min(
            timeout(label(OrderedTasks), 10, (writeln(timeout),fail)),
            MakeSpan,
            bb_options{strategy:restart}        % faster
%            bb_options{strategy:dichotomic}    % better
        ).

    % Assign machines and earliest feasible start time
    label(Tasks) :-
        ( foreach(task{start:Start,mach:Machine},Tasks), count(_I,1,_) do
            indomain(Machine, random),
            once indomain(Start, min)       % incomplete labeling here!
        ).


% Collect instance data: Resources, Machines, Tasks
get_data(Instance, Resources, Machines, Tasks) :-
        compile(Instance),
        findall(res{name:N,cap:C}, resource(N,C), Resources),
        findall(mach{name:N}, embedded_board(N), Machines),
        ( foreach(mach{id:I},Machines), count(I,1,NMach) do true ),
        findall(task{name:N,dur:D,machs:Ms,nmach:NM,res:Rs,nres:NR}, (
                test(N,D,MNames,Rs,_,_),
                length(Rs, NR),
                ( MNames==[] ->
                    NM = NMach, Ms = 1..NMach
                ;
                    length(MNames, NM),
                    ( foreach(MName,MNames), foreach(M,Ms), param(Machines) do
                        memberchk(mach{name:MName,id:M}, Machines)
                    )
                )
            ), Tasks).

