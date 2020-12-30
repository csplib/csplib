%-----------------------------------------------------------------------------%
% Copyright (C) 2013 National ICT Australia and Monahsh University 2017
% Licenced under CC-BY-4.0 : http://creativecommons.org/licenses/by/4.0/
%-----------------------------------------------------------------------------%
%
% Author(s): 
%   Original model, Flexible Job Shop Scheduling: 
%   Andreas Schutt <andreas.schutt@nicta.com.au>
%   Changes, Stochastic Assignment and Scheduling Problem: 
%   David Hemmi <david.hemmi@monash.edu>
%
%-----------------------------------------------------------------------------%
% Stochastic General Assignment Problem  
% First stage: 
%    assign task to machines  
% Second stage: 
%    based on observed processign times, schedule taks on respective machines 
% Objective: 
%    minimise expected makespan 
%-----------------------------------------------------------------------------%
% Including files

include "globals.mzn";

%-----------------------------------------------------------------------------%
% Parameters

int: no_mach;   % Number of machines
int: no_jobs;   % Number of jobs
int: no_task;   % Number of total tasks
int: no_optt;   % Number of total optional tasks

set of int: Mach  = 1..no_mach;
set of int: Jobs  = 1..no_jobs;
set of int: Tasks = 1..no_task;
set of int: OptTs = 1..no_optt;

array [Jobs] of set of int: tasks;
array [Tasks] of set of int: optts;

array [OptTs] of int: optt_mach;
array [SCENARIOS1,OptTs] of int: optt_dur;


array [Jobs] of int: last_task = [ max(tasks[j]) | j in Jobs ];
%---------implications for multi scenarion solving ---------------
int: nbScenarios;
set of int: SCENARIOS1 = 1..nbScenarios;
int: first_scen;
int: last_scen;
set of int: SCENARIOS = first_scen..last_scen;
array[SCENARIOS1] of int: weights;

%-------end of multi scenario addons ----------------
array [Tasks] of int: task_job = 
    [ min(j in Jobs where t in tasks[j])(j) | t in Tasks ];
array [SCENARIOS,Tasks] of int: task_mins =
    array2d(SCENARIOS,Tasks,[ sum(k in tasks[task_job[t]])(if k < t then task_mind[s,k] else 0 endif)
    |   s in SCENARIOS, t in Tasks ]);
array [SCENARIOS,Tasks] of int: task_maxs =
    array2d(SCENARIOS,Tasks,[ t_max[s] -
        sum(k in tasks[task_job[t]])(if k < t then 0 else task_mind[s,k] endif)
    |   s in SCENARIOS,  t in Tasks ]);

array [SCENARIOS,Tasks] of int: task_mind =
    array2d(SCENARIOS,Tasks,[ min(o in optts[t])(optt_dur[s,o]) | s in SCENARIOS,t in Tasks ]);

array [SCENARIOS,Tasks] of int: task_maxd = 
    array2d(SCENARIOS,Tasks,[ max(o in optts[t])(optt_dur[s,o]) | s in SCENARIOS, t in Tasks ]);

    % Additional deirved parameters for optional tasks
    %
array [OptTs] of int: optt_task =
    [ min(t in Tasks where o in optts[t])(t) | o in OptTs ];

array[SCENARIOS1] of int: min_dur = [ min([optt_dur[s,t] | t in OptTs]) | s in SCENARIOS1];
array[SCENARIOS1] of int: max_dur = [ max([optt_dur[s,t] | t in OptTs]) | s in SCENARIOS1];
set of int: Durs = min(min_dur)..max(max_dur);

    % Parameters related to the planning horizon
    %
array[SCENARIOS1] of int: t_max = [sum(t in Tasks)(max(o in optts[t])(optt_dur[s,o])) | s in SCENARIOS1];

set of int: Times = 0..max(t_max);

%-----------------------------------------------------------------------------%
% Variables

    % Start time variables for tasks
    %
array [SCENARIOS,Tasks] of var Times: start =
    array2d(SCENARIOS,Tasks,[ let { var task_mins[s,t]..task_maxs[s,t]: k } in k | s in SCENARIOS, t in Tasks ]);

    % Duration variables for tasks
    %
array [SCENARIOS,Tasks] of var Durs: dur =
    array2d(SCENARIOS,Tasks,[ if task_mind[s,t] = task_maxd[s,t] then task_mind[s,t] else
        let { var task_mind[s,t]..task_maxd[s,t]: d } in d endif
    |   s in SCENARIOS,t in Tasks ]);

    % Variables whether an optional task is executed
    %
array [OptTs] of var bool: b;

array[SCENARIOS] of var Times: de_objective;

set of int: StochTimes = 0..sum(t_max);
var StochTimes: objective;
%-----------------------------------------------------------------------------%
% Constraints

    % Precedence relations
    %
constraint
    forall(s in SCENARIOS)(
        forall(j in Jobs, i in tasks[j] where i < last_task[j])(
            start[s,i] + dur[s,i] <= start[s,i + 1]
        )    
    );

    % Duration constraints
    %
constraint
    forall(o in OptTs,s in SCENARIOS)(
        let { int: t = optt_task[o] } in (
            if card(optts[t]) = 1 then
                b[o] = true
            else
                b[o] -> dur[s,t] = optt_dur[s,o]
            endif
        )
    );

    % Optional tasks' constraints
    %
constraint
    forall(t in Tasks where card(optts[t]) > 1)(
        ( sum(o in optts[t])(bool2int(b[o])) <= 1     )
    /\  ( exists(o in optts[t])(b[o])                 )
    );

constraint
    forall(t in Tasks where card(optts[t]) = 2)(
        let {
            int: o1 = min(optts[t]),
            int: o2 = max(optts[t])
        } in ( b[o1] <-> not(b[o2]) )
    );

    % Resource constraints
    %
constraint
    forall(m in Mach,s in SCENARIOS)(
        let {
            set of int: MTasks = { o | o in OptTs where optt_mach[o] = m }
        } in (
            cumulative(
                [ start[s,optt_task[o]] | o in MTasks ],
                [ optt_dur[s,o]         | o in MTasks ],
                [ bool2int(b[o])      | o in MTasks ],
                1
            )
        )
    );

% Objective constraint
constraint
    forall(s in SCENARIOS)(
        forall(j in Jobs)(start[s,last_task[j]] + dur[s,last_task[j]] <= de_objective[s])
    );
constraint 
        objective = sum(s in SCENARIOS)(weights[s]*de_objective[s]);
%-----------------------------------------------------------------------------%
% Solve item

solve 
    :: search
    minimize objective;

%------------------------------------------------------------------------------%
% Searches

ann: s_mindur   = int_search([dur[s,t] |s in SCENARIOS, t in Tasks], smallest, indomain_min, complete);
ann: s_minstart = int_search([start[s,t] |s in SCENARIOS, t in Tasks], smallest, indomain_min, complete);
ann: s_bool     = bool_search(b, input_order, indomain_max, complete);
ann: s_obj      = int_search(de_objective, input_order, indomain_min, complete);

ann: search = seq_search([s_mindur, s_bool, s_minstart, s_obj]);

%-----------------------------------------------------------------------------%
% Output

output 
[   "objective = ", show(de_objective), ";\n",
    "stoch obj = ", show(objective), ";\n",
    "start = ", show(start), ";\n",
    "dur = ", show(dur), ";\n",
    "b = ", show(b), ";\n",
];
