% The balanced academic curriculum problem: see
% http://www.dcs.st-and.ac.uk/~ianm/CSPLib/prob/prob030/spec.html
%
% A curriculum is a set of courses with prerequisites.
%
% Each course must be assigned within a set number of periods.
%
% A course cannot be scheduled before its prerequisites.
%
% Each course confers a number of academic credits (it's "load").
%
% Students have lower and upper bounds on the number of credits
% they can study for in a given period.
%
% Students have lower and upper bounds on the number of courses
% they can study for in a given period.
%
% The goal is to assign a period to every course satisfying these
% criteria, minimising the load for all periods.

include "globals.mzn";

int: n_courses;
int: n_periods;
int: load_per_period_lb;
int: load_per_period_ub;
int: courses_per_period_lb;
int: courses_per_period_ub;
array [1..n_courses] of int: course_load;
int: max_course_load = sum(c in courses)(course_load[c]);

set of int: courses = 1..n_courses;
set of int: periods = 1..n_periods;

% period course is assigned to
array [courses] of var periods: course_period;
% whether period i has course j assigned
array [periods, courses] of var 0..1: x;
% total load for each period
array [periods] of var load_per_period_lb..load_per_period_ub: load;
% optimisation target
var load_per_period_lb..load_per_period_ub: objective;

constraint forall(p in periods) (
    forall(c in courses) (x[p,c] = bool2int(course_period[c] = p)) /\
    sum(i in courses) (x[p,i]) >= courses_per_period_lb /\
    sum(i in courses) (x[p,i]) <= courses_per_period_ub /\
    load[p] = sum(c in courses) (x[p,c] * course_load[c]) /\
    load[p] >= load_per_period_lb /\
    load[p] <= objective
);

% prerequisite(a, b) means "course a has prerequisite course b".

predicate prerequisite(courses: a, courses: b) =
    course_period[b] < course_period[a];

% add some redundant linear constraints

constraint forall(p in 0..n_periods-1) (
    let {
		var 0..max_course_load: l = sum(c in courses) (bool2int(course_period[c] > p) * course_load[c])
	} in 
        l >= (n_periods-p) * load_per_period_lb /\
        l <= (n_periods-p) * objective
    );

solve :: seq_search([
      int_search([x[i,j] | i in periods, j in courses], input_order, indomain_max, complete),
      int_search([objective], input_order, indomain_min, complete)
    ]) minimize objective;

output 
    [show(c) ++ "-" ++ show(course_period[c]) ++ "\t" | c in courses ] ++ ["\n"] ++
    ["objective = ", show(objective)];
