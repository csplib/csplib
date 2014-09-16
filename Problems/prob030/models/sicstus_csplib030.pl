/*
 * SICSTUS CLPFD DEMONSTRATION PROGRAM
 * Purpose   : Balanced Academic Curriculum Problem
 * Author    : Mats Carlsson
 *
 * A curriculum is a set of courses with prerequisites.
 *
 * Each course must be assigned within a set number of periods.
 *
 * A course cannot be scheduled before its prerequisites.
 *
 * Each course confers a number of academic credits (its "load").
 *
 * Students have lower and upper bounds on the number of credits
 * they can study for in a given period.
 *
 * Students have lower and upper bounds on the number of courses
 * they can study for in a given period.
 *
 * The goal is to assign a period to every course satisfying these
 * criteria, minimising the load for all periods.
 *
 * | ?- bacp(12).
 */

:- module(bacp, [bacp/1]).

:- use_module(library(clpfd)).

bacp(ID) :-
	bacp(ID, CPeriods, Objective),
	indomain(Objective),
	labeling([min], CPeriods), !,
	format('Max load: ~d\nCourse periods: ~w\n', [Objective,CPeriods]).

bacp(ID, CPeriods, Objective) :-
	param(ID, NCourses, NPeriods, LoadLB, LoadUB, CPPLB, CPPUB),
	length(CPeriods, NCourses),
	domain(CPeriods, 1, NPeriods),
	Objective in LoadLB..LoadUB,
	ObjectiveGap #= Objective - LoadLB,
	course_load(ID, CourseLoad),
	findall(P0-Q0, prerequisite(ID,P0,Q0), PQs),
	(   for(P,1,NPeriods),
	    foreach(P-Cnt,KeyCounts),
	    foreach(task(P,1,_,H,0),Tasks2),
	    foreach(H,Hs),
	    param(CPPLB,CPPUB,ObjectiveGap)
	do  Cnt in CPPLB..CPPUB,
	    H #>= 0,
	    H #=< ObjectiveGap
	),
	(   foreach(CP,CPeriods),
	    foreach(CL,CourseLoad),
	    fromto(0,Sum1,Sum2,CLSum),
	    count(No,1,_),
	    foreach(task(CP,1,_,CL,No),Tasks1)
	do  Sum2 is Sum1+CL
	),
	(   foreach(P1-Q1,PQs),
	    foreach(N1-N2 #= Dis,Precs),
	    param(ID)
	do  course_no(ID, P1, N1),
	    course_no(ID, Q1, N2), !,
	    Dis in 1..sup
	),
	global_cardinality(CPeriods, KeyCounts),
	append(Tasks1, Tasks2, Tasks),
	sum(Hs, #=, HSum),
	HSum + CLSum #= NPeriods*Objective,
	cumulative(Tasks, [limit(Objective),precedences(Precs)/*,global(true)*/]).

:- discontiguous
	param/7,
	course_no/3,
	course_load/2,
	prerequisite/3.
:- dynamic
	param/7,
	course_no/3,
	course_load/2,
	prerequisite/3.


% n_courses = 46;
% n_periods = 8;
% load_per_period_lb = 10;
% load_per_period_ub = 24;
% courses_per_period_lb = 2;
% courses_per_period_ub = 10;
param(8, 46,8,10,24,2,10).

course_no(8, dew100, 1).
course_no(8, fis100, 2).
course_no(8, hcw310, 3).
course_no(8, iwg101, 4).
course_no(8, mat190, 5).
course_no(8, mat192, 6).
course_no(8, dew101, 7).
course_no(8, fis101, 8).
course_no(8, iwi131, 9).
course_no(8, mat191, 10).
course_no(8, mat193, 11).
course_no(8, fis102, 12).
course_no(8, hxwxx1, 13).
course_no(8, iei134, 14).
course_no(8, iei141, 15).
course_no(8, mat194, 16).
course_no(8, dewxx0, 17).
course_no(8, hcw311, 18).
course_no(8, iei132, 19).
course_no(8, iei133, 20).
course_no(8, iei142, 21).
course_no(8, iei162, 22).
course_no(8, iwn170, 23).
course_no(8, mat195, 24).
course_no(8, hxwxx2, 25).
course_no(8, iei231, 26).
course_no(8, iei241, 27).
course_no(8, iei271, 28).
course_no(8, iei281, 29).
course_no(8, iwn261, 30).
course_no(8, hfw120, 31).
course_no(8, iei233, 32).
course_no(8, iei238, 33).
course_no(8, iei261, 34).
course_no(8, iei272, 35).
course_no(8, iei273, 36).
course_no(8, iei161, 37).
course_no(8, iei232, 38).
course_no(8, iei262, 39).
course_no(8, iei274, 40).
course_no(8, iwi365, 41).
course_no(8, iwn270, 42).
course_no(8, hrw130, 43).
course_no(8, iei218, 44).
course_no(8, iei219, 45).
course_no(8, iei248, 46).

course_load(8, [1,  3,  1,  2,  4, 4,  1,  5,  3,  4, 4,  5,  1,  3,  3, 4,  1,  1,  3,  3, 3,  3,  3,  3,  1, 4,  4,  3,  3,  3, 2,  4,  3,  3,  3, 3,  3,  3,  3,  3, 3,  3,  2,  3,  3, 3]).


prerequisite(8, dew101, dew100).
prerequisite(8, fis101, fis100).
prerequisite(8, fis101, mat192).
prerequisite(8, mat191, mat190).
prerequisite(8, mat193, mat190).
prerequisite(8, mat193, mat192).
prerequisite(8, fis102, fis101).
prerequisite(8, fis102, mat193).
prerequisite(8, iei134, iwi131).
prerequisite(8, iei141, iwi131).
prerequisite(8, mat194, mat191).
prerequisite(8, mat194, mat193).
prerequisite(8, dewxx0, dew101).
prerequisite(8, hcw311, hcw310).
prerequisite(8, iei132, iei134).
prerequisite(8, iei133, iei134).
prerequisite(8, iei142, iei141).
prerequisite(8, mat195, mat194).
prerequisite(8, iei231, iei134).
prerequisite(8, iei241, iei142).
prerequisite(8, iei271, iei162).
prerequisite(8, iei281, mat195).
prerequisite(8, iei233, iei231).
prerequisite(8, iei238, iei231).
prerequisite(8, iei261, iwn261).
prerequisite(8, iei272, iei271).
prerequisite(8, iei273, iei271).
prerequisite(8, iei273, iei271).
prerequisite(8, iei161, iwn261).
prerequisite(8, iei161, iwn261).
prerequisite(8, iei232, iei273).
prerequisite(8, iei232, iei273).
prerequisite(8, iei262, iwn261).
prerequisite(8, iei274, iei273).
prerequisite(8, iei274, iei273).
prerequisite(8, iei219, iei232).
prerequisite(8, iei248, iei233).
prerequisite(8, iei248, iei233).

% n_courses = 42;
% n_periods = 10;
% load_per_period_lb = 10;
% load_per_period_ub = 24;
% courses_per_period_lb = 2;
% courses_per_period_ub = 10;
param(10, 42,10,10,24,2,10).

course_no(10, dew100, 1).
course_no(10, fis100, 2).
course_no(10, hrwxx1, 3).
course_no(10, iwg101, 4).
course_no(10, mat021, 5).
course_no(10, qui010, 6).
course_no(10, dew101, 7).
course_no(10, fis110, 8).
course_no(10, hrwxx2, 9).
course_no(10, iwi131, 10).
course_no(10, mat022, 11).
course_no(10, dewxx0, 12).
course_no(10, fis120, 13).
course_no(10, hcw310, 14).
course_no(10, hrwxx3, 15).
course_no(10, ili134, 16).
course_no(10, ili151, 17).
course_no(10, mat023, 18).
course_no(10, hcw311, 19).
course_no(10, ili135, 20).
course_no(10, ili153, 21).
course_no(10, ili260, 22).
course_no(10, iwn261, 23).
course_no(10, mat024, 24).
course_no(10, fis130, 25).
course_no(10, ili239, 26).
course_no(10, ili245, 27).
course_no(10, ili253, 28).
course_no(10, fis140, 29).
course_no(10, ili236, 30).
course_no(10, ili243, 31).
course_no(10, ili270, 32).
course_no(10, ili280, 33).
course_no(10, ici344, 34).
course_no(10, ili263, 35).
course_no(10, ili332, 36).
course_no(10, ili355, 37).
course_no(10, iwn170, 38).
course_no(10, icdxx1, 39).
course_no(10, ili362, 40).
course_no(10, iwn270, 41).
course_no(10, icdxx2, 42).

course_load(10, [1, 3, 2, 2, 5, 3, 1, 5, 2, 3, 5, 1, 4, 1, 2, 4, 3, 4, 1, 4, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 3, 4, 4, 3, 4, 4, 3, 3, 3, 3, 3]).

prerequisite(10, dew101, dew100).
prerequisite(10, fis110, fis100).
prerequisite(10, fis110, mat021).
prerequisite(10, mat022, mat021).
prerequisite(10, dewxx0, dew101).
prerequisite(10, fis120, fis110).
prerequisite(10, fis120, mat022).
prerequisite(10, ili134, iwi131).
prerequisite(10, ili151, iwi131).
prerequisite(10, mat023, mat022).
prerequisite(10, hcw311, hcw310).
prerequisite(10, ili135, ili134).
prerequisite(10, ili153, ili134).
prerequisite(10, ili153, ili151).
prerequisite(10, mat024, mat023).
prerequisite(10, fis130, fis110).
prerequisite(10, fis130, mat022).
prerequisite(10, ili239, ili135).
prerequisite(10, ili245, ili153).
prerequisite(10, ili253, ili153).
prerequisite(10, fis140, fis120).
prerequisite(10, fis140, fis130).
prerequisite(10, ili236, ili239).
prerequisite(10, ili243, ili245).
prerequisite(10, ili270, ili260).
prerequisite(10, ili270, iwn261).
prerequisite(10, ili280, mat024).
prerequisite(10, ici344, ili243).
prerequisite(10, ili263, ili260).
prerequisite(10, ili263, iwn261).
prerequisite(10, ili332, ili236).
prerequisite(10, ili355, ili153).
prerequisite(10, ili355, ili280).
prerequisite(10, ili362, ili263).

% n_courses = 66;
% n_periods = 12;
% load_per_period_lb = 10;
% load_per_period_ub = 24;
% courses_per_period_lb = 2;
% courses_per_period_ub = 10;
param(12, 66,12,10,24,2,10).

course_no(12, dew100, 1).
course_no(12, fis100, 2).
course_no(12, hcw310, 3).
course_no(12, iwg101, 4).
course_no(12, mat111, 5).
course_no(12, mat121, 6).
course_no(12, dew101, 7).
course_no(12, fis110, 8).
course_no(12, iwi131, 9).
course_no(12, mat112, 10).
course_no(12, mat122, 11).
course_no(12, dewxx0, 12).
course_no(12, fis120, 13).
course_no(12, hcw311, 14).
course_no(12, hxwxx1, 15).
course_no(12, ili142, 16).
course_no(12, mat113, 17).
course_no(12, mat123, 18).
course_no(12, fis130, 19).
course_no(12, ili134, 20).
course_no(12, ili151, 21).
course_no(12, iwm185, 22).
course_no(12, mat124, 23).
course_no(12, fis140, 24).
course_no(12, hxwxx2, 25).
course_no(12, ile260, 26).
course_no(12, ili260, 27).
course_no(12, iwn170, 28).
course_no(12, qui104, 29).
course_no(12, ili231, 30).
course_no(12, ili243, 31).
course_no(12, ili252, 32).
course_no(12, ili273, 33).
course_no(12, mat210, 34).
course_no(12, mat260, 35).
course_no(12, ild208, 36).
course_no(12, ili221, 37).
course_no(12, ili274, 38).
course_no(12, ili281, 39).
course_no(12, iwn270, 40).
course_no(12, mat270, 41).
course_no(12, hrw150, 42).
course_no(12, ili238, 43).
course_no(12, ili242, 44).
course_no(12, ili275, 45).
course_no(12, ili355, 46).
course_no(12, hrw110, 47).
course_no(12, ici393, 48).
course_no(12, ili237, 49).
course_no(12, ili334, 50).
course_no(12, ili363, 51).
course_no(12, iwn261, 52).
course_no(12, hrw100, 53).
course_no(12, ici382, 54).
course_no(12, ili331, 55).
course_no(12, ili362, 56).
course_no(12, ili381, 57).
course_no(12, iln230, 58).
course_no(12, ici313, 59).
course_no(12, ici315, 60).
course_no(12, ici332, 61).
course_no(12, ici344, 62).
course_no(12, icn336, 63).
course_no(12, iwi365, 64).
course_no(12, ici314, 65).
course_no(12, ici367, 66).

course_load(12, [1, 3, 1, 2, 4, 4, 1, 5, 3, 4, 4, 1, 4, 1, 1, 4, 4, 4, 4, 4, 3, 3, 4, 4, 1, 3, 3, 3, 3, 3, 4, 4, 3, 4, 4, 3, 4, 3, 3, 3, 4, 2, 4, 3, 3, 4, 2, 4, 4, 4, 3, 3, 2, 4, 4, 3, 3, 3, 2, 2, 3, 4, 3, 3, 2, 2]).

prerequisite(12, dew101, dew100).
prerequisite(12, fis110, fis100).
prerequisite(12, fis110, mat121).
prerequisite(12, mat112, mat111).
prerequisite(12, mat122, mat111).
prerequisite(12, mat122, mat121).
prerequisite(12, dewxx0, dew101).
prerequisite(12, fis120, fis110).
prerequisite(12, fis120, mat122).
prerequisite(12, hcw311, hcw310).
prerequisite(12, ili142, iwi131).
prerequisite(12, mat113, mat112).
prerequisite(12, mat113, mat122).
prerequisite(12, mat123, mat112).
prerequisite(12, mat123, mat122).
prerequisite(12, fis130, fis110).
prerequisite(12, fis130, mat122).
prerequisite(12, ili134, iwi131).
prerequisite(12, ili151, mat112).
prerequisite(12, mat124, mat113).
prerequisite(12, mat124, mat123).
prerequisite(12, fis140, fis120).
prerequisite(12, fis140, fis130).
prerequisite(12, ile260, fis120).
prerequisite(12, ile260, mat124).
prerequisite(12, ili231, iwi131).
prerequisite(12, ili252, iwi131).
prerequisite(12, ili273, ili260).
prerequisite(12, mat210, mat113).
prerequisite(12, mat260, iwi131).
prerequisite(12, mat260, mat113).
prerequisite(12, mat260, mat123).
prerequisite(12, ili221, ili134).
prerequisite(12, ili221, ili231).
prerequisite(12, ili221, mat260).
prerequisite(12, ili274, ili273).
prerequisite(12, ili281, mat260).
prerequisite(12, mat270, iwi131).
prerequisite(12, mat270, mat113).
prerequisite(12, mat270, mat123).
prerequisite(12, ili238, ili134).
prerequisite(12, ili242, ili142).
prerequisite(12, ili275, ili274).
prerequisite(12, ili355, ili221).
prerequisite(12, hrw110, hrw150).
prerequisite(12, ici393, mat210).
prerequisite(12, ici393, mat260).
prerequisite(12, ili237, ili231).
prerequisite(12, ili237, ili252).
prerequisite(12, ili334, ili238).
prerequisite(12, ili363, ili273).
prerequisite(12, hrw100, hrw110).
prerequisite(12, ici382, ili334).
prerequisite(12, ili331, ili238).
prerequisite(12, ili331, ili274).
prerequisite(12, ili362, ili363).
prerequisite(12, ili381, ili281).
prerequisite(12, ili381, mat210).
prerequisite(12, iln230, iwn170).
prerequisite(12, ici313, ili331).
prerequisite(12, ici332, ici393).
prerequisite(12, ici332, ili331).
prerequisite(12, ici344, ili243).
prerequisite(12, icn336, ici393).
prerequisite(12, ici314, ici313).

