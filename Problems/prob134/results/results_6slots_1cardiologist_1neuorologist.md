---
title: Solution of six-slot appointment with 1 cardiologist and 1 neurologist
preferred_dates: [11.07.2025, 14.07.2025]
undesired_weekdays: [Mon (0), Tu (1) ]
preferred_times: "Friday(4), slotIds 6–11 (9:30–11:00)"
preferred_doctors:
  cardiologists: [4, 8]
  neurologists: [13, 17]
---

Solution found:
Appointment Slots:
appSlots[0]=106 appSlots[1]=107 appSlots[2]=108 appSlots[3]=109 appResource_CARDIOLOGY=8 appResource_NEUROLOGIE=17 
** Choco 4.10.9 (2022-08) : Constraint Programming Solver, Copyright (c) 2010-2022
- Model[MASP Scheduling Problem] features:
	Variables : 1463
	Constraints : 1091
	Building time : 0.184s
	User-defined search strategy : no
	Complementary search strategy : no
- Complete search - 1 solution found.
	Model[MASP Scheduling Problem]
	Solutions: 1
	MINIMIZE Optimize Soft Constraints = 0,
	Building time : 0.184s
	Resolution time : 0.206s
	Time to best solution : 0.184s
	Nodes: 163 (790.1 n/s) 
	Backtracks: 325
	Backjumps: 0
	Fails: 162
	Restarts: 0
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  1.036 s
[INFO] Finished at: 2025-07-11T10:49:31+02:00
[INFO] ------------------------------------------------------------------------

