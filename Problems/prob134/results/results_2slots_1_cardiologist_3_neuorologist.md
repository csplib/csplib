---
Title: Solutions two-slot appointment with 1 cardiologist and 3 neurologists
preferred_dates: [14.07.2025, 15.07.2025]
undesired_weekdays: [ Fri (4)]
preferred_times: "Monday, slotsIds 0–1 (08:00–08:30), Tu, slotsIds 2-3 (8:30-09:00)"
preferred_doctors:
  cardiologists: [4, 8]
  neurologists: [13,17]
---
Solution found:
Appointment Slots:
appSlots[0]=168 appSlots[1]=169 appResource_CARDIOLOGY=4 appResource_NEUROLOGIE=13 appResource_NEUROLOGIE=10 appResource_NEUROLOGIE=18 
** Choco 4.10.9 (2022-08) : Constraint Programming Solver, Copyright (c) 2010-2022
- Model[MASP Scheduling Problem] features:
	Variables : 2691
	Constraints : 1905
	Building time : 0.166s
	User-defined search strategy : no
	Complementary search strategy : no
- Complete search - 1 solution found.
	Model[MASP Scheduling Problem]
	Solutions: 1
	MINIMIZE Optimize Soft Constraints = 0,
	Building time : 0.166s
	Resolution time : 0.208s
	Time to best solution : 0.189s
	Nodes: 139 (669.6 n/s) 
	Backtracks: 277
	Backjumps: 0
	Fails: 138
	Restarts: 0
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  1.014 s
[INFO] Finished at: 2025-07-11T10:29:37+02:00
[INFO] ------------------------------------------------------------------------

