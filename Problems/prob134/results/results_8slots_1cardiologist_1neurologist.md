---
title: Solution of eight-slot appointment with 1 cardiologist and 1 neurologist
preferred_dates: [11.07.2025, 14.07.2025]
undesired_weekdays: [ Tu (1) ]
preferred_times: "Thu(3), slotIds 0–7 (8:0–09:30)"
preferred_doctors:
  cardiologists: [4, 8]
  neurologists: [13, 17]
---


Solution found:
Appointment Slots:
appSlots[0]=72 appSlots[1]=73 appSlots[2]=74 appSlots[3]=75 appSlots[4]=76 appSlots[5]=77 appSlots[6]=78 appSlots[7]=79 appResource_CARDIOLOGY=8 appResource_NEUROLOGIE=17 
** Choco 4.10.9 (2022-08) : Constraint Programming Solver, Copyright (c) 2010-2022
- Model[MASP Scheduling Problem] features:
	Variables : 1067
	Constraints : 799
	Building time : 0.292s
	User-defined search strategy : no
	Complementary search strategy : no
- Complete search - 1 solution found.
	Model[MASP Scheduling Problem]
	Solutions: 1
	MINIMIZE Optimize Soft Constraints = 0,
	Building time : 0.292s
	Resolution time : 0.204s
	Time to best solution : 0.184s
	Nodes: 131 (643.3 n/s) 
	Backtracks: 261
	Backjumps: 0
	Fails: 130
	Restarts: 0
[INFO] ------------------------------------------------------------------------
[INFO] BUILD SUCCESS
[INFO] ------------------------------------------------------------------------
[INFO] Total time:  1.169 s
[INFO] Finished at: 2025-07-11T10:56:43+02:00
[INFO] ------------------------------------------------------------------------
