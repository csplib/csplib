---
Title:    Balanced Academic Curriculum Problem (BACP)
Proposer: 
	- Brahim Hnich
	- Zeynep Kiziltan
	- Toby Walsh
Category: Scheduling and related problems
---


The BACP is to design a balanced academic curriculum by assigning periods to courses in a way that the academic load of each period is balanced, i.e., as similar as possible . The curriculum must obey the following administrative and academic regulations: 

* Academic curriculum: an academic curriculum is defined by a set of courses and a set of prerequisite relationships among them. 
* Number of periods: courses must be assigned within a maximum number of academic periods. 
* Academic load: each course has associated a number of credits or units that represent the academic effort required to successfully follow it. 
* Prerequisites: some courses can have other courses as prerequisites. 
* Minimum academic load: a minimum number of academic credits per period is required to consider a student as full time. 
* Maximum academic load: a maximum number of academic credits per period is allowed in order to avoid overload. 
* Minimum number of courses: a minimum number of courses per period is required to consider a student as full time. 
* Maximum number of courses: a maximum number of courses per period is allowed in order to avoid overload. 

The goal is to assign a period to every course in a way that the minimum and maximum academic load for each period, the minimum and maximum number of courses for each period, and the prerequisite relationships are satisfied. An optimal balanced curriculum minimises the maximum academic load for all periods.

Note that we could consider other types of balance criterion, such as minimising an expression on the deviation from the mean load per period. This is explored in Monette et al


**The Generalised Balanced Academic Curriculum Problem**

Marco Chiarandini, Luca Di Gaspero, Stefano Gualandi, and Andrea Schaerf have proposed a more challenging <a href="http://www.diegm.uniud.it/satt/projects/bacp/">generalised version</a> of the problem. 