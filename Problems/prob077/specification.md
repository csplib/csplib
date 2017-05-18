---
Title:    Stochastic Assignment and Scheduling Problem 
Proposer: David Hemmi, Guido Tack and Mark Wallace
Category: Stochastic Constraint Programming
---
The stochastic assignment and scheduling problem is a two-stage stochastic optimisation problem with recourse. 

A set of jobs, each composed of multiple tasks, is to be scheduled on a set of machines. 
Precedence constraints ensure that tasks, which belong to the same job are executed sequentially. 
Once the processing of a task has started, it can not be interrupted (non preemptive scheduling).
The tasks may be restricted to a sub-set of machines.
No more that one task may be executed concurrently on a machine.
The task processing time depends on the selected machine, e.g. certain machines can finish a task faster than others.
Furthermore, the processing times are subject to uncertainty, e.g. random variables. 
Scenarios are used to describe the uncertainty. 
A scenario describes a situation where all processing times are known and a complete schedule can be created.
We assume that all processing times are known at the beginning of the second stage. 

The problem is composed of two stages. 
In the first stage, all the tasks have to be allocated to a machine. 
Once the tasks are allocated, their processing time is revealed. 
In the second stage a schedule for each machine is created, with respect to the observed processing times. 
The objective is to find a task to machine assignment minimizing the expected (average) makespan over all scenarios.

