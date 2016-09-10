---
Title:    Resource-Constrained Project Scheduling Problem (RCPSP)
Proposer: Peter Nightingale and Emir Demirović
Category: Scheduling and related problems
---

The resource-constrained project scheduling problem is a classical well-known problem in operations research. 
A number of activities are to be scheduled. Each activity has a duration and cannot be interrupted. 
There are a set of precedence relations between pairs of activities which state that the second activity must start after the first has finished.
The set of precedence relations are usually given as a directed acyclic graph (DAG), where the edge (*u*,*v*) represents a precedence relation where *u* must finish before *v* begins. The DAG contains two additional activities with duration 0, the *source* and *sink*, where the *source* is the first activity and *sink* is the last activity (these are dummy activities).  

There are a set of renewable resources. Each resource has a maximum capacity and at any given time slot no more than this amount can be in use. Each activity has a demand (possibly zero) on each resource. The dummy *source* and *sink* activities have zero demand on all resources. 

The problem is usually stated as an optimisation problem where the makespan (i.e. the completion time of the *sink* activity) is minimised. 

# Multi-mode Variant

An extension of the basic RCPSP is the *multi-mode* variant where activities may have multiple *modes*. The mode dictates the duration and resource demands of the activity. In this variant, the schedule must give the mode of each activity as well as its starting time. 

The PSPLIB repository has a set of benchmark instances of both ordinary and multi-mode RCPSP \cite{psplib-paper}.

