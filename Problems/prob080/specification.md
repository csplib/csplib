---
Title:    Blocked n-Queens Problem
Proposer: Ian Gent
Category: Games and Puzzles
---

Overview
========

The {prob054} Problem, [prob054], suffers from the problem that its complexity is trivial as a decision problem.  
The n-Queens Completion and Blocked n-Queens problems are variants of {prob054} which have been proven to
be NP-Complete as decision problems and #P-Complete as counting problems.


Random Instance Generators
==========================

Python generators are available to implement the model from the paper cite{nqueenscompletion}. 
<a href="data/blocked-gen.py.html">Generator for random blocked n-Queens instances</a>


References
==========

The Blocked $n$-Queens problem was proposed by cite{blocked-queens} and used in ASP Competitions
cite{asp-competition-07}. The problem was proved NP-Complete and #P-Complete by cite{nqueenscompletion}, and the same paper 
introduced a generator of ranodm instances.


