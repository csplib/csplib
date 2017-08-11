---
Title:    Blocked n-Queens Problem
Proposer: Ian Gent
Category: Games and Puzzles
---

Overview
========

The {prob054} Problem, [prob054], suffers from the problem that its complexity is trivial as a decision problem.  
The Blocked n-Queens problems is a variant of {prob054} which has been proven to
be NP-Complete as a decision problem and #P-Complete as a counting problem.


The Blocked $n$-Queens problem is a variant where, as well as $n$, the input contains a list of squares which are blocked. A solution to the problem is a solution to the $n$-Queens problem containing no queens on any of the blocked 
squares.


Instances
==========================

Python generators are available to implement the model from the paper cite{nqueenscompletion}. 
<a href="data/blocked-gen.py.html">Generator for random blocked n-Queens instances</a>

Also provided are instances from the ASP Competitions (thanks to Martin Gebser for providing these.)   Results of ASP solvers on those instances can be found at 
<a href="https://asparagus.cs.uni-potsdam.de/contest/">the 2007 ASP Competition</a> and 
<a href="https://dtai.cs.kuleuven.be/events/ASP-competition/index.shtml">the 2009 ASP Competition</a> pages.


References
==========

The Blocked $n$-Queens problem was proposed by cite{blocked-queens} and used in ASP Competitions
cite{asp-competition-07}. The problem was proved NP-Complete and #P-Complete by cite{nqueenscompletion}, and the same paper 
introduced a generator of ranodm instances.



