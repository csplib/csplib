---
Title: Optimal Financial Portfolio Design
Proposer: Pierre Flener and Jean-Noël Monette
Category: Design and configuration
---

An OPD problem $\langle v, b, r \rangle$ is to find a matrix of $v$
rows and $b$ columns of $0$-$1$ values such that each row sums to $r$,
and the maximum, denoted $\lambda$, of the dot products beween all
pairs of distinct rows is minimal.  Equivalently, the objective is to
find $v$ subsets of cardinality $r$ drawn from a given set of $b$
elements, such that the largest intersection of any two of the $v$
sets has minimal cardinality, denoted $\lambda$.

This is an abstract description of a problem that appears in finance:
full details are given by cite{Flener:CP04} and
cite{Flener:CONS07:CDO2}.  In a typical OPD in finance, we have $250
\leq v \leq 500$ and $4 \leq b \leq 25$, with $r \approx 100$.  This
is one order of magnitude larger than the largest
[BIBDs](http://csplib.org/Problems/prob028) that have been built by
computer using systematic search; the BIBD problem, which is a
constraint satisfaction problem, is closely related to the OPD
problem, which is a constrained optimisation problem.  A lower bound
on the number of shared elements of any pair of same-sized subsets
drawn from a given set was established by cite{Sivertsson:MSc05,
Flener:AOC08}: this lower bound can be applied to the objective value
$\lambda$.  A first constraint-based model, with advanced
symmetry-handling methods, was proposed by cite{Flener:CP04}, then
improved by cite{Sivertsson:MSc05} and ultimately by
cite{Flener:CONS07:CDO2}, by using the lower bound.  As pointed out by
cite{Agren:CP05}, one can advantageously exploit the many symmetries
by using local search instead of systematic search; this was confirmed
by cite{Lebbah:ENDM15}, by cite{Lebbah:IJAMC15}, and at the [MiniZinc
Challenge 2015](http://www.minizinc.org/challenge2015/challenge.html),
where a constraint-based local search solver outperformed all
systematic search solvers, even on sub-realistic instances.