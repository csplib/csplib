---
Title: Equidistant Frequency Permutation Arrays
Proposer: Peter Nightingale
Category: Combinatorial mathematics
---

Informally, the problem is to find a set (optionally of maximal size) of codewords, such
that any pair of codewords are Hamming distance $d$ apart. Each codeword (which may
be considered as a sequence) is made up of symbols from the alphabet $\\{1, \ldots, q\\}$, with
each symbol occurring a fixed number $\lambda$ of times per codeword.

More precisely, the problem has parameters $v$, $q$, $\lambda$, $d$ and it is to find a set $E$ of size $v$, of sequences
of length $q\lambda$, such that each sequence contains $\lambda$ of each symbol in the set $\\{1, \ldots, q\\}$.
For each pair of sequences in $E$, the pair are Hamming distance $d$ apart (i.e. there are $d$
places where the sequences disagree). 

For the parameters $v=5$, $q=3$, $\lambda =2$, $d=4$, the 
following table shows a set $E=\\{c_1, c_2, c_3, c_4, c_5 \\}$.

Symbol  | Codeword
------  | --------
$c_1$ | 0 0 1 1 2 2
$c_2$ | 0 1 0 2 1 2
$c_3$ | 0 1 2 0 2 1
$c_4$ | 0 2 1 2 0 1
$c_5$ | 0 2 2 1 1 0




