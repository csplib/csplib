Title:    Quasigroup Existence
Proposer: Toby Walsh 
Category: Combinatorial mathematics


An order m quasigroup is a Latin square of size m. That is, a m by m multiplication table in which each element occurs once in every row and column. For example,
```
1	 2	 3	 4
4	 1	 2	 3
3	 4	 1	 2
2	 3	 4	 1
````
is an order 4 quasigroup. A quasigroup can be specified by a set and a binary multiplication opertor, \* defined over this set.
Quasigroup existence problems determine the existence or non-existence of quasigroups of a given size with additional properties. Certain existence problems are of sufficient interest that a naming scheme has been invented for them. We define two new relations, \*321 and \*312 by $a \*321 b = c$ iff $c\*b=a$ and $a \*312 b = c$ iff $b\*c=a$.

$1+2$

QG1.m problems are order m quasigroups for which if $a\*b=c$  $a\*b=c\*d$ and $a \*321 b = c \*321 d$ then $a=c$ and $b=d$.

QG2.m problems are order m quasigroups for which if a\*b=c\*d and a \*312 b = c \*312 d then a=c and b=d.

QG3.m problems are order m quasigroups for which $(a\*b)\*(b\*a) = a$.

QG4.m problems are order m quasigroups for which $(b\*a)\*(a\*b) = a$.

QG5.m problems are order m quasigroups for which $((b\*a)\*b)\*b = a$.

QG6.m problems are order m quasigroups for which $(a\*b)\*b = a\*(a\*b)$.

QG7.m problems are order m quasigroups for which $(b\*a)\*b = a\*(b\*a)$.

For each of these problems, we may additionally demand that the quasigroup is idempotent. That is, a\*a=a for every element a.


