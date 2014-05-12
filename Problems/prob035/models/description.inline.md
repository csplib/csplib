One natural model (see [1]) is a pair of matrices of decision
variables. The expression for the determinant is cumbersome but, at orders 4 and
greater, there is some repetition which can be assigned to a single variable.

<P>
Exchanging a pair of rows or a pair of columns of a matrix negates the sign of
the determinant. Hence, this problem does not quite have row and column symmetry.
However, if we insist that the absolute value of the determinant is 1 then we can
apply row and column symmetry breaking, for example by lexicographic and
all-permutation ordering. If a solution has a determinant of -1
(and we are solving a variant where a positive determinant is required) we can
simply exchange a pair of rows/columns in the solution.

<P>
The problem also has diagonal symmetry which can easily be broken by adding
further symmetry-breaking constraints (again, see [1]).

<OL>
 <LI>
  A. M. Frisch, C. Jefferson, I. Miguel,
  "Constraints for Breaking More Row and Column Symmetries,"
  Proceedings of the 9th International Conference on Principles and
  Practice of Constraint Programming, 2003.
 </LI>

</OL>