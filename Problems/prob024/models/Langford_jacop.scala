/*

  Langford's number problem in JaCoP/Scala.

  Langford's number problem (CSP lib problem 24)
  http://www.csplib.org/Problems/prob024/
  """
  Arrange 2 sets of positive integers 1..k to a sequence,
  such that, following the first occurence of an integer i, 
  each subsequent occurrence of i, appears i+1 indices later
  than the last. 
  For example, for k=4, a solution would be 41312432
  """
  
  * John E. Miller: Langford's Problem
    http://www.lclark.edu/~miller/langford.html
  
  * Encyclopedia of Integer Sequences for the number of solutions for each k
    http://www.research.att.com/cgi-bin/access.cgi/as/njas/sequences/eisA.cgi?Anum=014552
 


  This model was written by Hakan Kjellerstrand (hakank@gmail.com).
  See my JaCoP/Scala page: http://www.hakank.org/jacop/jacop_scala.html
   
*/

// Licenced under CC-BY-4.0 : http://creativecommons.org/licenses/by/4.0/

import scalaJaCoP._
import sys._


object Langford extends App with jacop {

  // data
  var k = 4

  if (args.length > 0) {
    k = args(0).toInt
  }

  // variables
  val position = Array.tabulate(2*k)(i=> new IntVar("position("+i+")", 0, 2*k-1))
  val solution = Array.tabulate(2*k)(i=> new IntVar("solution("+i+")", 1, k))

  // constraints
  alldifferent(position)
  
  for(i <- 1 until k+1) {
    position(i+k-1) #= position(i-1) + i+1
    element(position(i-1), solution, i:IntVar, -1)
    element(position(k+i-1), solution, i:IntVar, -1)
  }

  // symmetry breaking
  solution(0) #< solution(2*k-1)

  // search
  val result = satisfyAll(search(position ++ solution, max_regret, indomain_min), printIt) 
  
  statistics
  
  def printIt() {
    print("\nsolution: ")
    solution.foreach(v=>print(v.value + " "))
    print("\nposition: ")
    position.foreach(v=>print(v.value + " "))
    println()
  }
     
}

