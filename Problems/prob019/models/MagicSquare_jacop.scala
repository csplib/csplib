/*

  Magic square in JaCoP/Scala.
  
  This model was written by Hakan Kjellerstrand (hakank@gmail.com).
  See my JaCoP/Scala page: http://www.hakank.org/jacop/jacop_scala.html
   
*/

% Licenced under CC-BY-4.0 : http://creativecommons.org/licenses/by/4.0/

import scalaJaCoP._

object MagicSquare extends App with jacop {


  val n = 4
  val n2 = n*n
  val x = List.tabulate(n)(i=> 
                List.tabulate(n)(j=>
                  new IntVar("x("+i+","+j+")", 1, n2)))
  // val total = new IntVar("total", 1, n*n*n)
  val total = (n * (n*n + 1) / 2)


  // constraints
  alldifferent(x.flatten.toArray)

  
  // rows and columns
  for(i <- 0 until n) {
    sum( Array.tabulate(n)(j=> x(i)(j)) ) #= total
    sum( Array.tabulate(n)(j=> x(j)(i)) ) #= total
  }
  
  // diagonals
  sum( Array.tabulate(n)(i=> x(i)(i)) ) #= total
  sum( Array.tabulate(n)(i=> x(i)(n-i-1)) ) #= total

  // symmetry breaking
  // x(0)(0)   #< x(0)(n-1)
  // x(0)(n-1) #< x(n-1)(0)
  // x(0)(0)   #< x(n-1)(n-1)

  // numberSolutions(2)

   // search
  val result = satisfyAll(search(x.flatten, smallest_min, indomain_min), printIt) 
  

  statistics
 
  def printIt() {
    println("\nSolution:")
    for(i <- 0 until n) {
      for(j <- 0 until n) {
        print(x(i)(j).value + " ")
      }
      println()
    }
    println()
  }
     
}

