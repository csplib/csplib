package psp


import oscar.cp.modeling._
import oscar.cp._
import scala.io.Source
import oscar.util._
//import stockingCost._

//import stockingCost._

/**
 * 
 *  Production planning problems, such as Lot Sizing and Scheduling Problems, 
 *  require one to determine a minimum cost production schedule to satisfy 
 *  the demands for single or multiple items without exceeding machine 
 *  capacities while satisfying demands. 
 *  
 *  The Lot Sizing problem considered here is a discrete, multi-item, 
 *  single machine problem with capacity of production limited to one per period. 
 *  There are storage costs and sequence-dependent changeover costs, 
 *  respecting the triangle inequality. 
 *  The changeover cost $q^{i, j}$ is induced when passing from the production 
 *  of item $i$ to another one $j$ with $q^{i, i} = 0 for all i$.
 *  Backlogging is not allowed, each order has to be produced before 
 *  the corresponding demand time, and stocking (inventory) costs has to be paid 
 *  proportional to the number of days between the production date and 
 *  the demand time. 
 *  The objective is to minimize the sum of stocking costs and change over costs.
 *  
 *  @author Vinasetan Ratheil Houndji, ratheilesse@gmail.com
 *  @author Pierre Schaus, pschaus@gmail.com
 * 
 */


/* 
 * Instances : 15a 15b 15c 30a 30b 30c 100a 100b 100c 200a
 * Available at http://becool.info.ucl.ac.be/resources/discrete-lot-sizing-problem
*/

object DiscreteLotSizing extends App {

  val stockingConstraint: Int = 1
  /* 
   * stockingConstraint = 0 => Basic decomposition
   * stockingConstraint = 1 => AllDifferentBC and (sum(dealine(p) - date(p) <= H)
   * stockingConstraint = 2 => minAssignment and AllDifferentBC
   * stockingConstraint = 3 => stockingCost constraint
   */

  //******************** Problem Data *******************************
  val src = "ratheil/src/main/scala/data/pigment30a.txt"
  val lines = Source.fromFile(src).getLines.reduceLeft(_ + " " + _)
  val vals = lines.split("[ ,\t]").toList.filterNot(_ == "").map(_.toInt)
  var index = 0
  def next() = {
    index += 1
    vals(index - 1)
  }
  println(vals)
  val nbPeriods = next()
  val nbItems = next()
  val demands = Array.fill(nbItems, nbPeriods)(next)
  val stockingCost = next
  val changeOverCost = Array.tabulate(nbItems, nbItems) { case (i, j) => next }

  val optimalCost = next()

  for (a <- 0 until nbItems) {
    println(changeOverCost(a).mkString("\t"))
  }
  //*****************************************************************
  
  val cp = CPSolver()

  case class Order(val article: Int, val dueDate: Int, val date: CPIntVar)
  val productions = (for (a <- 0 until nbItems; t <- 0 until nbPeriods; if (demands(a)(t) == 1)) yield {
    Order(a, t, CPIntVar(cp, 0 to t)) // we force it (through the domain) to be produced before due date
  }).toArray
  val date: Array[CPIntVar] = productions.map(_.date) :+ CPIntVar(cp, nbPeriods)
  val dueDate = productions.map(_.dueDate)
  val article = productions.map(_.article)
  val nbDemands = article.size
  println("nbPeriods: " + nbPeriods)
  println("nbItems: " + nbItems)
  println("nbDemands: " + nbDemands)
  val successors = Array.tabulate(nbDemands + 1)(i => CPIntVar(cp, 0 to nbDemands))
  val costMatrix = Array.tabulate(nbDemands + 1, nbDemands + 1) {
    case (i, j) =>
      if (i == nbDemands || j == nbDemands) 0
      else changeOverCost(article(i))(article(j))
  }
  val costMatrixHolding = Array.tabulate(nbDemands, nbPeriods) {
    case (i, t) =>
      if (t > dueDate(i)) 100000
      else dueDate(i) - t
  }

  val t0 = System.currentTimeMillis()

  //Constraints
  cp.add(circuit(successors), Strong)
  val transitionCosts: CPIntVar = sum(0 until nbDemands)(i => costMatrix(i)(successors(i)))
  cp.add(minAssignment(successors, costMatrix, transitionCosts))

  // stocking constraint
  val stockingCosts: CPIntVar = sum(date.zip(dueDate).map { case (x, d) => -x + d })
  
  if (stockingConstraint == 0) {
    for (t <- 0 until nbPeriods) {
      cp.add((sum(0 until nbDemands)(d => date(d) === t)) <= 1)
    }    
  } else if (stockingConstraint == 1) {
    cp.add(allDifferent(date), Medium)
    cp.add(stockingCosts == -sum(0 until nbDemands)(d => date(d) - dueDate(d)))
  } else if (stockingConstraint == 2) {
    cp.add(allDifferent(date), Medium)
    cp.add(minAssignment(date.take(nbDemands), costMatrixHolding, stockingCosts))
  } else if (stockingConstraint == 3) {
    cp.add(new StockingCost(date.take(nbDemands), dueDate, stockingCosts, 1))
  }  

  // symmetry break
  for (p <- 0 until (nbDemands - 1); if (article(p) == article(p + 1))) {
    cp.add(date(p) < date(p + 1))
  }
  for (p1 <- 0 until (nbDemands - 1); p2 <- p1 until nbDemands; if (article(p1) == article(p2))) {
    cp.add(successors(p2) != p1)
  }
  for (p <- 0 until nbDemands) {
    cp.add(date(p) < date(successors(p)))
  }

  //objective
  val obj: CPIntVar = (stockingCosts * stockingCost) + transitionCosts
  cp.minimize(obj)
  
  //search
  cp.search {
    binaryStatic(date) ++ binaryStatic(successors)
  }

  val stats = cp.start()
  println(stats)
  println(src)
  println("stockingConstraint: " + stockingConstraint)
  println("optimalCost: " + optimalCost)

}