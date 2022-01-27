/*
 * Symmetry breaking constraints for MiniZinc
 * ---------------------------------
 *
 * @b Description
 * This file is to be included in all Minizinc models.
 * It provides the definitions for symmetry breaking constraints.
 *
 * @author 
 *    David Bogdan
 *    bogdan.david02@e-uvt.ro
 */
 
/******************************************
 ****                                  ****
 ****      AUXILIARY FUNCTIONS         ****
 ****                                  ****
 ******************************************/
 
function var bool: isLexicographic(array[int, int] of var 0..1: AssignmentMatrix,
                                     int: Row,
                                     int: Column,
                                     int: NoComponents) =
    if (Row == NoComponents) then
       if (AssignmentMatrix[Row,Column] <= AssignmentMatrix[Row, Column+1]) then true else false endif
    else
       if (AssignmentMatrix[Row, Column] == AssignmentMatrix[Row, Column+1]) then 
           isLexicographic(AssignmentMatrix, Row+1, Column, NoComponents) 
       else
           if (AssignmentMatrix[Row, Column] < AssignmentMatrix[Row, Column+1]) then true else false endif
       endif
    endif;
 
 
/******************************************
 ****                                  ****
 ****      SINGLE OPTIMIZATIONS        ****
 ****                                  ****
 ******************************************/
 
% Sort by price
predicate PR(array[int] of var 0..16000: Price, int: VM, int: start) =
    forall(k in start..VM-1)(Price[k] <= Price[k+1]);
    
% Sort by occupancy
predicate L(array[int,int] of var 0..1: AssignmentMatrix,
            int: NoComponents,
            int: VM,
            int: start) =
    forall(k in start..VM-1)(sum(i in 1..NoComponents)(AssignmentMatrix[i,k]) <=
                         sum(i in 1..NoComponents)(AssignmentMatrix[i,k+1]));
                         
% Sort lexicographically
predicate LX(array[int, int] of var 0..1: AssignmentMatrix,
             int: NoComponents,
             int: VM,
             int: start) =
    forall(k in start..VM-1)(isLexicographic(AssignmentMatrix, 1, k, NoComponents));
    

% Fixed Value
predicate FV(array[int, int] of var 0..1: AssignmentMatrix,
             set of int: Comp,
             array[int] of int: CompInst) =
      forall(k in 1..card(Comp))(
           forall(i in 1..CompInst[k])(
               if (k > 1) then (
                  AssignmentMatrix[Comp[k], i + sum(j in 1..k-1)(CompInst[j])] = 1
               ) else (
                  AssignmentMatrix[Comp[k], i] = 1
               ) endif
           )
      );

/******************************************
 ****                                  ****
 ****      DOUBLE OPTIMIZATIONS        ****
 ****                                  ****
 ******************************************/
 
% Sort by price and in case of equality sort by occupancy
predicate PRL(array[int] of var 0..16000: Price,
              array[int, int] of var 0..1: AssignmentMatrix,
              int: NoComponents,
              int: VM,
              int: start) =
    PR(Price, VM, start) /\ forall(k in start..VM-1)(if (Price[k] == Price[k+1]) then sum(i in 1..NoComponents)(AssignmentMatrix[i,k]) <=
                                                                           sum(i in 1..NoComponents)(AssignmentMatrix[i,k+1]) endif);

% Sort by price and in case of equality sort lexicographically  
predicate PRLX(array[int] of var 0..16000: Price,
               array[int, int] of var 0..1: AssignmentMatrix,
               int: NoComponents,
               int: VM,
               int: start) =
    PR(Price, VM, start) /\ forall(k in start..VM-1)(if (Price[k] == Price[k+1]) then isLexicographic(AssignmentMatrix, 1, k, NoComponents) endif);                                                              
                                                                                                                                                                                                      
% Sort by occupancy and in case of equality sort by price
predicate LPR(array[int] of var 0..16000: Price,
              array[int, int] of var 0..1: AssignmentMatrix,
              int: NoComponents,
              int: VM,
              int: start) =
    L(AssignmentMatrix, NoComponents, VM, start) /\ forall(k in start..VM-1)(if (sum(i in 1..NoComponents)(AssignmentMatrix[i,k]) ==
                                                                      sum(i in 1..NoComponents)(AssignmentMatrix[i,k+1])) then Price[k] <= Price[k+1] endif);
                                                                      
% Sort by occupancy and in case of equality sort lexicographically
predicate LLX(array[int, int] of var 0..1: AssignmentMatrix,
              int: NoComponents,
              int: VM,
              int: start) =
    L(AssignmentMatrix, NoComponents, VM, start) /\ forall(k in start..VM-1)(if (sum(i in 1..NoComponents)(AssignmentMatrix[i,k]) ==
                                                                      sum(i in 1..NoComponents)(AssignmentMatrix[i,k+1])) then     isLexicographic(AssignmentMatrix, 1, k, NoComponents) endif);
	
/******************************************
 ****                                  ****
 ****      TRIPLE OPTIMIZATIONS        ****
 ****                                  ****
 ******************************************/

predicate PRLLX(array[int] of var 0..16000: Price,
                array[int, int] of var 0..1: AssignmentMatrix,
                int: NoComponents,
                int: VM,
                int: start) =
    PRL(Price, AssignmentMatrix, NoComponents, VM, start) /\ forall(k in start..VM-1)(if (sum(i in 1..NoComponents)(AssignmentMatrix[i,k]) ==
                                                                      sum(i in 1..NoComponents)(AssignmentMatrix[i,k+1]) /\ Price[k] == Price[k+1]) then     isLexicographic(AssignmentMatrix, 1, k, NoComponents) endif);
                                                                      
predicate LPRLX(array[int] of var 0..16000: Price,
                array[int, int] of var 0..1: AssignmentMatrix,
                int: NoComponents,
                int: VM,
                int: start) =
    LPR(Price, AssignmentMatrix, NoComponents, VM, start) /\ forall(k in start..VM-1)(if (Price[k] == Price[k+1] /\ sum(i in 1..NoComponents)(AssignmentMatrix[i,k]) ==
                                                                      sum(i in 1..NoComponents)(AssignmentMatrix[i,k+1])) then isLexicographic(AssignmentMatrix, 1, k, NoComponents) endif);
