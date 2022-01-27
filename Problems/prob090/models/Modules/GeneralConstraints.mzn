/*
 * Constraints for MiniZinc
 * ---------------------------------
 *
 * @b Description
 * This file is to be included in all Minizinc models.
 * It provides the definitions for the general constraints used in all models.
 *
 * @author 
 *    David Bogdan
 *    bogdan.david02@e-uvt.ro
 */

% Checks whether a specific component is deployed on at least one VM.
function var int: isDeployed(array[int, int] of var 0..1: AssignmentMatrix,
                             int: VM,
                             int: compID) =
    if(sum(k in 1..VM)(AssignmentMatrix[compID,k]) > 0) then 1 else 0 endif;
    
    
% Ensures the NULL (0) Type is reserved only for unused virtual machines
predicate setType(array[int, int] of var 0..1: AssignmentMatrix,
                  array[int] of var int: VMType,
                  int: NoComponents,
                  int: VM) =
    forall(k in 1..VM)(
          (sum(i in 1..NoComponents)(AssignmentMatrix[i,k])) >= 1 -> (VMType[k] > 0));   
   
                
% Ensures used VMs have their occupancy vector set
predicate occupancy(array[int, int] of var 0..1: AssignmentMatrix,
                    array[int] of var 0..1: OccupancyVector,
                    int: NoComponents,
                    int: VM) =
    forall(k in 1..VM)((sum(i in 1..NoComponents) 
          (AssignmentMatrix[i,k])) >= 1 -> (OccupancyVector[k] = 1));
  
                  
% Ensures hardware requirements are met for each component
predicate capacity(array[int, int] of var 0..1: AssignmentMatrix,
                   array[int, int] of var int: CompREQ,
                   array[int, int] of var int: VMSpecs,
                   array[int] of var int: VMType,
                   int: HardwareREQ,
                   int: NoComponents,
                   int: VM) =
    forall(k in 1..VM, h in 1..HardwareREQ)  
          (sum(i in 1..NoComponents) 
          (AssignmentMatrix[i,k] * CompREQ[i,h]) <= VMSpecs[VMType[k],h]);
          
          
% Links resource arrays to VM specifications
predicate link(array[int, int] of var int: VMSpecs,
               array[int] of var int: VMPrice,
               array[int] of var 0..1: OccupancyVector,
               array[int] of var int: VMType,
               array[int] of var 1..64: CPU,
               array[int] of var 1700..976000: Memory,
               array[int] of var 1000..24000: Storage,
               array[int] of var 0..16000: Price,
               int: VMOffers,
               int: VM) =
    forall(k in 1..VM, o in 1..VMOffers)
          ((OccupancyVector[k] == 1 /\ VMType[k] == o) ->
          (CPU[k] == VMSpecs[o,1] /\ Memory[k] == VMSpecs[o,2] /\
           Storage[k] == VMSpecs[o,3] /\ Price[k] == VMPrice[o]));


% Ensures each component is deployed at least once
predicate basicAllocation (array[int, int] of var 0..1: AssignmentMatrix,
                           var set of int: exclusive,
                           set of int: S,
                           int: VM) 
  = forall( i in S diff exclusive)(sum (k in 1..VM) (AssignmentMatrix[i, k]) >= 1);


% Ensures unused machines have a set (default) type.
predicate nullType(array[int, int] of var 0..1: AssignmentMatrix,
                   array[int] of var int: VMType,
                   int: NoComponents,
                   int: VM) = 
    forall(k in 1..VM)(sum(i in 1..NoComponents) 
          
          % If set to 0, it produces invalid index error and discards solutions which don't use all VMs
          % If set to 1, it doesn't affect the minimal price as the price for the said VM will be 0 if it is unoccupied
          (AssignmentMatrix[i,k]) == 0 -> VMType[k] = 1);


% Ensures a component is deployed [AT LEAST | AT MOST | EXACTLY] a set number of times
predicate lowerBound(array[int, int] of var 0..1: AssignmentMatrix,
                     int: VM,
                     int: compID, 
                     int: compID2, 
                     int: bound) =
    sum(k in 1..VM)(AssignmentMatrix[compID,k]) + sum(k in 1..VM)(AssignmentMatrix[compID2, k]) >= bound;
 
predicate lowerBound(array[int, int] of var 0..1: AssignmentMatrix,
                     int: VM,
                     int: compID,
                     int: bound) =
    sum(k in 1..VM)(AssignmentMatrix[compID,k]) >= bound;   
          
predicate upperBound(array[int, int] of var 0..1: AssignmentMatrix,
                     int: VM,
                     int: compID, 
                     int: bound) =
    sum(k in 1..VM) (AssignmentMatrix[compID, k]) <= bound;
    
predicate equalBound(array[int, int] of var 0..1: AssignmentMatrix,
                     int: VM,
                     int: compID, 
                     int: bound) =
    sum(k in 1..VM) (AssignmentMatrix[compID, k]) == bound;
   

% Ensures Provide-specific constraints         
predicate requireProvide(array[int, int] of var 0..1: AssignmentMatrix,
                         int: VM,
                         int: compID1, 
                         int: compID2, 
                         int: NoInstID1,
                         int: NoInstID2) =
    NoInstID1 * sum(k in 1..VM) (AssignmentMatrix[compID1, k]) <=
    NoInstID2 * sum(k in 1..VM) (AssignmentMatrix[compID2, k]);
    
predicate provide(array[int, int] of var 0..1: AssignmentMatrix,
                  int: VM,
                  int: compID1, 
                  int: compID2, 
                  int: NoInstID1) =
    sum(k in 1..VM) (AssignmentMatrix[compID1, k]) <=
    sum(k in 1..VM) (AssignmentMatrix[compID2, k]) * NoInstID1;

predicate boundedRequireProvide(array[int, int] of var 0..1: AssignmentMatrix,
                                int: VM,
                                int: compID1, 
                                int: compID2, 
                                int: NoInstID2) =
   (sum(k in 1..VM)(AssignmentMatrix[compID1, k]) * NoInstID2 - sum(k in 1..VM)(AssignmentMatrix[compID2,k]) > 0) /\ 
   (sum(k in 1..VM)(AssignmentMatrix[compID1, k]) * NoInstID2 - sum(k in 1..VM)(AssignmentMatrix[compID2,k])  <= NoInstID2);
     
    
% Ensures conflicting components do not share the same VM
predicate conflict(array[int, int] of var 0..1: AssignmentMatrix,
                   var set of int: conflicts,
                   int: VM,
                   int: compID) =
    forall(k in 1..VM, i in conflicts)
          (AssignmentMatrix[compID, k] + AssignmentMatrix[i, k] <= 1);
   
  
% Ensures a set component is deployed on all VMs except those containing conflicting components
predicate fullDeployment(array[int, int] of var 0..1: AssignmentMatrix,
                         var set of int: conflicts,
                         int: VM,
                         int: NoComponents,
                         int: compID) =
         forall(k in 1..VM, i in 1..NoComponents diff conflicts)
               (AssignmentMatrix[i,k] == 1 -> AssignmentMatrix[compID, k] = 1);

% Ensures components in a given set are placed on the same VM
predicate colocation(array[int, int] of var 0..1: AssignmentMatrix,
                     var set of int: components,
                     int: VM) =
        forall(i in components, j in components)
              (forall(k in 1..VM)(AssignmentMatrix[i,k] == 1 -> AssignmentMatrix[j,k] = 1));

% Ensures that two components are not deployed simultaneously
predicate exclusiveDeployment(array[int, int] of var 0..1: AssignmentMatrix,
                              int: VM,
                              int: CompID1,
                              int: CompID2) =
        sum(k in 1..VM)(AssignmentMatrix[CompID1, k]) > 0 xor sum(k in 1..VM)(AssignmentMatrix[CompID2, k]) > 0;