/*
 * Wordpress model for MiniZinc
 * ---------------------------------
 *
 * @author 
 *    David Bogdan
 *    bogdan.david02@e-uvt.ro
 *
 * This model was originally designed and implemented by Andrei Iovescu (andrei.iovescu99@e- uvt.ro). I only adapted it so it would have the same format as the other problems.
 */
 
/***********************************
 ****                           ****
 ****      EXTERNAL MODULES     ****
 ****                           ****
 ***********************************/
include "Modules/SymmetryBreaking.mzn";
include "Modules/GeneralVariables.mzn";
include "Modules/GeneralConstraints.mzn";


/***********************************
 ****                           ****
 ****      INPUT VARIABLES      ****
 ****                           ****
 ***********************************/
int: WPInstances;
%set of int: FVComponents;
%array[int] of int: FVInstances;

/***********************************
 ****                           ****
 ****        CONSTANTS          ****
 ****                           ****
 ***********************************/
int: Wordpress = 1;
int: MySQL = 2;
int: DNS_LoadBalancer = 3;
int: HTTP_LoadBalancer = 4;
int: Varnish = 5;

/***********************************
 ****                           ****
 ****      MODELING PROBLEM     ****
 ****                           ****
 ***********************************/
constraint basicAllocation(AssignmentMatrix, {DNS_LoadBalancer, HTTP_LoadBalancer}, S, VM);
constraint setType(AssignmentMatrix, VMType, NoComponents, VM);
constraint occupancy(AssignmentMatrix, OccupancyVector, NoComponents, VM);
constraint capacity(AssignmentMatrix, CompREQ, VMSpecs, VMType, HardwareREQ, NoComponents, VM);
constraint link(VMSpecs, VMPrice, OccupancyVector, VMType, CPU, Memory, Storage, Price, VMOffers, VM);
constraint nullType(AssignmentMatrix, VMType, NoComponents, VM);

% Problem specific constraints
constraint exclusiveDeployment(AssignmentMatrix, VM, DNS_LoadBalancer, HTTP_LoadBalancer);

constraint sum(k in 1..VM)(AssignmentMatrix[DNS_LoadBalancer, k]) > 0 -> requireProvide(AssignmentMatrix, VM, Wordpress, DNS_LoadBalancer, 1, 7);
constraint sum(k in 1..VM)(AssignmentMatrix[HTTP_LoadBalancer, k]) > 0 -> requireProvide(AssignmentMatrix, VM, Wordpress, HTTP_LoadBalancer, 1, 3);
constraint requireProvide(AssignmentMatrix, VM, Wordpress, MySQL, 2, 3);

constraint conflict(AssignmentMatrix, {DNS_LoadBalancer, HTTP_LoadBalancer, MySQL}, VM, Varnish);
constraint conflict(AssignmentMatrix, {Wordpress, MySQL, Varnish}, VM, DNS_LoadBalancer);
constraint conflict(AssignmentMatrix, {Wordpress, MySQL, Varnish}, VM, HTTP_LoadBalancer);

constraint lowerBound(AssignmentMatrix, VM, Wordpress, WPInstances);
constraint lowerBound(AssignmentMatrix, VM, Varnish, 2);
constraint lowerBound(AssignmentMatrix, VM, MySQL, 2);
constraint upperBound(AssignmentMatrix, VM, DNS_LoadBalancer, 1);

/***********************************
 ****                           ****
 ****       OPTIMIZATIONS       ****
 ****                           ****
 ***********************************/

% See 'SymmetryBreaking.mzn' for constraint definition
%constraint LLX(AssignmentMatrix, NoComponents, VM);
%constraint PRLLX(Price, AssignmentMatrix, NoComponents, VM, sum(i in FVInstances)(i) + 1);
%constraint FV(AssignmentMatrix, FVComponents, FVInstances);
%constraint PRL(Price, AssignmentMatrix, NoComponents, VM, sum(i in FVInstances)(i) + 1);

/***********************************
 ****                           ****
 ****      SOLVING PROBLEM      ****
 ****                           ****
 ***********************************/ 
solve minimize sum(p in Price)(p);
