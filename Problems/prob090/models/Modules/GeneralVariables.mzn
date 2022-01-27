/*
 * Variables for MiniZinc
 * ---------------------------------
 *
 * @b Description
 * This file is to be included in all Minizinc models.
 * It provides the definitions for the input and decision variables used in all models.
 *
 * @author 
 *    David Bogdan
 *    bogdan.david02@e-uvt.ro
 */

/***********************************
 ****                           ****
 ****      INPUT VARIABLES      ****
 ****                           ****
 ***********************************/

% Maximum number of VMs required taken from ComputeVM
int: VM;

% Number of components of the application
int: NoComponents;

% Number of hardware requirements for a component (e.g. CPU, Memory, Storage, etc.)
int: HardwareREQ;

% Requirements for each component
array[1..NoComponents, 1..HardwareREQ] of int: CompREQ;

% Number of VM Offers
int: VMOffers;

% VM specifications
array[1..VMOffers, 1..HardwareREQ] of int: VMSpecs;
array[1..VMOffers] of int: VMPrice;

% Global variable used in conflict constraints
set of int: S = { i | i in 1..NoComponents};

/***********************************
 ****                           ****
 ****     DECISION VARIABLS     ****
 ****                           ****
 ***********************************/
 
array[1..NoComponents, 1..VM] of var 0..1: AssignmentMatrix;
array[1..VM] of var 0..1: OccupancyVector;
array[1..VM] of var int: VMType;

% Data of the virtual machine on which the ith component has been deployed
array[1..VM] of var 1..64: CPU;
array[1..VM] of var 1700..976000: Memory;
array[1..VM] of var 1000..24000: Storage;
array[1..VM] of var 0..16000: Price;