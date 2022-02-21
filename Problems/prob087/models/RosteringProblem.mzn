include "all_equal.mzn";
include "global_cardinality.mzn";

% load parameters
include "parameters.dzn";

int: daysPerWeek = 7;

% the number of weeks
int: numberOfWeeks;

int: numberOfDays = numberOfWeeks * daysPerWeek;

% the minimum number of days in a row with the same shift
int: s_min;

% the maximum number of days in a row with the same shift
int: s_max; 

% 3 shifts: early = 1, late = 2 and night shift = 3 + day off (rest day) = 0
int: numberOfShifts = 3;

% the shift requirement matrix, including at M_{i,j} the needed employees for day i and shift j
array[1..daysPerWeek, 1..numberOfShifts+1] of int: shiftRequirements;


% the variables X 
array[1..numberOfWeeks * daysPerWeek] of var 0..numberOfShifts: plan1d;
array[1..numberOfWeeks,1..daysPerWeek] of var 0..numberOfShifts: plan2d;
array[1..daysPerWeek,1..numberOfWeeks] of var 0..numberOfShifts: plan2dT;

% helper arrays
array[1..s_min,1..s_min] of var 0..numberOfShifts: s_min_arrays;
array[1..s_max,1..s_max] of var 0..numberOfShifts: s_max_arrays;

% convert the 2D plan into 1D
constraint forall(week in 1..numberOfWeeks, day in 1..daysPerWeek) (
                 plan2d[week, day] == plan1d[(week-1) * daysPerWeek + day]
           );  

% transpose the 2D plan
constraint forall(week in 1..numberOfWeeks, day in 1..daysPerWeek) (
                 plan2d[week, day] == plan2dT[day, week]
           );

% C_equalDays: constrains that weekend days (Saturday and Sunday) always have the same shift
constraint forall(week in 1..numberOfWeeks) (
               plan2d[week, daysPerWeek - 1] == plan2d[week, daysPerWeek]
           );                        

                                                                                                                                
% create the sub arrays other the array bounds
constraint forall(i in 1..s_min, j in 1..s_min) (
                 s_min_arrays[i, j] == plan1d[((numberOfDays - s_min - 1 + i + j) mod numberOfDays) + 1]
           );                                            
                                            
% C_shiftRepetitions:for every shift type a minimum number of consecutive assignments to this shift is given
constraint forall(day in 1..numberOfDays - s_min) (
               plan1d[day] != plan1d[day+1] -> all_equal(plan1d[day+1..day+s_min])
           );     
% the constraints over the array bounds           
constraint forall(d in 1..s_min) (
               plan1d[d + numberOfDays - s_min] != plan1d[((d + numberOfDays - s_min) mod numberOfDays) +1] -> all_equal(s_min_arrays[d,1..s_min])
           );                     

           
% create the sub arrays other the array bounds
constraint forall(i in 1..s_max, j in 1..s_max) (
                 s_max_arrays[i, j] == plan1d[((numberOfDays - s_max - 2 + i + j) mod numberOfDays) + 1]
           );                      
                                            
% C_shiftRepetitions:for every shift type a maximum number of consecutive assignments to this shift is given           
constraint forall(day in 1..numberOfWeeks * daysPerWeek - s_max) (
               (all_equal(plan1d[day..day+s_max])) -> (plan1d[day] != plan1d[day + s_max])
           );               
% the constraints over the array bounds             
constraint forall(d in 1..s_max) (
               (all_equal(s_max_arrays[d, 1..s_max])) -> (plan1d[d + numberOfDays - s_max] != plan1d[d])
           );                     

                               
                                                                                                   
% C_restDays: at least 2 days must be rest days every 2 weeks.
constraint forall(day in 1..(numberOfWeeks - 2) * daysPerWeek) (
               count(j in plan1d[day..day + daysPerWeek * 2])(j=0) >= 2                   
           );            
constraint forall(i in 1..2 * daysPerWeek-1) (
               count(j in plan1d[numberOfWeeks * daysPerWeek-i .. numberOfWeeks * daysPerWeek] ++ plan1d[1..2*daysPerWeek-i])(j=0) >= 2    
           );         
                                            
% C_shiftOrder: restricts the order of shifts. There is a forward rotating principle. This means, that after an early shift there can only follow a shift with the same or a higher value, or a rest shift.
constraint forall(day in 2..numberOfWeeks * daysPerWeek - 1) (
               plan1d[day] <= plan1d[day + 1] \/ plan1d[day+1] == 0
           );         
constraint plan1d[1] >= plan1d[numberOfDays] \/ plan1d[1] == 0;
                      
% C_shiftRequirements: for every weekday for each shift the number of required staff is provided (e.g. usually less staff is required on the weekend) 
constraint forall(day in 1..daysPerWeek) (
              (global_cardinality(row(plan2dT,day),[0,1,2,3],row(shiftRequirements,day)))   
           );    
           
% solve the problem           
solve :: int_search(plan1d, input_order, indomain_min, complete)
      satisfy;           
      
% print the problem      
output [ show(row(plan2d,j)) ++ "\n" | j in 1..numberOfWeeks ] ++ ["\n"];      