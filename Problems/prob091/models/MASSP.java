/**
 * @author: Dr. rer. nat. George Assaf (Brandenburg University of Technology (BTU), Cottbus, Germany)
 * @version: 1.0
 * @date: 2025-10-26
 * @description: Prototype implementation of the medical appointment sequence scheduling problem(MASSP) 
*/
package com.example.massp;


import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;
import java.util.Arrays;
public class MASSP {
    
    private Model model;
    private ArrayList<ArrayList<CalendarDomain>> potentialResourceSlot;
    private int[][] appID2Resources;
    private IntVar[] resourceVariables;
    private IntVar[][] sequenceSlots;
    private IntVar totalWorkloadPenalty;
    private Map<Integer, Integer> resourceIndex2AppointmentMap;
    private List<Integer>                 similiarResourcesList;
    private Map<Integer, List<Integer>>   resourceToIndices;



    public MASSP() {

        Appointment maxResAppointment = MASPConfigLarge.getAppointmentWithMostResources();


        this.appID2Resources = new int[MASPConfigLarge.sequence.size()][(maxResAppointment.getRequiredResources().size())+1];
        
        this.model = new Model("MASSP");

        this.potentialResourceSlot  = new ArrayList<ArrayList<CalendarDomain>>();

        int max = MASPConfigLarge.getMaxAppointmentDuration();

        this.sequenceSlots = new IntVar[max][MASPConfigLarge.sequence.size()];

    }



    public void solve() {

        initialize();

        //declare decision variables
        assignVariableDomains();

        //different resources per each appointment
        applyAllDifferentConstraints();

        //link resources with appointment slots and ensure sequential slots per appointment
        ensureSequentialSlotsPerAppointment();

        // No overlapping time slots for appointments
        NoTimeSlotOverlapping();

        // Set the chronological order of the sequence
        setChronologicalOrderOfSequence1( MASPConfigLarge.chronologicalOrder);

        ensureMinimalDistanceBetweenAppointments(sequenceSlots, MASPConfigLarge.min_distance );

        ensureMaximalDistanceBetweenAppointments(sequenceSlots, MASPConfigLarge.max_distance );

        //sameResourceconstraint();

        totalWorkloadPenalty = optimizeResourcceWorkload();
       // totalWorkloadPenalty = optimizeResourcecWorkload();

        model.setObjective(Model.MINIMIZE, totalWorkloadPenalty);



        Solver solver = model.getSolver();
 

        while (solver.solve()) {
        System.out.println("=== Solution ===");

        for (Map.Entry<Integer, Appointment> entry : MASPConfigLarge.sequence.entrySet()) {
            int appointmentId = entry.getKey();
            int appIndex = appointmentId - 1;
            Appointment app = entry.getValue();

            // Print all resource assignments for this appointment
            System.out.print("Appointment " + app.getId() + " (" + app.getRequiredResources() + "): ");
            System.out.print("assigned resource IDs = [");
            
            boolean first = true;
            for (int i = 0; i < resourceVariables.length; i++) {
                if (resourceIndex2AppointmentMap.get(i) == appIndex) {
                    if (!first) {
                        System.out.print(", ");
                    }
                    System.out.print(resourceVariables[i].getValue());
                    first = false;
                }
            }
            System.out.println("]");

            // Print slot identifiers for this appointment
            System.out.print("  Slots: ");
            for (int slot = 0; slot < app.getDurationInSlot(); slot++) {
                System.out.print(sequenceSlots[slot][appIndex].getValue() + " ");
            }
            System.out.println();
        }
        System.out.println();
    }
    solver.printStatistics();

     }


    private void initialize() {
        
        int requiredResourceId = 0;
        int maxResources = MASPConfigLarge.getAppointmentWithMostResources().getRequiredResources().size();


        for(int i=0;i<appID2Resources.length;i++) {
            appID2Resources[i][0] = i;
            Appointment appointment = MASPConfigLarge.sequence.get(i+1);
             for(int j=0;j<maxResources;j++) {

               if(appointment.getRequiredResources().size()>requiredResourceId) {
                
                 appID2Resources[i][j+1] = appointment.getRequiredResources().get(requiredResourceId).ordinal();

               } else {
                  appID2Resources[i][j+1] = -1;
               }
                 requiredResourceId++;
            }
            requiredResourceId = 0;
        }


        for(int i = 0; i < appID2Resources.length; i++)
        {
            for(int j = 1; j < appID2Resources[i].length; j++)
            {
                if(appID2Resources[i][j] != -1)
                {
                    potentialResourceSlot.add(new ArrayList<CalendarDomain>());
                }
            }
        }
    }



    private void assignVariableDomains() {

         int noResources = MASPConfigLarge.getTotalNumberOfResources();
        this.resourceVariables= new IntVar[noResources];

        this.resourceIndex2AppointmentMap = new HashMap<>();
        int resourceIndex =0;
        int resourceId=0;


        for(int i=0;i<appID2Resources.length;i++) {

            for(int j=1;j<appID2Resources[i].length;j++) {

            if(appID2Resources[i][j] == ResourceType.CARDIOLOGY.ordinal() && appID2Resources[i][j] !=-1)  {

            resourceVariables[resourceIndex] = model.intVar("Resource_var " + i, MASPConfigLarge.CARDIOLOGY_DOMAIN);

            for (int k = 0; k <  MASPConfigLarge.CARDIOLOGY_DOMAIN.length; k++) {
                CalendarDomain calendarDomain = new CalendarDomain(MASPConfigLarge.CARDIOLOGY_DOMAIN[k],
                        MASPConfigLarge.getFilteredGlobalCalendar(),
                        MASPConfigLarge.getFilteredResourceCalendar(),
                        MASPConfigLarge.SLOTS_PER_DAY,
                        MASPConfigLarge.CURRENT_WORKLOAD[MASPConfigLarge.CARDIOLOGY_DOMAIN[k]]
                );
                
                calendarDomain.findDate(model);

                potentialResourceSlot.get(resourceIndex).add(calendarDomain);
            }
            resourceIndex2AppointmentMap.put(resourceIndex++, i);

            }else if(appID2Resources[i][j] !=-1){
            
            resourceVariables[resourceIndex] = model.intVar("Resource_var " + i, MASPConfigLarge.NEUROLOGY_DOMAIN);

            for (int k = 0; k <  MASPConfigLarge.NEUROLOGY_DOMAIN.length; k++) {
                CalendarDomain calendarDomain = new CalendarDomain(MASPConfigLarge.NEUROLOGY_DOMAIN[k],
                        MASPConfigLarge.getFilteredGlobalCalendar(),
                        MASPConfigLarge.getFilteredResourceCalendar(),
                        MASPConfigLarge.SLOTS_PER_DAY,
                        MASPConfigLarge.CURRENT_WORKLOAD[MASPConfigLarge.NEUROLOGY_DOMAIN[k]]
                );
                calendarDomain.findDate(model);
                potentialResourceSlot.get(resourceIndex).add(calendarDomain);
            }
            resourceIndex2AppointmentMap.put(resourceIndex++, i);

            }
        }
    }


    // Initialize sequence start slots of appointments
    for (int i = 0; i < sequenceSlots[0].length; i++) {
         sequenceSlots[0][i] = model.intVar("SeqSlot_0_App" + (i+1), MASPConfigLarge.getFilteredGlobalCalendar());
    }
}



    private void applyAllDifferentConstraints() {
    // Collect resource variables per appointment
    Map<Integer, List<IntVar>> appointment2Vars = new HashMap<>();

    for (int idx = 0; idx < resourceVariables.length; idx++) {
        int appointmentId = resourceIndex2AppointmentMap.get(idx);
        appointment2Vars
            .computeIfAbsent(appointmentId, k -> new ArrayList<>())
            .add(resourceVariables[idx]);
    }

    // Apply allDifferent per appointment
    for (Map.Entry<Integer, List<IntVar>> entry : appointment2Vars.entrySet()) {
        List<IntVar> vars = entry.getValue();
        if (vars.size() > 1) {
            model.allDifferent(vars.toArray(new IntVar[0])).post();
        }
    }
}

private void ensureSequentialSlotsPerAppointment() {


    // link resources to slots
    for (int i=0;i<potentialResourceSlot.size();i++) {
        int appointmentId = resourceIndex2AppointmentMap.get(i);
        //Appointment appointment = MASPConfigLarge.sequence.get(appointmentId + 1);
        //System.out.println("Linking resource variable " + resourceVariables[i].getName() +
        // " to appointment " + (appointmentId + 1) + " starting slot variable " + sequenceSlots[0][appointmentId].getName());

        for (int j = 0; j < potentialResourceSlot.get(i).size(); j++) {
             model.ifThen(model.arithm(resourceVariables[i], "=", potentialResourceSlot.get(i).get(j).getResourceId()),
              //todo: fix this bug here for mapping resources to correct appointment
              model.arithm(sequenceSlots[0][appointmentId], "=", potentialResourceSlot.get(i).get(j).getCalendarVar()));
        }
    }

     //ensure sequential slots per each appointment
     for(int i=0;i<sequenceSlots[0].length;i++)
    {
           for(int j=1;j<sequenceSlots.length;j++)
        {
            if(j>=MASPConfigLarge.sequence.get(i+1).getDurationInSlot())
            {//when the appointment duration is greater than the number of slots
                sequenceSlots[j][i] = null;//model.intVar(-1*sequenceSlots.length*sequenceSlots.length, -1);
               // System.out.println("Appointment " + (i+1) + " exceeds available slots at "+ j);
            }
            else 
            {
                sequenceSlots[j][i] = model.intVar(MASPConfigLarge.getFilteredGlobalCalendar());
                model.arithm(sequenceSlots[j][i], "=", sequenceSlots[j-1][i].add(1).intVar()).post();
            }
        }
    
    }
}


private void NoTimeSlotOverlapping() {


        int index = 0;
        for(int i = 0; i < sequenceSlots.length; i++)
        {
            for(int j = 0; j < sequenceSlots[0].length; j++)
            {  
                if(sequenceSlots[i][j] == null) continue;

                 
                index++;
            }
        }
         
        IntVar[] unfoldedSequenceSlots = new IntVar[index];

        index = 0;
        for(int i = 0; i < sequenceSlots.length; i++)
        {
            for(int j = 0; j < sequenceSlots[0].length; j++)
            {  
                if(sequenceSlots[i][j] == null) continue;

                unfoldedSequenceSlots[index] = sequenceSlots[i][j];
                index++;
            }
        }

        model.allDifferent(unfoldedSequenceSlots).post();

}


private void setChronologicalOrderOfSequence1(List<Integer> requestedOrderList) {
  //  System.out.println("Applying chronological order: " + requestedOrderList);
    
    for (int i = 0; i < requestedOrderList.size() - 1; i++) {
        int aId = requestedOrderList.get(i);
        int bId = requestedOrderList.get(i + 1);
        int aIndex = aId - 1;
        int bIndex = bId - 1;
        
   /*      System.out.printf("Constraint: slot[%d][%d] (%s) <= slot[%d][%d] (%s)%n",
            aIndex, 0, sequenceSlots[0][aIndex].getName(),
            bIndex, 0, sequenceSlots[0][bIndex].getName()); */
        
        model.arithm(sequenceSlots[0][aIndex], "<=", sequenceSlots[0][bIndex]).post();
    }
}


 /* This method ensures that the minimal distance between two appointments is satisfied
     * @param appointments: The appointments
     * @param minDistanceList: The list of minimal distances
     * @param appointmentDurationsInSlots: The appointment durations in slots
     * @param NoSinD: The number of slots per day
    */
    private void ensureMinimalDistanceBetweenAppointments(IntVar[][] appointments, int[][] minDistanceList) 
    {

        if (minDistanceList.length == 0 || appointments.length == 0) {
            return;  // Nothing to process if minDistance or appointments are empty
        }

        for(int i=0;i<minDistanceList.length;i++)
        {
            for(int j=0;j<minDistanceList[i].length;j++)
            {
                if(minDistanceList[i][j] > 0)
                {
                    int minimalDistanceInDays = minDistanceList[i][j];

                    if (minimalDistanceInDays <= 0) {
                        continue;
                    }

                    // Convert minimal distance from days to slots (days * NoSinD slots per day)
                    int minimalDistanceInSlots = minimalDistanceInDays * MASPConfigLarge.SLOTS_PER_DAY;

                    // Ensure that the minimal distance in slots is enforced
                    if (minimalDistanceInSlots % MASPConfigLarge.SLOTS_PER_DAY == 0)// day case
                    {
                        int appDuration =  MASPConfigLarge.sequence.get(i + 1).getDurationInSlot();//appointmentDurationsInSlots[i];  // Get the duration of the current appointment
                        

                        model.arithm(appointments[0][j].sub(appointments[appDuration - 1][i]).add(appointments[0][i].mod(MASPConfigLarge.SLOTS_PER_DAY)).intVar(), ">=", minimalDistanceInSlots).post();;
                    }
                    else
                    {
                        //todo hours case
                    }
                }
            }
        }
    }



     /**
     * This method ensures that the maximal distance between two appointments is satisfied
     * @param appointments: The appointments
     * @param maxDistanceMatrix: The maximal distance matrix
     * @param appointmentDurationsInSlots: The appointment durations in slots
     * @param NoSinD: The number of slots per day
    */
    private void ensureMaximalDistanceBetweenAppointments(IntVar[][] appointments, int[][] maxDistanceMatrix) 
    {
     

        if (maxDistanceMatrix.length == 0 || appointments.length == 0) {
            return;  // Nothing to process if maxDistance or appointments are empty
        }

        // Iterate over each row in the maximal distance matrix
        for (int i = 0; i < maxDistanceMatrix.length; i++)
        {
            // Iterate over each column in the row
            for (int j = 0; j < maxDistanceMatrix[i].length; j++)
            {

                // Get the maximal distance between the two appointments
                int maximalDistanceInDays = maxDistanceMatrix[i][j];

                // If maximal distance is less than or equal to 0, skip this pair
                if (maximalDistanceInDays <= 0) {
                    continue;
                }

                // Convert maximal distance from days to slots (days * NoSinD slots per day)
                int maximalDistanceInSlots = maximalDistanceInDays * MASPConfigLarge.SLOTS_PER_DAY;

                // Ensure that the maximal distance in slots is enforced
                if (maximalDistanceInSlots % MASPConfigLarge.SLOTS_PER_DAY == 0)// day case
                {

                    int appDuration =  MASPConfigLarge.sequence.get(i + 1).getDurationInSlot();//appointmentDurationsInSlots[i];  // Get the duration of the current appointment

                    
                    // Calculate the end of the first appointment (start time + duration)
                    IntVar endOfFirstAppointment = appointments[0][i].add(appDuration).intVar();

                    // Calculate the constraint to ensure that the second appointment starts within the maximal distance
                    model.arithm(appointments[0][j], "<=", endOfFirstAppointment.add(maximalDistanceInSlots).intVar()).post();

                }
                else
                {
                    //todo hours case
                }
            }
        }
    }

    private void sameResourceconstraint() {

        for (SameResources constraint : MASPConfigLarge.sameResourcesConstraints) {
            int idxA = MASPConfigLarge.getGlobalResourceIndex(constraint.getAppointmentIndex1(), constraint.getResourceIndex1());
            int idxB = MASPConfigLarge.getGlobalResourceIndex(constraint.getAppointmentIndex2(), constraint.getResourceIndex2());
           
            model.arithm(resourceVariables[idxA], "=", resourceVariables[idxB]).post();
           
            //System.out.printf("Constraint: resources[%d] must equal resources[%d]%n", idxA, idxB);
        }


    }

    private IntVar optimizeResourcceWorkload() {

                
        IntVar[] workloadPenalties = new IntVar[resourceVariables.length];

        Map<Integer,IntVar> totalWorkloadperResource = new HashMap<>();

        Map<Integer,IntVar>    totalWorkloadPerResource = new HashMap<>();

        Map<Integer,IntVar>    resourceWorkloadPerResource = new HashMap<>();

            for (int i=0;i<potentialResourceSlot.size();i++) {

            
                int appointmentId = resourceIndex2AppointmentMap.get(i);
                   workloadPenalties[i] = model.intVar("workloadPenalty" + i, 0, 500);

                for (int j = 0; j < potentialResourceSlot.get(i).size(); j++) {

                    int resourceId = potentialResourceSlot.get(i).get(j).getResourceId();
                
                    int initialWorkload = potentialResourceSlot.get(i).get(j).getCurrentWorkload();
                   // System.out.println("Appointment " + (appointmentId+1) + " resource variable " + resourceVariables[i].getName() +
                    // " assigned to resource ID " + resourceId + " with initial workload " + initialWorkload);

                    int thisAppDuration = MASPConfigLarge.sequence.get(appointmentId+1).getDurationInSlot() * 15;//15 minutes per slot

                    totalWorkloadPerResource.putIfAbsent(resourceId, model.intVar(initialWorkload));

                     // Dynamically update workload penalty for this resource
                model.ifThen(
                    model.arithm(resourceVariables[i], "=", resourceId),
                    model.arithm(workloadPenalties[i], "=",
                        model.intOffsetView(totalWorkloadPerResource.get(resourceId), thisAppDuration))
                );

                resourceWorkloadPerResource.put(i, workloadPenalties[i]);

                }
            }

        // Step 2: Aggregate workloads for each resource
        for (int resourceId : totalWorkloadPerResource.keySet()) 
        {
            IntVar cumulativeWorkload = model.intVar("cumulativeWorkload_" + resourceId, 0, 500);
            ArrayList<IntVar> resourceWorkloads = new ArrayList<>();

            for (int i = 0; i < resourceVariables.length; i++)
            {
                IntVar workloadForThisResource = model.intVar("workloadForResource_" + resourceId + "_app" + i, 0, 500);
                model.ifThenElse(
                    model.arithm(resourceVariables[i], "=", resourceId),
                    model.arithm(workloadForThisResource, "=", workloadPenalties[i]),
                    model.arithm(workloadForThisResource, "=", 0)
                );
                resourceWorkloads.add(workloadForThisResource);
            }

            // Sum all workloads for this resource
            model.sum(resourceWorkloads.toArray(new IntVar[0]), "=", cumulativeWorkload).post();

            // Update the cumulative workload in the tracking map
            totalWorkloadPerResource.put(resourceId, cumulativeWorkload);
        }

         // Step 3: Calculate maximum workload
        IntVar maxWorkload = model.intVar("maxWorkload", 0, 500);
        model.max(maxWorkload, totalWorkloadPerResource.values().toArray(new IntVar[0])).post();



        return maxWorkload;
}





    private IntVar optimizeResourcecWorkload() {
    int numResources = MASPConfigLarge.CURRENT_WORKLOAD.length;
    
    // Calculate total duration each resource will work based on assignments
    IntVar[] finalWorkloads = new IntVar[numResources];
    IntVar[] workloadIncreases = new IntVar[numResources];
    
    for (int r = 0; r < numResources; r++) {
        // Start with current workload
        finalWorkloads[r] = model.intVar("final_workload_" + r, 
            MASPConfigLarge.CURRENT_WORKLOAD[r], 
            MASPConfigLarge.CURRENT_WORKLOAD[r] + getMaxPossibleWorkloadIncrease());
        
        // Calculate workload increase for this resource
        workloadIncreases[r] = model.intVar("workload_inc_" + r, 0, getMaxPossibleWorkloadIncrease());
        
        // Sum up all durations where this resource is selected
        List<IntVar> indicators = new ArrayList<>();
        List<Integer> durations = new ArrayList<>();
        
        for (int i = 0; i < resourceVariables.length; i++) {
            int appointmentId = resourceIndex2AppointmentMap.get(i);
            Appointment appointment = MASPConfigLarge.sequence.get(appointmentId + 1);
            int duration = appointment.getDurationInSlot();
            
            // Create indicator variable for whether this resource is selected
            BoolVar indicator = model.boolVar("indicator_r" + r + "_var" + i);
            model.arithm(resourceVariables[i], "=", r).reifyWith(indicator);
            
            indicators.add(indicator);
            durations.add(duration);
        }
        
        // Workload increase = sum of (indicator * duration) for all assignments to this resource
        if (!indicators.isEmpty()) {
            model.scalar(indicators.toArray(new IntVar[0]), 
                        durations.stream().mapToInt(Integer::intValue).toArray(), 
                        "=", workloadIncreases[r]).post();
        } else {
            model.arithm(workloadIncreases[r], "=", 0).post();
        }
        
        // Final workload = current workload + workload increase
        model.arithm(finalWorkloads[r], "=", 
                    model.intVar(MASPConfigLarge.CURRENT_WORKLOAD[r]).add(workloadIncreases[r]).intVar()).post();
    }
    
    // Calculate mean workload
    IntVar totalWorkload = model.intVar("total_workload", 0, 
        Arrays.stream(MASPConfigLarge.CURRENT_WORKLOAD).sum() + getTotalPossibleWorkloadIncrease());
    model.sum(finalWorkloads, "=", totalWorkload).post();
    
    IntVar meanWorkload = model.intVar("mean_workload", 0, 
        Arrays.stream(MASPConfigLarge.CURRENT_WORKLOAD).max().orElse(0) + getMaxPossibleWorkloadIncrease());
    model.arithm(meanWorkload, "=", totalWorkload.div(model.intVar(numResources)).intVar()).post();
    
    // Calculate variance = sum of (workload - mean)^2 for all resources
    IntVar[] squaredDeviations = new IntVar[numResources];
    for (int r = 0; r < numResources; r++) {
        IntVar deviation = model.intVar("dev_" + r, 
            -getMaxPossibleWorkloadIncrease(), 
            getMaxPossibleWorkloadIncrease());
        
        // deviation = finalWorkloads[r] - meanWorkload
        model.arithm(deviation, "=", finalWorkloads[r].sub(meanWorkload).intVar()).post();
        
        // squaredDeviation = deviation * deviation
        squaredDeviations[r] = model.intVar("sq_dev_" + r, 0, 
            (int)Math.pow(getMaxPossibleWorkloadIncrease() * 2, 2));
        model.times(deviation, deviation, squaredDeviations[r]).post();
    }
    
    // Total penalty = sum of squared deviations (variance)
    IntVar totalPenalty = model.intVar("workload_penalty", 0, 
        numResources * (int)Math.pow(getMaxPossibleWorkloadIncrease() * 2, 2));
    model.sum(squaredDeviations, "=", totalPenalty).post();
    
    return totalPenalty;
}

private int getMaxPossibleWorkloadIncrease() {
    // Maximum possible workload increase for a single resource
    // This is the sum of durations of all appointments that could potentially use this resource
    int maxIncrease = 0;
    for (Appointment app : MASPConfigLarge.sequence.values()) {
        maxIncrease += app.getDurationInSlot();
    }
    return maxIncrease;
}

private int getTotalPossibleWorkloadIncrease() {
    // Total possible workload increase across all resources
    int totalIncrease = 0;
    for (Appointment app : MASPConfigLarge.sequence.values()) {
        totalIncrease += app.getDurationInSlot() * app.getRequiredResources().size();
    }
    return totalIncrease;
}





    
}



