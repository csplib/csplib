/**
 * @author: Dr. rer. nat. George Assaf (Brandenburg University of Technology (BTU), Cottbus, Germany)
 * @version: 1.0
 * @date: 2025-07-20
 * @description: A prototype implementation of the Medical Appointment Scheduling Problem (MASP) using the Choco solver.
*/

package com.masp;
 
// Java imports data structures
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

// Choco solver imports
import org.chocosolver.solver.Model;
import org.chocosolver.solver.Solver;
import org.chocosolver.solver.constraints.Constraint;
import org.chocosolver.solver.variables.BoolVar;
import org.chocosolver.solver.variables.IntVar;

// Structure encodes the preferred time slots on a given weekday for the patient
class PreferedTime {
    int weekday;//0=Monday, 1=Tuesday, ..., 6=Sunday
    int stratSlotIndex;// start slot index in the global calendar
    int endSlotIndex;// end slot index in the global calendar

    public PreferedTime(int weekday, int stratSlot, int endSlot) {
        this.weekday = weekday;
        this.stratSlotIndex = stratSlot;
        this.endSlotIndex = endSlot;
    }

    //getters and setters


    public int getWeekday() {
        return weekday;
    }

    public int getStratSlotIndex() {
        return stratSlotIndex;
    }

    public int getEndSlotIndex() {
        return endSlotIndex;
    }
}


public class MedicalAppointmentSchedulingProblem {  

    // Enum to represent the types of medical resources, extend as needed
    public enum Resource {
        CARDIOLOGY,
        NEUROLOGIE
    }


    private final Model model; //The Model object

    //Resource domain definitions
    private final int[] CardiologyDomain = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9};// Identifers of resources in the Cardiology domain
    private final int[] NeurologyDomain =  {10, 11, 12, 13, 14, 15, 16, 17, 18, 19};// Identifers of resources in the Neurology domain

    private final int M = 24; // Number of time slots per day

    //Time slot domain definitions
    //Resource calendars, each resource calendar is represented by an integer array of available slot identifers
    //Resource calendar = 9 days x 24 slots 
    //Each calendar starts with Monday (0) at 8:00 -15:00,
    private final int[] resourceCalendars = {
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, //Mo (0)
        24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, //Tu (1)
        48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, //We (2)
        72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
        96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, //Th (3)
        -1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, //Fr (4)
        -25, -26, -27, -28, -29, -30, -31, -32, -33, -34, -35, -36, -37, -38, -39, -40, -41, -42, -43, -44, -45, -46, -47, -48, //Sa (5)
        168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, //Su (6)
        192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215 //Mo (0)
    };

    // Global calendar, which is a combination of all resource calendars, and determines the facility availability
    private final int[] globalCalendars = {
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, //Mo (0)
        24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, //Tu (1)
        48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, //We (2)
        72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95,
        96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, //Th (3)
        120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, //Fr (4)
        -145,-146, -147, -148, -149, -150, -151, -152, -153, -154, -155, -156, -157, -158, -159, -160, -161, -162, -163, -164, -165, -166, -167,  //Sa (5)
        168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, //Su (6)
        192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215 //Mo (0)
    };

    //model parameters
    private final int[] preferredDatesByPatient = {7,8}; //Identifiers of preferred doctors (0=Cardiology, 1=Neurology)
    //  Add undesired weekdays for the patient (example: exclude Monday (0) and Friday (4))
    private final int[] undesiredWeekDaysByPatient = {0, 4};
    //Identifiers of preferred doctors for each required resource
    private final Map<Integer, List<Integer>> preferredDoctorsByPatient = Map.of(
    0, List.of(4,8),// means that for the required cardiology specialization, it would be desired to have either specialist 4 or 8
    1, List.of(13,17)
    );
    List<PreferedTime> preferredTimesByPatient = new ArrayList<>(Arrays.asList(
        new PreferedTime(0, 3, 6) // Preferred time slots for Monday, 8:00-9:00
        ,new PreferedTime(1, 6, 9) // Preferred time slots for Tuesday, 9:00-10:00
       // new PreferedTime(2, 6, 9) // Preferred time slots for Wednesday, 10:00-11:00
    ));


    private int durationInSlots;//Appointment duration in slot
    private Resource[] resourcesArray;// Required resource types for the appointment 

    //decion variables
    private IntVar[] appSlots; // each deciion variable encodes one slot in the schedule
    private IntVar[] appResources; //resources to be assigned
    private IntVar[] auxVarCalendars; //  each variabel encodes one resource calendar
     


    
    
     // Constructor for the Medical Appointment Scheduling Problem (MASP).
     // Initializes the model, decision variables, and constraints based on the provided duration and resources.
     // @param duration The  duration of the scheduling problem in time slots, e.g. 4 means 1 hour appointment.
     // @param resourcesArray An array of resources (e.g., CARDIOLOGY, NEUROLOGIE) to be scheduled.
     //
    public MedicalAppointmentSchedulingProblem(int duration, Resource[] resourcesArray) {

        this.model = new Model("MASP Scheduling Problem");
  
        this.durationInSlots = duration;

        this.resourcesArray = resourcesArray;

        //declaer decision variables and assign domains
        createVariables();

        //create auxiliary variables
        createAuxiliaryVariables();

        //define constraints (hard and soft)
        MASPConstraints();

        //optimize soft constraints
        optimizeSoftConstraints();

    }



     // Creates decision variables for the appointment slots and resources.
     // Each appointment slot is represented as an integer variable over the global calendar domain.
     // Resources are assigned to each appointment based on their respective domains.
     //
    private void createVariables() {
        //  Create decision variables for appointment slots over global calendar domain 
        appSlots = model.intVarArray("appSlots", durationInSlots, globalCalendars);

        this.appResources = new IntVar[resourcesArray.length];

        // Create decision variables for resources assigned to each appointment 
        for(int i=0;i<resourcesArray.length;i++) {
            // Validate resource type
            Resource resource = resourcesArray[i];

            if (resource == null) {

                throw new IllegalArgumentException("Resource cannot be null");
            }
            if (resource != Resource.CARDIOLOGY && resource != Resource.NEUROLOGIE) {

                throw new IllegalArgumentException("Resource must be either CARDIOLOGY or NEUROLOGIE");
            }

            if(resourcesArray[i]==Resource.CARDIOLOGY) {// Cardiology medical specialization //

                this.appResources[i] = model.intVar("appResource_" + resource, CardiologyDomain);

            }
            else if(resourcesArray[i]==Resource.NEUROLOGIE) {// Neurology medical specialization //

                this.appResources[i] = model.intVar("appResource_" + resource, NeurologyDomain);

            } else {

                throw new IllegalArgumentException("Unknown resource type: " + resource);
            }
 
        }
    
    }

    
    // Creates auxiliary variables for resource calendars.
    // Each auxiliary variable represents the first time slot for the appointment assigned to a specific resource.
    // Each variable is declared over the corrsponding resourceCalendar domain.
    //
    private void createAuxiliaryVariables() {

        // Create auxiliary variables for resource calendars, each nominates the first time slot for the appointment
        this.auxVarCalendars = model.intVarArray("auxVarCalendars", CardiologyDomain.length + NeurologyDomain.length, resourceCalendars);
    }
    

     // Defines the constraints for the appointment scheduling problem.
     // This method includes hard constraints to ensure valid scheduling and soft constraints to optimize patient preferences.
     //
    private void MASPConstraints() {

        //Eliminate slots that fit with the undesired dates for the patient
        eleminiatePatientUndesiredDates();

        // Link appSlots to auxVarCalendars based on the resources assigned
        linkResoucesToAppointmentFirstSlotConstraint();


        //appSlots are not allowed to be negative
        for (IntVar slot : appSlots) {
            model.arithm(slot, ">=", 0).post();
        }

        //app slots must be different
        model.allDifferent(appSlots).post();

        //app slots mmust be sequential
        for (int i = 0; i < appSlots.length - 1; i++) {
            model.arithm(appSlots[i + 1], "=", appSlots[i], "+", 1).post();
        }

        //app resources must be different
        model.allDifferent(appResources).post();


        for (IntVar auxVar : auxVarCalendars) {
            // Ensure that auxiliary variables are not negative
            model.arithm(auxVar, ">=", 0).post();
        }


    }

    
     // Optimizes soft constraints
     // This method calculates the total violation count for soft constraints and then posts it as an objective to minimize.
     //
    private void optimizeSoftConstraints() {
        // Soft constraint: Optimize preferred dates for the patient
        IntVar optimizeDateSoft = calaculatePreferredDatesViolation();

        IntVar preferredDoctorsSoft = setPreferedDoctorsByPatient();


        IntVar optimizePreferredTimesSoft = optimizePreferredtimes1();// optimizePreferredtimes();

        IntVar optimizeSoftConstraints= model.intVar("Optimize Soft Constraints", 0, optimizeDateSoft.getUB() + preferredDoctorsSoft.getUB()+ optimizePreferredTimesSoft.getUB());

        model.sum(new IntVar[]{optimizeDateSoft, preferredDoctorsSoft, optimizePreferredTimesSoft}, "=", optimizeSoftConstraints).post();
        
        model.setObjective(Model.MINIMIZE, optimizeSoftConstraints);
    
    }



    
     // Links resources to the first slot of the appointment.
     // If a resource is assigned, it will determine the first time slot for the appointment.
     // @return void
    private void linkResoucesToAppointmentFirstSlotConstraint() {


        for (int i = 0; i < appResources.length; i++) {
            Resource resource = resourcesArray[i];
            //IntVar resourceVar = appResources[i];

            if (resource == Resource.CARDIOLOGY) {
                for(int j= 0; j < CardiologyDomain.length; j++) {
                    model.ifThen(model.arithm(appResources[i], "=", CardiologyDomain[j]),
                    model.arithm(appSlots[0], "=", auxVarCalendars[j]));
                }
            } else if (resource == Resource.NEUROLOGIE) {
                for(int j= 0; j < NeurologyDomain.length; j++) {
                    model.ifThen(model.arithm(appResources[i], "=", NeurologyDomain[j]),
                    model.arithm(appSlots[0], "=", auxVarCalendars[j+ CardiologyDomain.length]));
                }
            }          
        }
    
    }



    
    //Soft constraint: Calculates the violation of preferred dates for the patient.
    // This method creates a list of boolean variables representing whether each preferred date is violated by aeach potential resouce, i.e. Physican.
    // @return An IntVar variable representing the total number of violations across all preferred dates by all potential resources.
    private IntVar calaculatePreferredDatesViolation() {
       
        List<BoolVar> violationVars= new ArrayList<>();
         
        //init viloation variabel per patient preferred date
        for(int i=0;i<CardiologyDomain.length+NeurologyDomain.length;i++) {
            BoolVar prefDateViolation = model.boolVar("prefDateViolation_" + i, false);
            violationVars.add(prefDateViolation);
        }


        for (int i = 0; i < appResources.length; i++) {
            Resource resource = resourcesArray[i];

        if (resource == Resource.CARDIOLOGY) {
                for(int j= 0; j < CardiologyDomain.length; j++) {

                    //calculaet violation status for the thsi resource
                    BoolVar prefDateViolation = setPreferredDatesByPatient(auxVarCalendars[j]);

                    //add the violation variable to the list, if the resource is assigned to the appointment
                    model.ifThen(model.arithm(appResources[i], "=", CardiologyDomain[j]),
                    model.arithm(violationVars.get(j), "=", prefDateViolation));
                }
            } else if (resource == Resource.NEUROLOGIE) {
                for(int j= 0; j < NeurologyDomain.length; j++) {
                     BoolVar prefDateViolation = setPreferredDatesByPatient(auxVarCalendars[j+ CardiologyDomain.length]);
                    model.ifThen(model.arithm(appResources[i], "=", NeurologyDomain[j]),
                    model.arithm(violationVars.get(j), "=", prefDateViolation));
                }
            }          
        }

     

        // Combine all violations into a single variable
        IntVar optimizeDateSoft = model.intVar("Optimize Date Soft", 0, violationVars.size());

        // Convert list to array and sum up all violations
         model.sum(violationVars.toArray(new BoolVar[0]), "=", optimizeDateSoft).post();

        return optimizeDateSoft;
    }

     // Soft constraint: Sets preferred dates for the patient.
     // This method posts constraints to ensure that the appointment slots fall on preferred dates if possible.
     // @param resourceCalendaar The calendar variable representing the resource's availability.
     // @return A boolean variable indicating whether the preferred date constraint is satisfied.
    private BoolVar setPreferredDatesByPatient(IntVar resourceCalendaar) {

        BoolVar prefDateViolationcheckVar= model.boolVar("prefDateViolation", false);

        
        BoolVar[] dateConstraints = new BoolVar[preferredDatesByPatient.length];
         

        for(int prefDateInd = 0; prefDateInd < preferredDatesByPatient.length; prefDateInd++) {

            dateConstraints[prefDateInd] = model.arithm(resourceCalendaar.div(M).intVar(), "=",preferredDatesByPatient[prefDateInd] ).reify();

        }
         

        prefDateViolationcheckVar = model.not(model.or(dateConstraints)).reify();

        return prefDateViolationcheckVar;    
    }


  

    
     // Eliminates undesired dates for  patient.
     // This method posts constraints to ensure that the appointment slots do not fall on undesired weekdays.
    private void eleminiatePatientUndesiredDates() {
   
        for(int badDateInd = 0; badDateInd < undesiredWeekDaysByPatient.length; badDateInd++) {

        for(IntVar calendar: auxVarCalendars) {

           model.arithm(calendar.div(M).intVar(), "!=", undesiredWeekDaysByPatient[badDateInd]).post();
        }
      }
    }
    


    
     // Soft constraint: Sets preferred doctors chosen by the patient.
     // For each preferred doctor, this method checks if the assigned resource matches the doctor's identifier, and tracks violations per prefered doctor.
     // @return An IntVar variable representing the total number of violations across all preferred doctors to be optimized.
    private IntVar setPreferedDoctorsByPatient() {

        int[] preferredDoctorViolations = new int[appResources.length];
        
        BoolVar[][] allResourcesCheck  = model.boolVarMatrix(appResources.length, preferredDoctorsByPatient.size());


        BoolVar[] b_preferredSpecialIsBroken = model.boolVarArray(appResources.length);//to record total violation occurance per required resource

        int upperBoundPrefferedResources = 0;// simple counter to track the number of preferred resources

         

        for (int i =0; i< appResources.length; i++) {
            Resource resource = resourcesArray[i];
            List<Integer> preferredDoctors = preferredDoctorsByPatient.get(i);
       
            if (preferredDoctors != null) {
                // Reification of resource assignment
                for (int j = 0; j < preferredDoctors.size(); j++) {
                    int doctorId = preferredDoctors.get(j);
                    if (resource == Resource.CARDIOLOGY) {
                        allResourcesCheck[i][j] = model.arithm(appResources[i], "=", doctorId).reify();
                        preferredDoctorViolations[i] ++;
                        upperBoundPrefferedResources++;
                    } else if (resource == Resource.NEUROLOGIE) {
                        allResourcesCheck[i][j] = model.arithm(appResources[i], "=", doctorId).reify();
                        preferredDoctorViolations[i] ++;
                        upperBoundPrefferedResources++;


                    }
                }
            }
        }

        // store violation check variables
        BoolVar[] isPreferedDoctor = new BoolVar[appResources.length ];


        for(int i=0;i<allResourcesCheck.length;i++) {
           if( preferredDoctorViolations[i] > 0) {
               isPreferedDoctor[i] = model.or(Arrays.copyOf(allResourcesCheck[i], preferredDoctorViolations[i])).reify();
            } else {
                isPreferedDoctor[i] = model.boolVar("isPreferedDoctor_AppResource" + i, true);//no violation occcurred
            }
        }
        
        for (int i = 0; i < isPreferedDoctor.length; i++) {
            model.ifThenElse(
                model.arithm(isPreferedDoctor[i], "=", 0),
                model.arithm(b_preferredSpecialIsBroken[i], "=", 1),
                model.arithm(b_preferredSpecialIsBroken[i], "=", 0));
        }

        // Store sum up all violations for preferred resources
        IntVar sumViolations = model.intVar("total violations for prefered resolureces", 0, upperBoundPrefferedResources);

        // post the constraint to sum up all violations
        model.sum(b_preferredSpecialIsBroken, "=", sumViolations).post();

        return sumViolations;

    }


     // Soft constraint: Sets preferred times for the patient.
     // This method checks if the appointment slots fall within the patient's preferred time ranges and returns violations.
     // @param auxCalendarVar The auxiliary calendar variable representing the resource's availability.
     // @return An array of boolean variables indicating whether each preferred time constraint is violated.
    private BoolVar[] setPrefferedtimes1(IntVar auxCalendarVar) {
    // Check if there are any preferred times defined for the patient
    if (preferredTimesByPatient == null || preferredTimesByPatient.isEmpty()) {
        return new BoolVar[0];
    }

    BoolVar[] violations = new BoolVar[preferredTimesByPatient.size()];

    IntVar dayVar = auxCalendarVar.div(M).mod(7).intVar();//day = floor(auxVar div M) mod 7

    IntVar slotVar = auxCalendarVar.mod(M).intVar(); //slotIndex = auxVar mod M

    for (int i = 0; i < preferredTimesByPatient.size(); i++) {

        PreferedTime pref = preferredTimesByPatient.get(i);
        
        // Create constraints (not yet reified)
        Constraint matchesDayConstraint = model.arithm(dayVar, "=", pref.getWeekday());
        Constraint afterStartConstraint = model.arithm(slotVar, ">=", pref.getStratSlotIndex());
        Constraint beforeEndConstraint = model.arithm(slotVar, "<=", pref.getEndSlotIndex());
        
        // Create the "not within time range" constraint
        Constraint withinTimeRangeConstraint = model.and(afterStartConstraint, beforeEndConstraint);
        Constraint notWithinTimeRangeConstraint = model.not(withinTimeRangeConstraint);
        
        // Combine into final constraint: matches day AND not within time range
        Constraint violationConstraint = model.and(matchesDayConstraint, notWithinTimeRangeConstraint);
        
        // Reify the final constraint to get a BoolVar
        violations[i] = violationConstraint.reify();
    }

    return violations;
}

 // Soft constraint: Optimizes preferred times for the patient.
 // This method aggregates violations across all resources and returns a single IntVar representing the total number of violations.
 // @return An IntVar variable representing the total number of violations across all preferred times.
private IntVar optimizePreferredtimes1() {

    if (preferredTimesByPatient == null || preferredTimesByPatient.isEmpty()) {

        return model.intVar(0);
    }

    List<BoolVar> allViolations = new ArrayList<>();

    for (int i = 0; i < appResources.length; i++) {

        Resource resource = resourcesArray[i];

        int domainOffset = (resource == Resource.CARDIOLOGY) ? 0 : CardiologyDomain.length;

        int domainSize = (resource == Resource.CARDIOLOGY) ? CardiologyDomain.length : NeurologyDomain.length;

        for (int j = 0; j < domainSize; j++) {
            BoolVar[] resourceViolations = setPrefferedtimes1(auxVarCalendars[domainOffset + j]);
            int resourceId = (resource == Resource.CARDIOLOGY) ? CardiologyDomain[j] : NeurologyDomain[j];
            
            // Create resource selection constraint (not reified yet)
            Constraint resourceSelectedConstraint = model.arithm(appResources[i], "=", resourceId);
            
            for (BoolVar violation : resourceViolations) {
                // The violation is already a BoolVar (reified)
                // Create a new BoolVar that is true iff both resourceSelectedConstraint and violation are true
                BoolVar combinedViolation = model.boolVar();
                // If the resource is selected, then combinedViolation == violation; otherwise, combinedViolation == 0
                model.ifThenElse(
                    resourceSelectedConstraint,
                    model.arithm(combinedViolation, "=", violation),
                    model.arithm(combinedViolation, "=", 0)
                );
                allViolations.add(combinedViolation);
            }
        }
    }

    if (allViolations.isEmpty()) {
        return model.intVar(0);
    }

    IntVar totalViolations = model.intVar("totalTimeViolations", 0, allViolations.size());

    model.sum(allViolations.toArray(new BoolVar[0]), "=", totalViolations).post();

    return totalViolations;
}

     // Solves the Medical Appointment Scheduling Problem (MASP).
     // This method initializes the solver, sets a limit on the number of solutions, and iterates through the solutions,
    // printing the values of appointment slots and resources for each solution found.
    public void solve() {

    Solver solver = model.getSolver();


    while (solver.solve()) {


        System.out.println("Solution found:");
            System.out.println("Appointment Slots:");

            // Print the values of appointment slots (decison variables )
            for (IntVar slot : appSlots) {
                System.out.print(slot.getName() + "=" + slot.getValue() + " ");
                }

            for (IntVar resource : appResources) {
                System.out.print(resource.getName() + "=" + resource.getValue() + " ");
            }
  
  
            System.out.println();

      
    }

    solver.printStatistics(); // Print solver statistics 

    }
     
}
