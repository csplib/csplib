package com.example.massp;

import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.Arrays;

import com.example.massp.Appointment;
import com.example.massp.ResourceType;


public class MASPConfigMedium {


    /* Resource domain  (IDs) definitions */
    public static final int[] CARDIOLOGY_DOMAIN = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}; // Identifiers of resources in the Cardiology domain
    public static final int[] NEUROLOGY_DOMAIN = {10, 11, 12, 13, 14, 15, 16, 17, 18, 19}; // Identifiers of resources in the Neurology domain
    public static final int[] CURRENT_WORKLOAD = {20, 10, 5, 10, 40, 30, 20, 10, 10, 50, 70, 80, 10, 15, 20, 25, 15, 60, 70, 60}; //current workload of resources
   
    public static final int SLOTS_PER_DAY = 24; // Number of time slots per day
    
    /* Resource calendars, each resource calendar is represented by an integer array of available slot identifiers */
    /* Resource calendar = 9 days x 24 slots */
    /* Each calendar starts with Monday (0) at 8:00 -15:00 */
    public static final int[] RESOURCE_CALENDARS = {
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, // Mo (0) 07.07.2025
        24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, // Tu (1) 08.07.2025
        48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, // We (2) 09.07.2025
        72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, // Th (3) 10.07.2025
        96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, // Fr (4) 11.07.2025
        -1, -2, -3, -4, -5, -6, -7, -8, -9, -10, -11, -12, -13, -14, -15, -16, -17, -18, -19, -20, -21, -22, -23, -24, // Sa (5) 12.07.2025
        -25, -26, -27, -28, -29, -30, -31, -32, -33, -34, -35, -36, -37, -38, -39, -40, -41, -42, -43, -44, -45, -46, -47, -48, // Su (6) 13.07.2025
        168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, // Mo (7) 14.07.2025
        192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215 // Tu (8) 15.07.2025
    };

    // Global calendar, which is a combination of all resource calendars, and determines the facility availability
    public static final int[] GLOBAL_CALENDAR = {
        0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, // Mo (0) 07.07.2025
        24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, // Tu (1) 08.07.2025
        48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, // We (2) 09.07.2025
        72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, // Th (3) 10.07.2025
        96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, // Fr (4) 11.07.2025
        120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, // Sa (5) 12.07.2025
        -145,-146, -147, -148, -149, -150, -151, -152, -153, -154, -155, -156, -157, -158, -159, -160, -161, -162, -163, -164, -165, -166, -167,  // Su (6) 13.07.2025
        168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, // Mo (7) 14.07.2025
        192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215 // Tu (8) 15.07.2025
    };

 // Sequence of predefined appointments
    public static final Map<Integer, Appointment> sequence = Map.of(
        1, new Appointment(1, List.of(ResourceType.CARDIOLOGY), 2),
        2, new Appointment(2, List.of(ResourceType.NEUROLOGY), 2),
        3, new Appointment(3, List.of(ResourceType.CARDIOLOGY), 4)
    );

    public static final List<Integer> order = List.of(1,2,3); // Order of appointments in the sequence by their IDs
    public static final ArrayList<Integer> chronologicalOrder = new ArrayList<>(order);

    //ResourceType[] resourceSimilarity = {ResourceType.CARDIOLOGY, null, ResourceType.CARDIOLOGY, null};

    public static final List<SameResources> sameResourcesConstraints = List.of(
        new SameResources(1, 1, 3, 1) // Appointment 1's first resource and Appointment 3's first resource must be the same
    );


    // min_distance[i][j] = minimum gap (slots) between appointment i and j
    // -1 means no constraint
    public static final int[][] min_distance = {
        {-1,  -1, -1},  // distances from appointment 1
        {-1, -1,  -1},  // distances from appointment 2
        {-1, -1, -1}   // distances from appointment 3
    };

    // max_distance[i][j] = maximum allowed gap (slots) between appointment i and j
    // 0 means no upper bound
    public static final int[][] max_distance = {
        { 0,  5,  0},  
        { 0,  0, 4},  
        { 0,  0,  0}   
    };

    public static final int[] patient_absence_dates={0,4};// patient is absent on Monday (0) and Friday (4)


    public static final Map<Integer, List<Integer>> sameResource = Map.of(
        1, List.of(0, -1),
        2, List.of(-1, -1),
        3, List.of(0, -1)
    );


   // Find the appointment with the longest list of required resources
    public static Appointment getAppointmentWithMostResources() {
        return sequence.values().stream()
                .max((a1, a2) -> Integer.compare(
                        a1.getRequiredResources().size(),
                        a2.getRequiredResources().size()))
                .orElse(null); // return null if sequence is empty
    }

    public static int getTotalNumberOfResources() {
        return sequence.values().stream()
                .flatMap(appointment -> appointment.getRequiredResources().stream())
                .mapToInt(resourceType -> 1)
                .sum();
    }

    public static int getMaxAppointmentDuration() {
    return sequence.values().stream()
            .mapToInt(Appointment::getDurationInSlot)
            .max()
            .orElse(0); // return 0 if no appointments
}



    // Create a filtered version of GLOBAL_CALENDAR without negative values
    public static int[] getFilteredGlobalCalendar() {
        return Arrays.stream(GLOBAL_CALENDAR)
                    .filter(value -> value >= 0)
                    .toArray();
    }


    // Create a filtered version of RESOURCE_CALENDARS without negative values
    public static int[] getFilteredResourceCalendar() {
        return Arrays.stream(RESOURCE_CALENDARS)
                    .filter(value -> value >= 0)
                    .toArray();
    }

    public static int getGlobalResourceIndex(int appId, int rsourcePositionInAppointment){

        int index =0;

        for (int i=0;i<sequence.size();i++){
            if(sequence.get(i+1).getId()==appId){
                return index + (rsourcePositionInAppointment-1);
            }
        
            index += sequence.get(i+1).getRequiredResources().size();
        }
        
        throw new IllegalArgumentException("Appointment " + appId + " not found");

     }

}