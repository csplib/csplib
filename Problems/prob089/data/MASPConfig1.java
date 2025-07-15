package com.example;


import java.util.List;
import java.util.Map;

public class MASPConfig1 {
    /* Resource domain definitions */
    public static final int[] CARDIOLOGY_DOMAIN = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12}; // Identifiers of resources in the Cardiology domain
    public static final int[] NEUROLOGY_DOMAIN = {14, 15, 16, 17, 18, 19}; // Identifiers of resources in the Neurology domain

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

    /* Patient preferences */
    public static final int[] PREFERRED_DATES = {3, 8}; /* Identifiers of preferred doctors (0=Cardiology, 1=Neurology) */
    public static final int[] UNDESIRED_WEEKDAYS = {1}; /* Add undesired weekdays for the patient (example: exclude Monday (0) and Friday (4)) */
    
    /* Identifiers of preferred doctors for each required resource */
    public static final Map<Integer, List<Integer>> PREFERRED_DOCTORS = Map.of(
        0, List.of(1, 4), // means that for the required cardiology specialization, it would be desired to have either specialist 4 or 8
        1, List.of(14, 17)
    );
    
    public static final List<PreferedTime> PREFERRED_TIMES = List.of(
        new PreferedTime(3, 0, 7) // Preferred time slots for Monday, 8:00-9:00
    );
}