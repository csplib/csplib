/**
 * @author: Dr. rer. nat. George Assaf (Brandenburg University of Technology (BTU), Cottbus, Germany)
 * @version: 1.0
 * @date: 2025-10-26
 * @description: Encoding of resource sameness across different appointments.
*/


package com.example.massp;

public class SameResources {

    int appointmentIndex1;
    int resourceIndex1;
    int appointmentIndex2;
    int resourceIndex2;

    public SameResources( int appointmentIndex1, int resourceIndex1, int appointmentIndex2, int resourceIndex2) {
        this.appointmentIndex1 = appointmentIndex1;
        this.resourceIndex1 = resourceIndex1;
        this.appointmentIndex2 = appointmentIndex2;
        this.resourceIndex2 = resourceIndex2;
    }

    public int getAppointmentIndex1() {
        return appointmentIndex1;
    }
    public int getResourceIndex1() {
        return resourceIndex1;
    }
    public int getAppointmentIndex2() {
        return appointmentIndex2;
    }
    public int getResourceIndex2() {
        return resourceIndex2;
    }
    
}
