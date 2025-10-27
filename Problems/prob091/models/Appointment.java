/**
 * @author: Dr. rer. nat. George Assaf (Brandenburg University of Technology (BTU), Cottbus, Germany)
 * @version: 1.0
 * @date: 2025-10-26
 * @description: Appointment structure per sequence
*/


package com.example.massp;


import java.util.List;

import com.example.massp.ResourceType;

public class Appointment {
    private int id;
    private List<ResourceType> requiredResources;
    private int durationInSlot;


    public Appointment(int id, List<ResourceType> requiredResources, int durationInSlot) {
        this.id = id;
        this.requiredResources = requiredResources;
        this.durationInSlot = durationInSlot;
    }

    public int getId() {
        return id;
    }

    public List<ResourceType> getRequiredResources() {
        return requiredResources;
    }

    public int getDurationInSlot() {
        return durationInSlot;
    }



}
