/**
 * @author: Dr. rer. nat. George Assaf (Brandenburg University of Technology (BTU), Cottbus, Germany)
 * @version: 1.0
 * @date: 2025-10-26
 * @description: Helper class dealing with pat-ent related constraints, such as patient availability and later patient prefrences.
*/

package com.example.massp;

import org.chocosolver.solver.Model;
import org.chocosolver.solver.variables.IntVar;

class CalendarDomain{
    private int resourceId;
    private int[] globalCalendar;
    private int[] resourceCalendar;
    private int   currentWorkload;
    private int   appointmentDurationInSlot;
    private IntVar calendarVar;

    public CalendarDomain(int resourceId, int[] globalCalendar, int[] resourceCalendar, int appointmentDurationInSlot, int currentWorkload) {
        this.resourceId = resourceId;
        this.globalCalendar = globalCalendar;
        this.resourceCalendar = resourceCalendar;
        this.appointmentDurationInSlot = appointmentDurationInSlot;
        this.currentWorkload = currentWorkload;
    }

    public void findDate(Model model) {

        calendarVar = model.intVar("appointmentSlotVar doctor" + resourceId, resourceCalendar);

        elemenatePatientsAbsenceDates(model, MASPConfig.patient_absence_dates);

        //return calendarVar;
    }

    public int getResourceId() {
        return resourceId;
    }

    public int getCurrentWorkload() {
        return currentWorkload;
    }


    public IntVar getCalendarVar() {
        return calendarVar;
    }


    private void elemenatePatientsAbsenceDates(Model model, int[] patientAbsenceDates) {
        if (patientAbsenceDates == null || patientAbsenceDates.length == 0) {
            return; // No absence dates to process
        }

        for (int absenceDate : patientAbsenceDates) {
            

            if(absenceDate>=0)
            {

             model.arithm(calendarVar.div(MASPConfig.SLOTS_PER_DAY).intVar(), "!=", absenceDate).post();
        
            }
        }
    }


}