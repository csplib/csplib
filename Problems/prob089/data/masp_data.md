## MASP Problem Data File

Data files are given in Java format. Each files represents one MASP instance comprising different calendar settings, slots, and patient prefrences. Please note that one needs to give the constructor MedicalAppointmentSchedulingProblem(...) the required resource types as well as the required duration of the appointment (in slots). 

Here I give an example how to instantiate the problem in the main() method:
  
 MedicalAppointmentSchedulingProblem.Resource[] resourcesArray = {Resource.CARDIOLOGY,Resource.NEUROLOGIE};// Two resources are required for teh appointment
 
 int appointmetnDurationInSlot= 8;


 MedicalAppointmentSchedulingProblem masp=new  MedicalAppointmentSchedulingProblem(appointmetnDurationInSlot, resourcesArray);//
  masp.solve();

If you want to chnage calendar settings or patient preferences, or resource domains you can easily do so by amending th econfig file in Java.
           
           
> 📖 For full modeling context, refer to the publication [10.5220/0013091300003890](https://doi.org/10.5220/0013091300003890).
