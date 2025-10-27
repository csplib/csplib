## MASP Problem Data File

Data files are provided in Java format, with each file representing a distinct MASP instance. Instances are categorized by size:

- Small: Limited resources and shorter calendar duration

- Medium: Moderate resource count and calendar length

- Large: Extensive resources and extended calendar scope

Please change the configuration file in the model file (MASSP.java) to use one of these instnaces. 

Appointment Configuration
Modify the sequence map in the configuration file to adjust:

- Number of resources required per appointment

- Duration of each appointment (in time slots)

Temporal Constraints
- Update the min_distance structure to set minimum required gaps between appointments

- Update the max_distance structure to set maximum allowed gaps between appointments


