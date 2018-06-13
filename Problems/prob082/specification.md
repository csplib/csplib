---
Title:    Patient Transportation Problem
Proposer: Charles Thomas
Category: Scheduling and related problems
---


The problem consists in transporting patients to medical appointments given a fleet of vehicles. The main objective is to satisfy as much patient requests as possible. Additional objectives such as minimizing the waiting time or maximum ride time of the patients or towards minimizing costs could be considered.

Each request consists of a forward travel where the patient must be brought to their destination on time for their appointment, a backward travel that must be done after the end of the appointment or both. For back and forth requests, both travels must be done for the request to be satisfied. Patients are associated with categories restricting the vehicles that can take them and might be accompanied. Patients might also require some amount of time to embark/disembark vehicles.
The vehicles have a limited capacity which must never be exceeded and are available at fixed times.

The data is provided as JSON files having the following format:

* The fields "version", "id" and "name" correspond respectfully to the version of the PTP instance encoding, the id and the name of the instance.
* The "coordType" field indicates if the coordinates are geographical ("Geo") or euclidian ("Eucl").
* The "sameVehicleBackWard" field indicates if the same vehicle has to be used for the two trips of a back and forth request.
* The "maxWaitTime" field contains the duration of the time windows during which the patient has to be transported. It is expressed as a string having the following format: "HHhMM"
* The "places" field contains all the distinct locations of the problem. For each of them:
    * "id" is the id of the location. It corresponds to its position in the list of locations.
    * "lat" and "long" are the coordinates of the location.
    * "category" is the category of the location. Its value can be 0 for a medical center, 1 for a vehicle depot and 2 for a patient location.
* The "vehicles" field contains all the vehicles available in the fleet. Each of them has the following fields:
	* "id" which contains the id of the vehicle.
	* "canTake" which is a set of categories of patients that the vehicle can take.
	* "start" and "end" which are the ids of the starting and ending depots of the vehicle. A -1 value indicates that the vehicle has no depot.
	* "capacity" which is the capacity of the vehicle.
	* "availability" which is a set of time windows when the vehicle is available. Each time window is encoded by a string having the following format: "HHhMM:HHhMM".
* The "patients" field contains all the requests. Each patient/request has the following fields:
	* "id" which contains the id of the request.
	* "category" which indicates the category of the patient
	* "load" which indicates the number of places required in the vehicle during the transport.
	* "start", "destination" and "end" which indicate the ids of respectfully the starting location, medical center and return location for the patient. "start" or "end" can have a -1 value in case of single trip.
	* "rdvTime" and "rdvDuration" indicate the start of the appointment and its duration as strings under the format "HHhMM". In terms of constraints, the patient must be picked up at its starting location for its forward trip at or after rdvTime - maxWaitTime. They must be dropped at their destination before or at rdvTime. They must be picked up for their backward trip at the medical center after or at rdvTime + rdvDuration and must be dropped at their end location before or at rdvTime + rdvDuration + maxWaitTime.
	* "srvDuration" indicates the time needed for the patient to embark/disembark the vehicle. It is encoded as a string under the format "HHhMM".
* The "distMatrix" field contains the distance (in minutes) matrix between the locations. it follows the ids and order of the locations (distMatrix[2][3] is the distance from location 2 to location 3).

Instances are separated into three sets by difficulty.