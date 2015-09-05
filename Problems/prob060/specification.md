---
Title:    Ridesharing
Proposer: Vincent Armant
Category: Scheduling and related problems
---



The aim is to match riders to drivers, satisfying car capacities and individual trip constraints, while maximizing the total number of rider participants. A driver's trip offer is specified as a route from start to finish, a time window of earliest departure and latest arrival, and the number of available seats for passengers. A rider's trip request specifies a start location and destination and a time window. Some drivers (called shifters) are flexible, and are willing to be selected as riders. Shifters specify both a trip offer and a trip request. We assume a driver will definitely drive, regardless of whether or not match is made; a shifter will definitely travel, and thus will drive if not selected as a passenger and no rider is matched. Each driver that is allocated one or more riders must be given a departure time and an arrival time at each point on the route, such that the time gap between any pair of locations is not less than the known travel times. For a rider to be assigned to a driver, the rider's start and finish locations must be on the drivers route in the right order, and the driver's times must satisfy the rider's time window. Multiple riders can be allocated to the same driver, as long as the number of passengers travelling between any pair of locations on the route does not exceed the available number of seats. If a shifter is assigned as a passenger for another driver, then the shifter doesnot drive, and no rider can be assigned to the shifter. Similarly, if a rider is assigned as a passenger on the shifter's trip, then the shifter cannot be assigned as a passenger. A driver is served when at least one rider is assigned to the driver's trip; a rider is served when he or she is assigned to a trip. A solution to the ride sharing problem is then an assignment of a clear role to shifters (driver or rider) and a matching of drivers to riders that satisfies the above constraints.


* For each instance of ridesharing problem the following data file is required.
* The Map 
* "source" "destination" "distance" "time" "cost"
* The drivers trip offers 
* "userId" "departure" "arrival" "earliest starting time" "latest arrival time" "car capacity"
* The riders trip requests
* "userid" "departure" "arrival" "earliest starting time" "latest arrival time" "reward"



Example of map
<pre>
1 2 10
2 3 10
3 4 10
4 5 10
5 6 10
7 8 10
8 4 10
5 9 10
9 10 10
</pre>

Example of driver trip offers:
<pre>
1 1 6 0 70 2
2 7 10 10 80 2
3 8 9 20 40 2
</pre>

Example of rider trip requests:
<pre>
4 2 3 20 30 1
5 4 5 40 60 1
3 8 9 20 40 3
</pre>

