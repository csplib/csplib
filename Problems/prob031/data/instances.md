---
Title: Instances for rack Configuration
---

Instances are described by

* nbRackModels: number of rack models
* nbCardTypes: number of card types
* nbRacks: number of racks available
* RackModels: each rack model characterised by the maximal power it can supply, its number of connectors, and its price
* CardTypes: each card type characterised by the power it requires, and a demand designating how many cards of that card type are to be plugged.

the instances:

	/* Instance 1*/ 
	nbRackModels = 2; 
	nbCardTypes = 4; 
	nbRacks = 5; 
	RackModels = [< 150, 8, 150 >,
	< 200, 16, 200 > ]; 
	CardTypes = [ < 20, 10 >,
	< 40, 4 >,
	< 50, 2 >,
	< 75, 1 > ];

	/* Instance 2*/ 
	nbRackModels = 2; 
	nbCardTypes = 4; 
	nbRacks = 10; 
	RackModels = [<150,8,150>,
	<200,16,200> ]; 
	CardTypes = [ < 20, 20 >,
	< 40, 8 >,
	< 50, 4 >,
	< 75, 2 > ];

	/* Instance 3 */ 
	nbRackModels = 2; 
	nbCardTypes = 6; 
	nbRacks = 12; 
	RackModels = [<150,8,150>,
	<200,16,200> ]; 
	CardTypes = [ <10, 20>, 
	<20, 10 >,
	<40, 8 >,
	<50, 4 >,
	<75, 2 >,
	<100,1> ];

	/* Instance 4 */ 
	nbRackModels = 6; 
	nbCardTypes = 6; 
	nbRacks = 9; 
	RackModels = [<50,2,50>,
	<100,4,100>,
	<150,8,150 >,
	<200,16,200 >,
	<250,32,250>,
	<300,64,300> ]; 
	CardTypes = [ < 20, 10 >,
	< 40, 6 >,
	< 50, 4 >,
	< 75, 2 >,
	<100,2>,
	<150,1>];