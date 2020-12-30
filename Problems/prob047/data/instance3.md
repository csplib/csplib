---
Title: (7 agents, 3 tiers)
---

#### Note on instance description


This instance is based on a supply chain topology referred to as the 'W' system, which is composed of 2 final products produced from 3 components. Each product is assembled from a common and a unique component. This system has the characteristics of pure distribution (common component is used in both products) and pure assembly structure. W system is well-known in the assemble-to-order inventory literature.

The instance description consists of two parts:

1.  Topology: the supply chain network topology consists of (i) a list of agents; (ii) a list of products that each agent produces (*agent-name=product-name(s)*); and (iii) a bill of materials specifying what products are used in building other products (*product-name=component-name(s)*).
2.  Local problems: the private data describing each agent's local problem - this is in the form of an OPL data file and is for use in conjunction with the OPL models provided in this benchmark description.

#### Topology

    Agents: A1,A2,A3,A4,A5,A6,A7

    Products: 
    A1=P0,P1
    A2=P2
    A3=P3
    A4=P4
    A5=P5
    A6=P6
    A7=P7

    BOM:
    P0=P2,P3
    P1=P3,P4
    P2=P5
    P3=P6
    P4=P7

#### Agent: A1

    numPeriods = 12;
    numProducts = 2;
    numComponents = 3;
    maxNumComponentOrders = [30,30,30];
    bom = [
    [1,1,0],
    [0,1,1]
    ];
    componentBatchSize = [77,53,91];
    deliveryCost = [2387,2332,4459];
    leadtime = [0,2];
    cycles = [2,1];
    setupCycles = [7,10];
    capacity = [133,113,136,187,119,186,189,125,128,198,155,186];
    demand = [
    [31,25],
    [61,22],
    [74,10],
    [34,18],
    [79,117],
    [70,34],
    [58,90],
    [73,12],
    [36,19],
    [109,75],
    [20,71],
    [106,120]
    ];
    penaltyCost = [424,359];
    openingProductInventory = [97,77];
    openingComponentInventory = [82,98,88];
    productHoldingCost = [19,30];
    componentHoldingCost = [14,1,26];
    setupCosts = [1900,3000];

#### Agent: A2

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [77];
    leadtime = [0];
    cycles = [2];
    setupCycles = [8];
    capacity = [69,97,99,75,67,58,75,100,90,87,84,78];
    openingProductInventory = [53];
    openingComponentInventory = [60];
    productHoldingCost = [14];
    componentHoldingCost = [10];
    setupCosts = [1400];
    maxNumComponentOrders = [30];
    componentBatchSize = [91];
    deliveryCost = [3458];

#### Agent: A4

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [91];
    leadtime = [1];
    cycles = [2];
    setupCycles = [9];
    capacity = [83,57,88,72,64,55,85,99,50,80,75,53];
    openingProductInventory = [40];
    openingComponentInventory = [64];
    productHoldingCost = [26];
    componentHoldingCost = [23];
    setupCosts = [2600];
    maxNumComponentOrders = [30];
    componentBatchSize = [89];
    deliveryCost = [2759];

#### Agent: A5

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [91];
    leadtime = [0];
    cycles = [2];
    setupCycles = [6];
    capacity = [76,66,99,54,62,86,97,84,65,92,76,56];
    openingProductInventory = [94];
    openingComponentInventory = [59];
    productHoldingCost = [10];
    componentHoldingCost = [10];
    setupCosts = [1000];
    componentArrivals = [
    [39],
    [32],
    [30],
    [48],
    [49],
    [39],
    [39],
    [50],
    [49],
    [46],
    [27],
    [28]
    ];

#### Agent: A6

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [93];
    leadtime = [0];
    cycles = [2];
    setupCycles = [9];
    capacity = [72,66,76,55,98,52,80,68,51,54,92,55];
    openingProductInventory = [80];
    openingComponentInventory = [28];
    productHoldingCost = [1];
    componentHoldingCost = [4];
    setupCosts = [100];
    componentArrivals = [
    [33],
    [35],
    [47],
    [31],
    [41],
    [38],
    [32],
    [29],
    [43],
    [32],
    [39],
    [26]
    ];

#### Agent: A7

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [89];
    leadtime = [0];
    cycles = [1];
    setupCycles = [5];
    capacity = [82,75,70,71,82,53,51,60,65,75,86,64];
    openingProductInventory = [47];
    openingComponentInventory = [58];
    productHoldingCost = [23];
    componentHoldingCost = [21];
    setupCosts = [2300];
    componentArrivals = [
    [91],
    [50],
    [56],
    [59],
    [61],
    [83],
    [97],
    [85],
    [64],
    [88],
    [68],
    [58]
    ];