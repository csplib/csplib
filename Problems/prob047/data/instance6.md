---
Title: (10 agents, 3 tiers)
---

#### Note on instance description

This instance is based on a supply chain topology referred to as the 'W' system, which is composed of 2 final products produced from 3 components. Each product is assembled from a common and a unique component. This system has the characteristics of pure distribution (common component is used in both products) and pure assembly structure. W system is well-known in the assemble-to-order inventory literature.

The instance description consists of two parts:

1.  Topology: the supply chain network topology consists of (i) a list of agents; (ii) a list of products that each agent produces (*agent-name=product-name(s)*); and (iii) a bill of materials specifying what products are used in building other products (*product-name=component-name(s)*).
2.  Local problems: the private data describing each agent's local problem - this is in the form of an OPL data file and is for use in conjunction with the OPL models provided in this benchmark description.

#### Topology

    Agents: A1,A2,A3,A4,A5,A6,A7,A8,A9,A10

    Products: 
    A1=P0,P1
    A2=P2
    A3=P3
    A4=P4
    A5=P5
    A6=P6
    A7=P7
    A8=P8
    A9=P9
    A10=P10

    BOM:
    P0=P2,P3
    P1=P3,P4
    P2=P5,P8
    P3=P6,P9
    P4=P7,P10

#### Agent: A1

    numPeriods = 12;
    numProducts = 2;
    numComponents = 3;
    maxNumComponentOrders = [30,30,30];
    bom = [
    [1,1,0],
    [0,1,1]
    ];
    componentBatchSize = [50,60,70];
    deliveryCost = [1300,2460,1540];
    leadtime = [0,2];
    cycles = [2,2];
    setupCycles = [8,7];
    capacity = [198,139,145,166,171,108,155,120,174,156,141,130];
    demand = [
    [97,28],
    [72,106],
    [92,71],
    [90,22],
    [17,59],
    [20,59],
    [78,94],
    [42,50],
    [13,42],
    [52,84],
    [113,113],
    [52,109]
    ];
    penaltyCost = [463,472];
    openingProductInventory = [80,21];
    openingComponentInventory = [37,40,83];
    productHoldingCost = [90,71];
    componentHoldingCost = [45,49,22];
    setupCosts = [9000,7100];

#### Agent: A2

    numPeriods = 12;
    numProducts = 1;
    numComponents = 2;
    maxNumProductOrders = [30];
    bom = [
    [1,1]
    ];
    productBatchSize = [50];
    leadtime = [0];
    cycles = [2];
    setupCycles = [9];
    capacity = [60,87,61,53,69,67,86,79,69,52,77,93];
    openingProductInventory = [71];
    openingComponentInventory = [79,11];
    productHoldingCost = [45];
    componentHoldingCost = [20,26];
    setupCosts = [4500];
    maxNumComponentOrders = [30,30];
    componentBatchSize = [93,84];
    deliveryCost = [4464,2940];

#### Agent: A3

    numPeriods = 12;
    numProducts = 1;
    numComponents = 2;
    maxNumProductOrders = [30];
    bom = [
    [1,1]
    ];
    productBatchSize = [60];
    leadtime = [1];
    cycles = [1];
    setupCycles = [5];
    capacity = [58,85,50,75,84,65,72,59,64,61,99,62];
    openingProductInventory = [81];
    openingComponentInventory = [84,89];
    productHoldingCost = [49];
    componentHoldingCost = [26,23];
    setupCosts = [4900];
    maxNumComponentOrders = [30,30];
    componentBatchSize = [54,100];
    deliveryCost = [2106,2300];

#### Agent: A4

    numPeriods = 12;
    numProducts = 1;
    numComponents = 2;
    maxNumProductOrders = [30];
    bom = [
    [1,1]
    ];
    productBatchSize = [70];
    leadtime = [1];
    cycles = [2];
    setupCycles = [9];
    capacity = [59,98,82,99,92,99,77,86,93,60,100,84];
    openingProductInventory = [15];
    openingComponentInventory = [44,24];
    productHoldingCost = [22];
    componentHoldingCost = [12,8];
    setupCosts = [2200];
    maxNumComponentOrders = [30,30];
    componentBatchSize = [95,91];
    deliveryCost = [2375,4277];

#### Agent: A5

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [93];
    leadtime = [2];
    cycles = [2];
    setupCycles = [7];
    capacity = [78,77,63,56,70,86,87,95,98,87,86,88];
    openingProductInventory = [73];
    openingComponentInventory = [24];
    productHoldingCost = [20];
    componentHoldingCost = [23];
    setupCosts = [2000];
    componentArrivals = [
    [45],
    [35],
    [44],
    [25],
    [44],
    [31],
    [40],
    [38],
    [42],
    [43],
    [40],
    [41]
    ];

#### Agent: A6

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [54];
    leadtime = [1];
    cycles = [2];
    setupCycles = [7];
    capacity = [89,69,52,50,81,97,68,85,74,71,55,58];
    openingProductInventory = [40];
    openingComponentInventory = [83];
    productHoldingCost = [26];
    componentHoldingCost = [24];
    setupCosts = [2600];
    componentArrivals = [
    [29],
    [28],
    [36],
    [47],
    [43],
    [44],
    [48],
    [29],
    [36],
    [33],
    [44],
    [42]
    ];

#### Agent: A7

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [95];
    leadtime = [2];
    cycles = [2];
    setupCycles = [7];
    capacity = [59,79,81,58,82,85,54,52,68,78,93,97];
    openingProductInventory = [78];
    openingComponentInventory = [80];
    productHoldingCost = [12];
    componentHoldingCost = [9];
    setupCosts = [1200];
    componentArrivals = [
    [45],
    [38],
    [48],
    [38],
    [38],
    [30],
    [39],
    [43],
    [43],
    [35],
    [43],
    [36]
    ];

#### Agent: A8

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [84];
    leadtime = [0];
    cycles = [1];
    setupCycles = [5];
    capacity = [52,88,78,69,63,97,95,93,76,57,55,53];
    openingProductInventory = [56];
    openingComponentInventory = [51];
    productHoldingCost = [26];
    componentHoldingCost = [23];
    setupCosts = [2600];
    componentArrivals = [
    [73],
    [53],
    [96],
    [71],
    [69],
    [73],
    [87],
    [67],
    [87],
    [85],
    [100],
    [77]
    ];

#### Agent: A9

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [100];
    leadtime = [2];
    cycles = [1];
    setupCycles = [8];
    capacity = [55,75,57,77,97,89,60,53,56,60,53,90];
    openingProductInventory = [10];
    openingComponentInventory = [26];
    productHoldingCost = [23];
    componentHoldingCost = [21];
    setupCosts = [2300];
    componentArrivals = [
    [70],
    [65],
    [84],
    [69],
    [85],
    [58],
    [73],
    [79],
    [83],
    [77],
    [78],
    [89]
    ];

#### Agent: A10

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [91];
    leadtime = [1];
    cycles = [1];
    setupCycles = [7];
    capacity = [59,76,97,74,85,72,84,88,62,95,94,65];
    openingProductInventory = [57];
    openingComponentInventory = [84];
    productHoldingCost = [8];
    componentHoldingCost = [8];
    setupCosts = [800];
    componentArrivals = [
    [54],
    [51],
    [60],
    [56],
    [78],
    [92],
    [70],
    [57],
    [68],
    [85],
    [95],
    [85]
    ];