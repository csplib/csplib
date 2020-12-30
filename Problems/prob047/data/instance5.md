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
    componentBatchSize = [97,98,71];
    deliveryCost = [3298,4116,3124];
    leadtime = [2,2];
    cycles = [1,2];
    setupCycles = [10,9];
    capacity = [160,115,109,178,149,170,195,154,170,136,158,194];
    demand = [
    [92,13],
    [95,48],
    [55,53],
    [47,84],
    [101,58],
    [31,104],
    [117,113],
    [14,18],
    [12,89],
    [59,19],
    [102,85],
    [97,15]
    ];
    penaltyCost = [453,419];
    openingProductInventory = [78,10];
    openingComponentInventory = [8,45,15];
    productHoldingCost = [49,65];
    componentHoldingCost = [20,34,33];
    setupCosts = [4900,6500];

#### Agent: A2

    numPeriods = 12;
    numProducts = 1;
    numComponents = 2;
    maxNumProductOrders = [30];
    bom = [
    [1,1]
    ];
    productBatchSize = [97];
    leadtime = [2];
    cycles = [2];
    setupCycles = [7];
    capacity = [92,54,97,79,77,58,99,56,51,83,98,65];
    openingProductInventory = [4];
    openingComponentInventory = [51,59];
    productHoldingCost = [20];
    componentHoldingCost = [19,1];
    setupCosts = [2000];
    maxNumComponentOrders = [30,30];
    componentBatchSize = [90,95];
    deliveryCost = [2790,2565];

#### Agent: A3

    numPeriods = 12;
    numProducts = 1;
    numComponents = 2;
    maxNumProductOrders = [30];
    bom = [
    [1,1]
    ];
    productBatchSize = [98];
    leadtime = [0];
    cycles = [1];
    setupCycles = [9];
    capacity = [62,50,71,72,59,87,88,50,68,65,90,90];
    openingProductInventory = [40];
    openingComponentInventory = [44,87];
    productHoldingCost = [34];
    componentHoldingCost = [14,23];
    setupCosts = [3400];
    maxNumComponentOrders = [30,30];
    componentBatchSize = [52,84];
    deliveryCost = [1508,3276];

#### Agent: A4

    numPeriods = 12;
    numProducts = 1;
    numComponents = 2;
    maxNumProductOrders = [30];
    bom = [
    [1,1]
    ];
    productBatchSize = [71];
    leadtime = [2];
    cycles = [1];
    setupCycles = [9];
    capacity = [61,63,51,85,65,64,97,91,86,94,51,91];
    openingProductInventory = [92];
    openingComponentInventory = [85,34];
    productHoldingCost = [33];
    componentHoldingCost = [15,23];
    setupCosts = [3300];
    maxNumComponentOrders = [30,30];
    componentBatchSize = [88,50];
    deliveryCost = [3960,2300];

#### Agent: A5

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [90];
    leadtime = [1];
    cycles = [1];
    setupCycles = [10];
    capacity = [61,85,92,66,56,85,58,58,93,85,70,67];
    openingProductInventory = [53];
    openingComponentInventory = [39];
    productHoldingCost = [19];
    componentHoldingCost = [21];
    setupCosts = [1900];
    componentArrivals = [
    [56],
    [51],
    [87],
    [99],
    [92],
    [64],
    [53],
    [64],
    [86],
    [95],
    [50],
    [100]
    ];

#### Agent: A6

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [52];
    leadtime = [1];
    cycles = [1];
    setupCycles = [10];
    capacity = [51,84,76,77,67,89,75,50,76,98,67,79];
    openingProductInventory = [43];
    openingComponentInventory = [55];
    productHoldingCost = [14];
    componentHoldingCost = [18];
    setupCosts = [1400];
    componentArrivals = [
    [62],
    [98],
    [78],
    [65],
    [71],
    [91],
    [98],
    [100],
    [92],
    [60],
    [97],
    [54]
    ];

#### Agent: A7

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [88];
    leadtime = [0];
    cycles = [2];
    setupCycles = [7];
    capacity = [52,53,86,85,100,84,67,61,73,75,97,66];
    openingProductInventory = [15];
    openingComponentInventory = [95];
    productHoldingCost = [15];
    componentHoldingCost = [16];
    setupCosts = [1500];
    componentArrivals = [
    [50],
    [30],
    [47],
    [36],
    [36],
    [25],
    [34],
    [40],
    [43],
    [42],
    [40],
    [28]
    ];

#### Agent: A8

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [95];
    leadtime = [2];
    cycles = [1];
    setupCycles = [6];
    capacity = [66,99,89,95,71,59,86,50,80,60,88,57];
    openingProductInventory = [77];
    openingComponentInventory = [71];
    productHoldingCost = [1];
    componentHoldingCost = [4];
    setupCosts = [100];
    componentArrivals = [
    [94],
    [76],
    [78],
    [61],
    [55],
    [61],
    [97],
    [86],
    [56],
    [70],
    [91],
    [72]
    ];

#### Agent: A9

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [84];
    leadtime = [1];
    cycles = [2];
    setupCycles = [9];
    capacity = [69,50,53,62,75,53,67,66,79,87,69,58];
    openingProductInventory = [52];
    openingComponentInventory = [11];
    productHoldingCost = [23];
    componentHoldingCost = [23];
    setupCosts = [2300];
    componentArrivals = [
    [26],
    [33],
    [45],
    [40],
    [49],
    [48],
    [36],
    [48],
    [44],
    [33],
    [27],
    [48]
    ];

#### Agent: A10

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [50];
    leadtime = [2];
    cycles = [1];
    setupCycles = [6];
    capacity = [96,52,65,99,53,90,88,71,52,57,64,58];
    openingProductInventory = [1];
    openingComponentInventory = [15];
    productHoldingCost = [23];
    componentHoldingCost = [19];
    setupCosts = [2300];
    componentArrivals = [
    [58],
    [88],
    [60],
    [95],
    [60],
    [96],
    [96],
    [86],
    [87],
    [78],
    [52],
    [68]
    ];