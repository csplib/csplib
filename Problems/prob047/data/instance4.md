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
    componentBatchSize = [61,71,95];
    deliveryCost = [1708,2769,2850];
    leadtime = [2,2];
    cycles = [2,2];
    setupCycles = [9,8];
    capacity = [151,167,198,121,126,128,168,103,155,118,183,134];
    demand = [
    [54,14],
    [82,85],
    [29,103],
    [40,80],
    [70,114],
    [66,103],
    [91,71],
    [33,108],
    [46,25],
    [104,75],
    [115,99],
    [15,41]
    ];
    penaltyCost = [395,373];
    openingProductInventory = [42,59];
    openingComponentInventory = [77,14,42];
    productHoldingCost = [16,5];
    componentHoldingCost = [11,2,5];
    setupCosts = [1600,500];

#### Agent: A2

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [61];
    leadtime = [2];
    cycles = [1];
    setupCycles = [6];
    capacity = [100,62,59,50,54,77,68,79,72,95,75,83];
    openingProductInventory = [24];
    openingComponentInventory = [34];
    productHoldingCost = [11];
    componentHoldingCost = [10];
    setupCosts = [1100];
    maxNumComponentOrders = [30];
    componentBatchSize = [69];
    deliveryCost = [2208];

#### Agent: A3

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [71];
    leadtime = [2];
    cycles = [1];
    setupCycles = [6];
    capacity = [87,54,57,64,54,53,100,77,77,85,97,72];
    openingProductInventory = [88];
    openingComponentInventory = [63];
    productHoldingCost = [2];
    componentHoldingCost = [4];
    setupCosts = [200];
    maxNumComponentOrders = [30];
    componentBatchSize = [68];
    deliveryCost = [2992];

#### Agent: A4

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [95];
    leadtime = [0];
    cycles = [1];
    setupCycles = [6];
    capacity = [68,98,67,62,68,61,56,61,90,71,84,78];
    openingProductInventory = [20];
    openingComponentInventory = [51];
    productHoldingCost = [5];
    componentHoldingCost = [4];
    setupCosts = [500];
    maxNumComponentOrders = [30];
    componentBatchSize = [99];
    deliveryCost = [4851];

#### Agent: A5

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [69];
    leadtime = [2];
    cycles = [1];
    setupCycles = [7];
    capacity = [89,63,83,97,52,64,56,82,94,93,78,72];
    openingProductInventory = [85];
    openingComponentInventory = [12];
    productHoldingCost = [10];
    componentHoldingCost = [9];
    setupCosts = [1000];
    componentArrivals = [
    [85],
    [97],
    [86],
    [91],
    [100],
    [76],
    [77],
    [96],
    [91],
    [75],
    [98],
    [76]
    ];

#### Agent: A6

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [68];
    leadtime = [2];
    cycles = [1];
    setupCycles = [5];
    capacity = [100,95,78,78,98,81,67,78,85,68,77,70];
    openingProductInventory = [86];
    openingComponentInventory = [49];
    productHoldingCost = [4];
    componentHoldingCost = [3];
    setupCosts = [400];
    componentArrivals = [
    [59],
    [56],
    [64],
    [64],
    [91],
    [90],
    [74],
    [53],
    [51],
    [63],
    [98],
    [88]
    ];

#### Agent: A7

    numPeriods = 12;
    numProducts = 1;
    numComponents = 1;
    maxNumProductOrders = [30];
    bom = [
    [1]
    ];
    productBatchSize = [99];
    leadtime = [0];
    cycles = [2];
    setupCycles = [5];
    capacity = [55,60,75,86,63,84,53,84,98,96,94,59];
    openingProductInventory = [33];
    openingComponentInventory = [47];
    productHoldingCost = [4];
    componentHoldingCost = [5];
    setupCosts = [400];
    componentArrivals = [
    [39],
    [27],
    [28],
    [44],
    [49],
    [41],
    [32],
    [43],
    [50],
    [39],
    [27],
    [30]
    ];