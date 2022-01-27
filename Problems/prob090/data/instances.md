The solution of the problem is provided in the Minizinc framework (see Models directory). Hence, in order to solve the problem, the input model file (mzn) needs to be accompanied by input files (dzn) with the following structure:
- NoComponents - number of components of the Wordpress problem;
- HardwareREQ - number of hardware requirements; for example, if we consider CPU, memory, storage, then HardwareREQ=3;
- VMOffers - number of virtual machines offers
- VMSpecs - virtual machines offers
- CompREQ - minimum requirements for each component
- VMPrice - the cost of each virtual machine

The dzn files we consider are taking into account different number of VM offers, i.e. 20, 40, 250, 500.
