#include <ilsolver/ilcint.h>
#include <stdlib.h>


class rulers {

public :
   rulers(IlcManager, IlcInt, IlcInt, IlcInt, IlcInt, IlcInt);
   ~rulers() { };
   void print_solution();
   void print(IlcIntVar);

private :
   IlcManager manager;
   IlcInt n;
   IlcInt m;
   IlcInt diffs;
   IlcInt heuristic;
   IlcInt propagation_method;
   IlcIntVarArray originalVars;
   IlcIntVarArray auxiliaryVars;
};

rulers::rulers(IlcManager man, IlcInt vars, IlcInt dom, IlcInt added_vars, 
IlcInt h, IlcInt method) :

 manager(man),
 n(vars),
 m(dom),
 diffs(added_vars),
 heuristic(h),
 propagation_method(method),
 originalVars(man,n,0,m-1),
 auxiliaryVars(man,added_vars,0,m-1) {

 IlcInt i,j,counter=0;

 if (!propagation_method) {
 	manager.add(IlcAllDiff(auxiliaryVars, IlcWhenDomain));
 }
 else {
 	manager.add(IlcAllDiff(auxiliaryVars, IlcWhenValue));
 }

 for (i=0; i<(n-1); i++) manager.add(originalVars[i] < originalVars[i+1]);	
 manager.add(originalVars[0]==0);
 manager.add(originalVars[n-1]==m-1);

 for (i=0; i<(n-1); i++) {
    for (j=i+1; j<n; j++) {	 
 	manager.add(originalVars[j]-originalVars[i]==auxiliaryVars[counter]);
	counter++;
    }
 }

 IlcGoal goal;
 if (heuristic==1) goal = IlcGenerate(originalVars, IlcChooseMinSizeInt);
 else if (heuristic==2) goal = IlcGenerate(originalVars);
 else exit(1);
 manager.add(goal);
}



void rulers::print_solution()
{
  IlcInt i;

  for (i = 0; i < n; i++){
         print(originalVars[i]);
         manager.out() << " ";
  }
  manager.out() << endl;
  manager.printInformation();
}


void rulers::print(IlcIntVar var)
{
 if (var.isBound())
	 var.getManager().out() << " " << var.getValue();
 else  var.getManager().out() << " ||";
}



int main(int argc, char **argv) {

 IlcInt vars, added_vars, domain, heuristic, backtrack_limit, 
propagation_method;

 if (argc!=6) {
      cout << "USAGE" << endl;
      cout << "#Variables   Domain Size     Backtrack limit   Propagation 
Method (GAC-0,AC-1)";
      cout << "    Heuristic(FF-1, Lx-2)" << endl;
      exit(1);
 }
 vars = atoi(argv[1]);
 domain = atoi(argv[2]);
 propagation_method = atoi(argv[4]);
 backtrack_limit = atoi(argv[3]);
 heuristic = atoi(argv[5]);
 added_vars = (vars*(vars-1))/2;

 IlcManager m(IlcEdit);
 #if defined(ILCLOGFILE)
    	m.openLogFile("output.log");
 #endif

 rulers r(m, vars, domain, added_vars, heuristic, propagation_method);
 m.setFailLimit(backtrack_limit);
 if (m.nextSolution()) {
        r.print_solution();
       	m.out() << endl;
 }
 else {
 	cout << endl << m.getNumberOfFails() << " branches";
        cout << endl << m.getNumberOfChoicePoints() << " choice points";
        cout << endl << m.getTime() << " CPU time" << endl;
 }      
 #if defined(ILCLOGFILE)
     	m.closeLogFile();
 #endif

 m.end();
 return 0;
}



