
\* 
     Quasigroup generator for ILOG Solver

     Code due to Kostas Stergiou

     All the usual disclaimers apply

*\

#include <ilsolver/ilcint.h>
#include <stdlib.h>

class quasi {

public :
   quasi(IlcManager, IlcInt, IlcInt);
   void set_constraints();
   void print_solution(IlcInt);

private :
   void row_constraints(IlcInt, IlcInt, IlcInt);
   void column_constraints(IlcInt, IlcInt, IlcInt);
   void QG1();
   void QG2();
   void QG3();
   void QG4();
   void QG5();
   void QG6();
   void QG7();
   IlcManager manager;
   IlcInt n;
   IlcInt axiom;
   IlcIntVarArray square;
};


quasi::quasi(IlcManager m, IlcInt order, IlcInt property) :

 manager(m),
 n(order),
 axiom(property),
 square(m,n*n,0,n-1) {

}

void quasi::print_solution(IlcInt max)
{
 IncInt i,j;

 for (i=0; i<max*max; i++)
       manager.out() << square[i] << " ";
}

void quasi::row_constraints(IlcInt min, IlcInt max, IlcInt rowno)
{
 IlcInt j;
 IlcIntVarArray array(manager,max);

 for (j=min; j<max; j++)
    array[j]=square[rowno*max+j];
 manager.add(IlcAllDiff(array, IlcWhenDomain));
}


void quasi::column_constraints(IlcInt min, IlcInt max, IlcInt columnno)
{
 IlcInt j;
 IlcIntVarArray array(manager,max);

 for (j=min; j<max; j++)
    array[j]=square[j*max+columnno];
 manager.add(IlcAllDiff(array, IlcWhenDomain));
}


void quasi::set_constraints()
{
 IlcInt i;

 for (i=0; i<n; i++) row_constraints(0, n, i);          // AllDiff for rows
 for (i=0; i<n; i++) column_constraints(0, n, i);  // AllDiff for columns
 for (i=0; i<n; i++) manager.add(square[i*n+i]==i);   // Idempotency
 for (i=0; i<n; i++) manager.add(square[i*n+n-1]>=i-1);  // Symmetry Break
 switch (axiom) {

        case 3 : QG3(); break;
        case 4 : QG4(); break;
        case 5 : QG5(); break;
        case 6 : QG6(); break;
        case 7 : QG7(); break;
   default : printf("no such condition\n"); exit(1);
 }

 IlcGoal goal;
 goal = IlcGenerate(square, IlcChooseMinSizeInt);
 manager.add(goal);
}

void quasi::QG3()
{
 IlcInt i,j;

 for (i=0; i<n; i++) {                                                                                  
         for (j=0; j<n; j++) {
                manager.add(square[square[i*n+j]*n+square[j*n+i]]==i);
    }
 }
}

void quasi::QG4()
{
 IlcInt i,j;

 for (i=0; i<n; i++) {                                                                                  
         for (j=0; j<n; j++) {
                manager.add(square[square[j*n+i]*n+square[i*n+j]]==i);
    }
 }
}

void quasi::QG5()
{
 IlcInt i,j;

 for (i=0; i<n; i++) {                                                                                  
         for (j=0; j<n; j++) {
                manager.add(square[square[square[j*n+i]*n+j]*n+j]==i);
    }
 }
}

void quasi::QG6()
{
 IlcInt i,j;

 for (i=0; i<n; i++) {                                                                                  
         for (j=0; j<n; j++) {
                manager.add(square[square[i*n+j]*n+j]==square[i*n+square[i*n+j]]);
    }
 }
}

void quasi::QG7()
{
 IlcInt i,j;

 for (i=0; i<n; i++) {                                                                                  
         for (j=0; j<n; j++) {
                manager.add(square[square[j*n+i]*n+j]==square[i*n+square[j*n+i]]);
    }
 }
}


IlcInt main(int argc, char **argv) {

  IlcInt order, axiom, NumberOfModels=0;

  if (argc!=3) {
                printf("USAGE\n");
                printf("Order  Axiom\n");
      exit(1);
  }
  order = atoi(argv[1]);
  axiom = atoi(argv[2]);
  IlcManager m(IlcNoEdit);
  quasi qg(m,order,axiom);
  qg.set_constraints();
  if (m.nextSolution()) {
     qg.print_solution(order);
  }
  return 0;
}
