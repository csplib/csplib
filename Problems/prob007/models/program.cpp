
#include<ilsolver/ilcint.h>

ILOSTLBEGIN

class IlcIntSelectMaxI : public IlcIntSelectI {
public:
    IlcIntSelectMaxI(){};
    virtual IlcInt select(IlcIntVar var);
};

IlcInt IlcIntSelectMaxI::select(IlcIntVar var){
    return var.getMax();
}

IlcIntSelect IlcIntSelectMax(IlcManager m){
    return IlcIntSelect(new (m.getHeap()) IlcIntSelectMaxI());
}

void prob007large(IlcManager m, int n){
  IlcIntVarArray x(m, n, 0, n-1);
  IlcIntVarArray y(m, n-1, 1, n-1);
  int i;
  for (i=0; i<n-1; i++){
    m.add(y[i]==IlcAbs(x[i+1] - x[i]));
  }
  m.add(x[0] < x[1]);
  m.add(IlcAllDiff(x, IlcWhenRange));
  m.add(IlcAllDiff(y, IlcWhenRange));
  m.add(IlcGenerate(y, IlcChooseMaxMaxInt, IlcIntSelectMax(m)));
  m.add(IlcGenerate(x));
}

void prob007AC(IlcManager m, int n){
  IlcIntVarArray x(m, n, 0, n-1);
  IlcIntVarArray y(m, n-1, 1, n-1);
  IlcIntTupleSet set(m,3);
  int i,j;
  for(i=0;i<n-1;i++){
    for(j=i+1;j<n;j++){
      set.add(IlcIntArray(m,3, i, j, IlcAbs(j-i)));
      set.add(IlcIntArray(m,3, j, i, IlcAbs(j-i)));
    }
  }
  set.close();
  for (i=0; i<n-1; i++){
    IlcIntVarArray vars(m, 3, x[i], x[i+1], y[i]);
    m.add(IlcTableConstraint(vars, set, IlcTrue));
  }
  m.add(IlcAllDiff(x, IlcWhenDomain));
  m.add(IlcAllDiff(y, IlcWhenDomain));
  m.add(IlcGenerate(x, IlcChooseMinSizeInt));
}

void prob007(IlcManager m, int n) {
  IlcIntVarArray x(m, n, 0, n-1);
  IlcIntVarArray y(m, n-1, 1, n-1);
  for (int i=0; i<n-1; i++)
    m.add(y[i]==IlcAbs(x[i+1] - x[i]));
  m.add(IlcAllDiff(x, IlcWhenRange));
  m.add(IlcAllDiff(y, IlcWhenRange));
  m.add(IlcGenerate(x, IlcChooseMinSizeInt));
}

int main(int argc, char** argv) {
  IlcManager m(IlcNoEdit);
  int n = (argc>1)? atoi(argv[1]): 10;
  prob007(m, n);
  //prob007AC(m, n);
  int sol=0;
  //m.nextSolution();
  // m.nextSolution();

  //m.out() << x << endl;
   while(m.nextSolution()){
      sol++;
    }
    m.out() << sol << endl;
  m.printInformation();
  m.end();
  return 0;
}




