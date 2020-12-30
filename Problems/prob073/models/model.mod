/*********************************************
 * OPL 12.6.2.0 Model
 * Author: laborie
 * Creation Date: Sep 4, 2015 at 3:21:50 PM
 *********************************************/

using CP;

tuple Task {
  key string name;
  int duration;
  {string} machines;
  {string} resources;
};

{Task}   Tasks = ...;
{string} Machines = ...;
{string} Resources = ...;

tuple Alloc {
  string tname;
  string machine;
};

{Alloc} Allocs = 
  { <t.name, m> | t in Tasks, m in t.machines } union
  { <t.name, m> | t in Tasks, m in Machines : card(t.machines)==0 };

dvar interval tasks[t in Tasks] size t.duration;
dvar interval allocs[a in Allocs] optional;
cumulFunction machines = sum(t in Tasks) pulse(tasks[t],1); // Redundant cumulative

minimize max(t in Tasks) endOf(tasks[t]);
subject to {
  forall(t in Tasks) {
    alternative(tasks[t], all(a in Allocs: a.tname==t.name) allocs[a]);
  }
  forall(m in Machines) {
    noOverlap(all(a in Allocs: a.machine==m) allocs[a]);
  }
  forall(r in Resources) {
    noOverlap(all(t in Tasks: r in t.resources) tasks[t]);
  }
  machines <= card(Machines);
}
