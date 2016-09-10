---
Title:    Interview Assignment Problem
Proposer: Helmut Simonis
Category: TimeTabling
---

This problem was used for the programming competition of the ACP
Summer School 2016 in Cork. The problem was defined in multiple
stages, adding constraints and problem data as the competition
progressed.

This is a totally made-up problem. None of the companies mentioned
  have been contacted, there are no interviews that will be arranged. Sorry! But there are conferences which run
  such a scheme, as well as some universities. The companies mentioned
  are interested in Constraint Programming, many are sponsoring the CP
  2016 conference, or have participants in the ACP Summer School.

#Initial Problem Statement

You have been asked to provide a tool that helps match students to companies in
an interview session at the next major Constraint Programming conference. A
number of companies have expressed interest in participating, there are currently
15 companies in the scheme.

Each student from set $S$ can express a preference to interview with each
company, with values ranging from 1 (very interested) to 5 (not at all interested).
We provide a comma separated text file that shows the answers that have been
collected through a Google Forms document (test.csv).
Each student should have three interviews during the conference, with different companies. The companies have expressed limits on how many interviews
they want to perform during the conference.
We want to find an assignment of students to companies that stays within
the capacity limits for each company, while providing the best match to the
student preferences. Overall quality will be measured by two quality
indicators:

1. If the preference of student $i$ for company $j$ is $p_{ij}$ , and the three companies selected for student $i$ are $s_{ik}$ with $k \in \[1, 3\]$, the first overall quality
  indicator is the sum of the preferences for the selected assignments,
  i.e. $\sum_{i \in S} \sum_{k=1}^{3} p_{is_{ik}}$ . We want to minimize this sum.

2. We do not want to have a solution that satisfies all preferences for some
  students, but not for others. As a secondary criterion, we want to minimize the maximum of the total preference costs per student, adding the
  preference values for their assignments, i.e. $\min \max_{i \in S}
  \sum_{k=1}^{3} p_{is_{ik}}$  .
  
The capacities for the companies are given in Table 1.


|Nr | Company | Capacity |
|--|--|--|
|1 |AIMMS |5|
|2 |SAS |5|
|3 |Keelvar |3|
|4 |Microsoft |10|
|5 |Google |10|
|6 |IBM |10|
|7 |Cadence |5|
|8 |Quintiq |10|
|9 |Siemens |10|
|10 |Cosling |3|
|11 |COSYTEC |3|
|12 |LocalSolver |3|
|13 |N-side |3|
|14 |UTRC-I |5|
|15 |Zoomer |5|

#Updated Problem (Day 2)
More students are signing up to the process, there is now an updated
file test2.csv with a larger number of participating students.

In the data yesterday there were some students that had only negative
preferences (5,...,5). This made it impossible to optimize the second
criterion on balancing the assignment between students. As a first
change we reduce the number of interviews to the minimum of three and
the number of preferences given with a value less than 4. If a student
only assigns preference value five, then no interviews should be scheduled. If
a student only gives two preferences of value three or better, then
only two interviews should be scheduled for the student.

Today, we also change the objective:

We first want to minimize the maximum *regret* over all students. This is
the difference between the sum of the assigned preferences, and the
three best indicated preferences. If a student had stated two preferences
with cost one, and two with cost two, the best three choices have value
four (1+1+2). If he is assigned to one interview with preference one, and two
with preference two, then the assigned cost is five (1+2+2), a resulting
regret of value 1. Minimizing the maximal regret is the highest
priority, and then the total cost (sum of all assigned preferences)
should be minimized as secondary criterion.

The companies have replied to the increased demand by also increasing
the number of slots available. But they are worried that not enough
student are assigned to their interviews, and they wont participate if
less than half of their slots are taken. This leads to two scenarios:

1. We can assign no students to a company, and incur a disappointment
  cost $d_{j}$ for the company. 
2. The number of students assigned is between a given lower and an
  upper bound. In this case the company is happy without extra cost.


For companies $j$, they should either be assigned no student,
at a disappointment cost $d_{j}$, or be assigned at least a given
minimum number of students $l_{j}$, and at most a maximum number
$u_{j}$ so that they are satisfied. The data for the companies is
given in Table 2.

|Nr |Company | Disappointment Cost | Min Assignment|Max Assignment |
|--|--|--|--|--|
|1 |AIMMS | 10 |5 | 10|
|2 | SAS | 10 | 5 |10|
|3 |Keelvar | 10 | 3 |6|
|4 |Microsoft |10 | 10 | 20|
|5 |Google |20 | 10 | 20|
|6 |IBM |10 | 10 | 20|
|7 |Cadence | 5 | 5 |10|
|8 |Quintiq | 10 | 10 | 20|
|9 |Siemens |10 | 10 | 20|
|10 |Cosling |5 | 3 |6 |
|11 |COSYTEC | 5| 3 |6|
|12 |LocalSolver |5 | 3|6|
|13 |N-side |5 | 3 | 6|
|14 |UTRC-I |5 | 5 |10|
|15 |Zoomer | 5 | 5 |10|

#Updated Problem (Day 3)

The interview assignment process seems to be working fine, but the
conference organizers see a problem with the scheduling of the
interviews. The conference is run over five days (Mon-Fri), with four
time periods on each day. The sessions are numbered from 1 (Monday, early
morning), to 20 (Friday, late afternoon). The students can indicate five of these
twenty slots, when they are available for interviews. They are not
available during any other period.  Otherwise the
conference organizers are worried that nobody will attend the talks of
the conference. 

|              | Mon | Tue | Wed | Thu | Fri |
|--|--|--|--|--|--|
|AM Early | 1 | 5 | 9 | 13 | 17 |
|AM Late | 2 | 6 | 10 | 14 | 18 |
|PM Early | 3 | 7 | 11 | 15 | 19 |
|PM Late | 4 | 8 | 12 | 16 | 20 |


Students can only have one interview during one session, so their
three interviews will occupy three of their five time slots. Companies
can perform two interviews in one session. All interviews are
scheduled in a set of suites, with each suite costing 200 units per
week. Using a small number of suites is a good idea, as if even just
one
interview is scheduled in a suite, then the complete weekly rate has
to be paid. Of course, each suite can hold only one interview at a
time. There are at most 12 suites available.

If an
interview of a company is scheduled in a period, then the company
representative has to attend the conference for that day. Indeed, the
company has to pay for all days between their earliest and the latest
interview. If for example the first interview is in session 7 (Tue), and the
latest interview in session 18 (Fri), then the company has to attend for
four days (Tue, Wed, Thu, Fri), even if no interviews for them are
scheduled on the Thursday. It therefore pays to group all interviews
for a company together. The daily rate varies with each company, and
is given in the company data below.

As the best solution yesterday allowed for a maximum of one regret only, this is
now imposed as a hard constraint, so the maximal preference regret for
each student is one. As all companies could be satisfied in yesterday's
optimal solution, we now have
to plan for all companies getting between their lower and their upper
bound of interviews, we can no longer disappoint them.

The objective now consists of the sum of the preference cost for the students,
the attendance cost for the companies, and the rental cost for the
interview suites. Using more suites might decrease the number of days
that the companies have to attend, as more interviews can be performed
in parallel, but increases the rental fee.

## Data Format

The time slots for the students are given in a new file
slots.csv, which on each line, lists the possible five time slots
for each student.

```
StudentNr,slot1,slot2,slot3,slot4,slot5
1,3,5,9,13,17
...
```

The cost of attendance for each company is given is
Table 4, which is otherwise unchanged.

|Nr |Company | DisappointmentCost |Min Assignment | Max Assignment |Attendance Cost|
|--|--|--|--|--|--|
|1 |AIMMS | 10 |5 | 10 | 20|
|2 | SAS | 10 | 5 |10 | 20|
|3 |Keelvar | 10 | 3 |6 | 10|
|4 |Microsoft |10 | 10 | 20| 30|
|5 |Google |20 | 10 | 20| 30|
|6 |IBM |10 | 10 | 20 | 30|
|7 |Cadence | 5 | 5 |10| 10|
|8 |Quintiq | 10 | 10 | 20 | 10|
|9 |Siemens |10 | 10 | 20 | 20|
|10 |Cosling |5 | 3 |6  | 5|
|11 |COSYTEC | 5| 3 |6 | 5|
|12 |LocalSolver |5 | 3|6 | 5|
|13 |N-side |5 | 3 | 6 | 5|
|14 |UTRC-I |5 | 5 |10| 10|
|15 |Zoomer | 5 | 5 |10 | 10|

# Updated Problem (Day 4)

Success! People have heard about your assignment tool, and want to use
it for their next conference. You can be proud of your modelling
skills! There is only a small problem: This is for a major conference,
with 15 companies participating, and up to 400 students that should be
assigned. The corresponding preference file is testNNN.csv, the slot
data are given in slotsNNN.csv,
and the company data are in a file companyNNN.csv.

The file companyNNN.csv contains the company specific data in
a .csv file. We've added a field to tell how many interviews the
company can perform in parallel in each session. The data from
yesterday are in the file company.csv shown in Table 5. The disappointment value
is not used.


|Company|Disappointment|Lower|Upper|DailyRate|Parallel|
|--|--|--|--|--|--|
|AIMMS|10|5|10|20|2|
|SAS|10|5|10|20|2|
|Keelvar|10|3|6|10|2|
|Microsoft|10|10|20|30|2|
|Google|20|10|20|30|2|
|IBM|10|10|20|30|2|
|Cadence|5|5|10|10|2|
|Quintiq|10|10|20|10|2|
|Siemens|10|10|20|20|2|
|Cosling|5|3|6|5|2|
|COSYTEC|5|3|6|5|2|
|LocalSolver|5|3|6|5|2|
|N-side|5|3|6|5|2|
|UTRC-I|5|5|10|10|2|
|Zoomer|5|5|10|10|2|


The new datasets are given in Table 6.


|Preferences | Slots | Company |
|--|--|--|
|test2.csv | slots.csv | company2.csv |
|test100.csv | slots100.csv | company100.csv |
|test200.csv | slots200.csv | company200.csv |
|test400.csv | slots400.csv | company400.csv |


We also may need more than 12 suites to schedule all the interviews.

Can you still provide a good assignment?

