See the following sample solution for instance Rostering-*8-M-2-4*, where *M* =

| Shift \ Day | Mo | Tu | We | Th | Fr | Sa | Su |
|-------------|---:|---:|---:|---:|---:|---:|---:|
| Day Off     | 2  | 2  | 2  | 2  | 2  | 4  | 4  |
| Early       | 2  | 2  | 2  | 2  | 2  | 2  | 2  |
| Late        | 2  | 2  | 2  | 2  | 2  | 1  | 1  |
| Night       | 2  | 2  | 2  | 2  | 2  | 1  | 1  |

Solution:

|            | Mo      | Tu       | We       | Th       | Fr       | Sa       | Su       |
|------------|---------|----------|----------|----------|----------|----------|----------|
| Employee 1 | Night   |  Day Off |  Day Off |  Day Off |  Early   |  Early   |  Early   |
| Employee 2 | Day Off |  Day Off |  Early   |  Early   |  Early   |  Late    |  Late    |
| Employee 3 | Late    |  Night   |  Night   |  Night   |  Night   |  Day Off |  Day Off |
| Employee 4 | Early   |  Early   |  Early   |  Early   |  Day Off |  Day Off |  Day Off |
| Employee 5 | Early   |  Early   |  Late    |  Late    |  Late    |  Day Off |  Day Off |
| Employee 6 | Night   |  Night   |  Day Off |  Day Off |  Day Off |  Early   |  Early   |
| Employee 7 | Late    |  Late    |  Night   |  Night   |  Night   |  Day Off |  Day Off |
| Employee 8 | Day Off |  Late    |  Late    |  Late    |  Late    |  Night   |  Night   |

The Solution can be read as follows:

 - For all 8 employees the plan of the first week is given.
 - The second week for “Employee 1” is written in row “Employee 2”
 - The third week for “Employee 1” and the second week of “Employee 2” is given in row “Employee 3”
 - …
 - The second week for “Employee 8” is written in row “Employee 1”
 - The Third week for “Employee 8” is written in row “Employee 2”
 - …
