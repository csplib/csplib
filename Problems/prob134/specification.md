## Model Description

The **Medical Appointment Scheduling Problem (MASP)** focuses on assigning a set of medical resources at specific times to constitute a medical appointment for a given patient.

Given a set of medical resources—e.g., physicians—with their current schedules represented as calendars (each with _M_ time slots per day), the MASP must satisfy the following constraints:

- Resources must have sufficient available time slots in their schedules to cover the required appointment duration.
- Time slots allocated to the appointment must be **sequential** (i.e., no gaps are allowed).
- The correct medical resource types must be chosen to serve the appointment's purpose. For example, a cardiologist is required for cardiological conditions.
- The appointment must not be scheduled on **undesired dates** specified by the patient.
- Patient preferences—such as preferred dates, physicians, weekdays, and times—should be respected as much as possible.

---

## Model Parameters

- **T = { τ₁, ..., τₗ }**: A set of required **resource types** for the appointment.  
  Example:  
  **T = { Cardiologist, Neurologist, CT_Room }**  
  means that the appointment must include a cardiologist, a neurologist, and a CT scan room.

- **_d_**: The required **duration** of the appointment in time slots.  
  Assuming a slot is 15 minutes long,  
  **_d = 4_** implies an appointment duration of 60 minutes (1 hour).

- **_P<sub>r</sub>_**: A set of **preferred resource identifiers** (e.g., preferred physicians) specified by the patient.

- **_P<sub>d</sub>_**: A set of **preferred dates**, represented as column indices of the calendar matrix.

- A set of **preferred time slot specifications**. Each specification consists of:
  - Start slot index $s_p$ 
  - End slot index  $e_p$
  - Weekday index (0 = Monday, ..., 6 = Sunday) $p_w$

---

## Modelling

This section defines the **decision variables**, their **domains**, and the **constraints** governing the problem.

---
## Assumptions 

In the prototype Model (Choco implementation), we assume the duration of each time slot is 15 minutes and the number of slots per each calendar day is $M =24$ slots. Moreover, we assume that all resources have the same availability, i.e., they share slot identifers. Please also note that, resource calendars are unfolded into 1 D array with previously calculated slot identifers. To teach yourself on constructing time slot identiefers, please check our reference published paper.

---

## Problem Domains

We define two types of integer domains:

- **Resource Identifier Domain (R)**: Divided into sub-domains **R<sub>τ</sub>** for each **τ ∈ T**, depending on the resource type.  
  For instance, all cardiology physicians form a cardiology sub-domain.

- **Time Slot Identifier Domain**: Describes the available time slots of all resource calendars. Each slot is uniquely encoded by an identifier based on its start time and date.

---

## Decision Variables

- **V = { x₁, ..., xₗ }**: A set of decision variables over domains **R<sub>τ</sub>**, where **τ ∈ T**.  
  For example, if two cardiologists are needed, two variables over the cardiology domain are introduced.

- For each involved resource $z$ in resource domain, an auxiliary variable is declared:
  - **aux<sub>z</sub> ∈ S<sub>z</sub> \ {–1}**,  
    where **S<sub>z</sub>** is the calendar of resource **z** excluding unavailable slots (denoted by –1).  
    These track time slot allocations for that resource.
    The set of all auxuliary variables are indicated by $A = \{aux_1, \dots, aux_n\}$

- **N** decision variables for the **time slots** of the appointment:  
  **∀k ∈ {1, ..., N}**,  
  **t<sub>k</sub> ∈ S<sub>g</sub>**,  
  where **S<sub>g</sub>** is the global time slot domain (facility-wide calendar).



---

## Constraints

The model includes **hard constraints** (structural rules) and **soft constraints** (patient preferences).

### Hard Constraints

- **Distinct Resources**:  
  Resources allocated for the appointment must be different:
  ```math
  \texttt{AllDifferent}(\{x_1, x_2, \dots, x_l\}), \quad \forall\, \tau_i = \tau_j \in T
  ```

- **Sequential Time Slots**:  
  Appointment time slots must be consecutive:
  ```math
  t_k = t_{k-1} + 1, \quad \forall k = 2, \dots, N
  ```

- **Linking Constraint**:  
  Connects a resource assignment to the start time of the appointment:
  ```math
  (x_i \in R_\tau) \rightarrow (t_1 = \text{aux}_i)
  ```

### Soft Constraints

**Date Preference Violation**:  
  Indicates whether the assigned date violates patient preferences by one resource calendar $aux$:
  ```math
  v_{\text{date}, \text{aux}} =
  \begin{cases}
    1 & \text{if } \displaystyle \bigvee_{p \in P_d} \left(\left\lfloor \frac{\text{aux}}{M} \right\rfloor = \text{dateIndex}(p) \right) = 0 \\
    0 & \text{no violation}
  \end{cases}
  ```
The total date violation over all resource calendars is:
$$
 \text{DateViolation} = \sum_{a\in \mathcal{A}}  v_{\text{date}, \text{a}}
$$

 **Preferred Resource Violation**:
  ```math
  \text{resourceViolation} =
  \begin{cases}
    1 & \text{if } x \notin P_r \\
    0 & \text{no violation}
  \end{cases}
  ```
$x$ is a decison variable that is desired to be assigned a specififc resource $P_r$ by  the patient. Then, the violation for all prefered resources $V'\subset V$:

$$
\text{resourceViolation} = \sum_{x\in \mathcal{V'}}  \text{resourceViolation}({x})
$$
**Preferred  weekday and time violation**

For a resource calendar $aux$ and weekday preference $p_d$:

$$
\text{WeekDayMatch}_{aux,p_w} = \begin{cases} 
1 & \text{if } \left\lfloor \frac{aux}{M} \right\rfloor \mod 7 = p_w \\
0 & \text{otherwise}
\end{cases}
$$

*Implementation note*:  
Corresponds to `auxCalendarVar.div(24).mod(7).intVar()`

 Time window matching specified by preferred startTimeslot $s_p$ and endTimeSlot $e_p$ for one resource is expressed as 

$$
\text{timeMatch}_{aux,p} = \begin{cases} 
1 & \text{if } (aux \mod M) \in [s_p, e_p] \\
0 & \text{otherwise}
\end{cases}
$$

*Implementation note*:  
Corresponds to `auxCalendarVar.mod(24).intVar()`


A violation occurs when:
- Appointment is on the right day **AND**
- Outside the preferred time window

$$
\text{violation}_{aux,p} = \text{dayMatch}_{aux,p_w} \cdot (1 - \text{timeMatch}_{aux,p})
$$

Total violations over all calendars:

$$
 \text{weekDayTimeViolation} =\sum_{a \in \mathcal{A}} \sum_{p \in \mathcal{P}} \text{violation}_{a,p}
$$

**Optimization Function**
 As the goal is to minimize the violation of patient preferences, we define the objective function as: 
 $$
\text{Minimize} \quad F = \text{DateViolation}  + \text{resourceViolation} + \text{weekDayTimeViolation}  
 $$
