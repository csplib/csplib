## MASP Problem Data File

This file defines example parameters for a **Medical Appointment Scheduling Problem (MASP)** instance. Each resource calendar spans **9 scheduling days**, each comprising **24 time slots**. The first slot of the day starts at **08:00**.

---

### 🗂 Resource Calendars

Each resource calendar is represented by an array of available **slot identifiers**. A value of `-1` or any negative number indicates **unavailability** on that slot.

```java
// Each resource calendar = 9 days × 24 slots
private final int[] resourceCalendars = {
    // Monday (0) – 07.07.2025
    0, 1, 2, ..., 23,
    // Tuesday (1) – 08.07.2025
    24, 25, ..., 47,
    // Wednesday (2) – 09.07.2025
    48, 49, ..., 71,
    // Thursday (3) – 10.07.2025
    72, 73, ..., 95,
    // Friday (4) – 11.07.2025
    96, 97, ..., 119,
    // Saturday (5) – 12.07.2025 (Unavailable)
    -1, -2, ..., -24,
    // Sunday (6) – 13.07.2025 (Unavailable)
    -25, -26, ..., -48,
    // Monday (7) – 14.07.2025
    168, 169, ..., 191,
    // Tuesday (8) – 15.07.2025
    192, 193, ..., 215
};
```

---

### 🗂 Global Calendar

Represents the combined availability of the entire facility. This array determines global time slot coverage.

```java
private final int[] globalCalendars = {
    // Monday (0)
    0, 1, ..., 23,
    // ...
    // Saturday (5)
    120, ..., 144,
    // Sunday (6) – Unavailable
    -145, ..., -167,
    // Monday (7)
    168, ..., 191,
    // Tuesday (8)
    192, ..., 215
};
```

---

### 📆 Patient Preferences

#### Preferred Dates (by slot index)
```java
private final int[] preferredDatesByPatient = {3, 8}; // Thursday and Tuesday
```

#### Undesired Weekdays
```java
private final int[] undesiredWeekDaysByPatient = {1}; // Tuesday (1)
```

#### Preferred Doctors by Specialization
```java
private final Map<Integer, List<Integer>> preferredDoctorsByPatient = Map.of(
    0, List.of(4, 8),   // Cardiologists
    1, List.of(13, 17)  // Neurologists
);
```

#### Preferred Time Slots
```java
List<PreferedTime> preferredTimesByPatient = new ArrayList<>(Arrays.asList(
    new PreferedTime(3, 0, 7) // Thursday: slots 0–7 (08:00–09:45)
    // new PreferedTime(2, 6, 9) // Wednesday: slots 6–9 (09:30–10:45) (optional)
));
```

---

### 📌 Notes

- Slot identifiers start at `0` for Monday, 07.07.2025 at 08:00.
- Each day consists of 24 slots (15-minute increments from 08:00 to 14:45).
- Negative slot IDs indicate **unavailable time slots**.

---

> 📖 For full modeling context, refer to the MASP documentation or the publication [10.5220/0013091300003890](https://doi.org/10.5220/0013091300003890).
