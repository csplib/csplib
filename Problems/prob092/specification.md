---
Title:    Half-block School Timetabling
Proposer: Bérénice Dubois
Category: Timetabling
---

This is a timetabling problem with instances coming from real Canadian high schools, where students must be assigned to groups and these groups must be scheduled into compatible time clusters.

## Courses


Each **subject** may have multiple **course-groups** (i.e., sections). Each course-group represents one instance of a subject taught by a specific teacher in a specific room.

A course-group is defined by:
- subject
- teacher
- room
- grade level
- maximum capacity (`cap`)
- minimum capacity (`low`)

Each row in `schoolX_groups.csv` corresponds to one course-group.

|subject|room|teacher|grade|cap|low|
|-------|----|-------|-----|---|---|
|S1|l02|p02|sec4|32|21|
| S2| l09| p09| sec5|33|33|
| S3| l10| p10| sec5|32|30|
| S4| l04| p04| sec3|33|33|
| S5| l15| p16| sec4|32|26|
| S6| l03| p03| sec3|33|33|
| S7| l02| p02| sec3|33|33|
| S7| l02| p02| sec3|33|33|
| S8| l05| p05| sec4|34|34|
| S8| l05| p05| sec4|34|34|
| S9| l03| p03| sec4|32|26|
| S9| l03| p03| sec4|32|26|
|S10| l09| p08| sec4|33|33|
|S10| l09| p08| sec4|33|33|
|S10| l09| p09| sec4|33|33|
 |S11| l11| p11| sec4|32|26|
 |S11| l12| p12| sec4|32|26|
| S11| l12| p12| sec4|32|26|
| S12| l06| p06| sec5|36|36|
 |S12| l06| p06| sec5|36|36|
 |S12| l06| p06| sec5|36|36|
| S13| l00| p00| sec3|34|34|
 |S13| l00| p00| sec3|34|34|
 |S13| l00| p00| sec3|34|34|
 |S13| l00| p00| sec3|34|34|
 |S14| l01| p01| sec5|32|26|
 |S14| l01| p01| sec5|32|26|
 |S14| l01| p01| sec5|32|26|
 |S14| l01| p01| sec5|32|26|
 |S15| l13| p13| sec3|35|35|
| S15| l14| p14| sec3|35|35|
 |S15| l14| p13| sec3|35|35|
 |S15| l13| p15| sec3|35|35|
 |S16| l16| p17| sec5|32|26|
 |S16| l16| p17| sec5|32|26|
 |S16| l16| p17| sec5|32|26|
 |S16| l16| p17| sec5|32|26|
 |S17| l07| p07| sec4|37|37|
| S17| l07| p07| sec4|37|37|
 |S17| l08| p07| sec4|37|37|
 |S17| l08| p07| sec4|37|37|
 |S18| l01| p01| sec4|32|26|
 |S18| l01| p01| sec4|32|26|
 |S18| l01| p01| sec4|32|26|
 |S18| l01| p01| sec4|32|26|
 |S18| l01| p01| sec4|32|26|

All course-groups of the same subject must be scheduled on a **block** of meeting periods, such that two course-groups either have all their periods in common, or none. Therefore, the problem can be modeled as assigning course-groups to **time clusters**

Students assigned to different course-groups in the same cluster cannot attend both.

## Students



Students must be assigned to course-groups.

Each student follows a fixed **curriculum**, defined as a set of subjects. This selection cannot be modified.

The file `schoolX_curricula.csv` contains:
- the number of students following each curriculum
- the list of subjects in that curriculum

Each row represents a group of identical students. The curricula may have varied lengths and students with shorter curriculum will have shorter schedules.

|num_students|subject_1|subject_2|subject_3|subject_4|subject_5|subject_6|
|------------|---------|---------|---------|---------|---------|---------|
|71|S13|S15|||||
|29|S11|S12|S14|S16|||
|19|S10|S12|S14|S16||
|1|S9|S15|S17|S18|||
|16|S9|S11|S17|S18|||
|31|S9|S10|S17|S18|||
|26|S8|S11|S17|S18|||
|34|S8|S10|S17|S18|||
|33|S6|S7|S13|S15|||
|15|S5|S11|S17|S18|||
|7|S5|S10|S17|S18|||
|33|S4|S7|S13|S15|||
|30|S3|S12|S14|S16|||
|2|S2|S14|S16|S17|||
|31|S2|S12|S14|S16|||
|4|S1|S9|S17|S18|||
|1|S1|S9|S11|S15|S17|S18|
|1|S1|S9|S10|S18|||
|3|S1|S9|S10|S15|S17|S18|
|6|S1|S8|S17|S18|||
|2|S1|S8|S10|S18|||
|1|S1|S5|S17|S18|||
|1|S1|S5|S11|S18|||
|1|S1|S5|S10|S18|||
|1|S1|S5|S10|S15|S17|S18|

The file `schoolX_selection_matrix.csv` contains a symmetric matrix where:

- rows and columns correspond to subjects
- each entry (i, j) is the number of students taking both subjects i and j

This matrix can be derived from the curricula data but is provided for convenience.

## Data Provided

Four instances are available. The data comes from real life data from Canadian high-schools for the 2024-2025 school year. For each school, three files are provided:
 * `shoolX_groups.csv` contains the subject, room, teacher, grade, cap and low for each course-group 
 * `schoolX_curricula.csv` contains on each line the number of student following a given curriculum and the list of subjects forming that curriculum
 * `scholX_selection_matrix.csv` contains the course selection matrix, where a number correspond to the number of student taking both the column course and the line course

## Goal

## Goal

The goal is to:

1. Assign each course-group to a time cluster
2. Assign students to course-groups

Such that:
- no student is assigned to two course-groups in the same cluster
- no teacher is teaching two course-groups in the same cluster
- no room is being used by two course-groups in the same cluster
- capacities (`cap`) are respected
- minimum sizes (`low`) are respected

Objective:
- Balance the number of students across course-groups of the same subject

A subject is perfectly balanced if all its course-groups have the same number of students.


