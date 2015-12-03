---
Title:    Balanced Nursing Workload Problem
Proposer: Gilles Pesant
Category: 
  - Scheduling and related problems
  - Bin packing
---

Given a set of patients distributed in a number of hospital zones and an available nursing staff, one must assign a nurse to each patient in such a way that the work is distributed evenly between nurses. Each patient is assigned an acuity level corresponding to the amount of care he requires; the workload of a nurse is defined as the sum of the acuities of the patients he cares for. A nurse can only work in one zone and there are retrictions both on the number of patients assigned to a nurse and on the corresponding workload. We balance the workloads by minimizing their standard deviation.

This problem can be decomposed in two phases: *nurse staffing* that assigns nurses to zones and *nurse-patient assignment* that then assigns patients to nurses.

A variant of this problem has patients grouped into a small number of types and has the acuity associated with each patient type being nurse-dependent. Note that the total workload is no longer known a priori since each patient's acuity now depends on which nurse he is assigned to. There are two objectives to minimize: the total workload and the standard deviation of the workloads. 
