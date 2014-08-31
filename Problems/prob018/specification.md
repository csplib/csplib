---
Title:    Water Bucket Problem 
Proposer: Toby Walsh
Category: 
    - Bin packing
    - Partitioning and related problems
---


Given the promise of SAT and CSP techniques for solving "classical" planning problems, I decided to propose this puzzle.

You are given an 8 pint bucket of water, and two empty buckets which can contain 5 and 3 pints respectively. You are required to divide the water into two by pouring water between buckets (that is, to end up with 4 pints in the 8 pint bucket, and 4 pints in the 5 pint bucket).

What is the minimum number of transfers of water between buckets? The challenge is to solve this as a planning problem (encoded into satisfiability or constraint satisfaction) with an efficiency approaching (or exceeding) a simple [enumeration](models/enumerate.pl).
