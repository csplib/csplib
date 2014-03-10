With the above heuristics a FD constraint program works efficiently to find one solution (e.g. N = 1000).
The Adaptive Search (a local search method) can solve even larger instances very efficiently (e.g. N = 3000).

Even with the above redundant constraints, there are a lot of solutions (so finding all solutions is challenging). Here are some results (with a CLP(FD) program written in GNU Prolog on a quad core 2.8Ghz + Linux):


Instance | Number of Solutions   | Time taken
-------- | --------------------- | -----------
N=8      |    1                  | 0.00 secs = 0ms
N=12     |    1                  | 0.00 secs = 0ms
N=16     |    7                  | 0.01 secs = 10ms
N=20     |    24                 | 0.01 secs = 10ms
N=24     |    296                | 0.03 secs = 30ms
N=28     |    1443               | 0.35 secs = 350ms
N=32     |    17444              | 3.51 secs = 3s 510ms
N=36     |    138905             | 35.86 secs = 35s 860ms
N=40     |    1581207            | 385.07 secs = 6m 25s 70ms
N=44     |    14762400           | 4222.02 secs = 1h 10m 22s 20ms
N=48     |    176977514          | 48276.96 secs = 13h 24m 36s 960ms
N=52     |    1850331835         | 552017.03 secs = 6d 9h 20m 17s 30ms