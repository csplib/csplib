from subprocess import check_output
import math
import sys
import os
from random import randint
import random

def main():
    no_jobs = 4
    no_mach = 3
    no_tasks = [2,4]
    max_e_mach = 3
    proc = [50,90]
    tasks = []
    start = 1
    for i in range(0,no_jobs):
        end = start + randint(no_tasks[0],no_tasks[1])
        tasks.append([start,end])
        start = end + 1
    optts = []
    upper_limit = end
    start = 1
    for i in range(0,upper_limit):
        end = start + randint(1,max_e_mach-1)
        optts.append([start,end])
        start = end + 1
    optt_mach = []  
    for i in optts: 
        optt_mach.append(random.sample(range(1,no_mach+1), i[1]-i[0]+1))
    optt_dur = []
    for i in range(0,optts[-1][1]):
        optt_dur.append(randint(proc[0],proc[1]))

    s_no_mach =  'no_mach = ' + str(no_mach) + ';\n'
    s_no_jobs  = 'no_jobs = ' + str(no_jobs) + ';\n'
    s_no_task  = 'no_task = ' + str(tasks[-1][1]) + ';\n'
    s_no_optt =  'no_optt = ' + str(optts[-1][1]) + ';\n'
    s_tasks =    'tasks = ['
    for t in tasks:
        s_tasks += str(t[0]) + '..' + str(t[1]) + ', '
    s_tasks = s_tasks[:-2] + '];\n'

    s_optts = 'optts = ['
    for o in optts: 
        s_optts += str(o[0]) + '..' + str(o[1]) + ', '
    s_optts = s_optts[:-2] + '];\n'
    s_optt_mach = 'optt_mach = ['
    
    for m in optt_mach:
        m.sort()
        for q in m:
            s_optt_mach += str(q) + ', '
    s_optt_mach = s_optt_mach[:-2] + '];\n'
    s_optt_dur = 'optt_dur = ['
    for o in optt_dur: 
        s_optt_dur += str(o) + ', '
    s_optt_dur = s_optt_dur[:-2] + '];\n'
    
    file_name = 'dh_' + str(no_mach) + '_' + str(tasks[-1][1]) + '.dzn'
    new_dzn = open(file_name,'w')
       
    new_dzn.write(s_no_mach)
    new_dzn.write(s_no_jobs)
    new_dzn.write(s_no_task)
    new_dzn.write(s_no_optt)
    new_dzn.write(s_tasks)
    new_dzn.write(s_optts)
    new_dzn.write(s_optt_mach)
    new_dzn.write(s_optt_dur)
if __name__ == "__main__":
    main()
