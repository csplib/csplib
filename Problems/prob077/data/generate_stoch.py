#import matplotlib.pyplot as plt  
from subprocess import check_output
import math
import sys
import os
from random import randint
import random

def main():
    nbScen = int(sys.argv[2])
    original_file = sys.argv[1]
    #creat folder and file handles
    target_folder = original_file.replace('original_data/','').replace('.dzn','') + '_' + str(nbScen) + '/'
    print target_folder
    command = 'mkdir -p processed_data/' + target_folder
    tmp = check_output(command, shell=True)
    command = 'mkdir -p processed_data/' + target_folder + '/fzn'
    tmp = check_output(command, shell=True)
    command = 'mkdir -p processed_data/' + target_folder + '/data' 
    tmp = check_output(command, shell=True)
    command = 'mkdir -p processed_data/' + target_folder + '/results'
    tmp = check_output(command, shell=True)
    file_name = 'processed_data/' + target_folder + 'data/' + sys.argv[1].split('/')[-1].replace('.dzn','_stoch.dzn')

    reduction_factor = 4
    stoch_dzn = open(file_name,'w')
    new_dzn = []
    for i in range(0,nbScen/reduction_factor): 
        new_dzn.append(open(file_name.replace('_stoch.dzn','_' + str(i+1) + '.dzn'),'w' ))
    #read original file
    of = open(original_file,'r')
    lines = of.readlines()
    of.close()
    dur = []
    mach = []

    optts = []
    durA = True
    for l in lines:
        if 'no_mach' in l:
            no_mach = int(l.split(' = ')[1].replace(';',''))
        if 'optt_dur = [' in l:
            dur.append(l)
        else:
            stoch_dzn.write(l)
            for n in new_dzn:
                n.write(l);
        if 'optt_mach' in l:
            mach.append(l)
        if 'optts' in l:
            optts.append(l)
    stoch_dzn.write('nbScenarios = ' + str(nbScen)+';\n')
    for n in new_dzn: 
        n.write('nbScenarios = ' + str(nbScen)+';\n')
    weights = []
    for i in range(0,nbScen):
        weights.append(1)
    for idx,n in enumerate(new_dzn):
        n.write('first_scen = ' + str(idx * reduction_factor + 1) + ';\n')
        n.write('last_scen = ' + str((idx + 1) * reduction_factor) + ';\n')
        n.write('weights = ' + str(weights) +';\n')
    stoch_dzn.write('weights = ' + str(weights) +';\n')
    
    start_dur = dur[0].index('[')+len('[')
    end_dur = dur[0].index(']',start_dur)
    start_mach = mach[0].index('[')+len('[')
    end_mach = mach[0].index(']',start_mach)
    start_optts = optts[0].index('[')+len('[')
    end_optts = optts[0].index(']',start_optts)
    #print dur[0][start_dur:end_dur]
    orig_dur = []
    for v in dur[0][start_dur:end_dur].split(','):
        orig_dur.append(int(v))
    orig_mach = []
    for v in mach[0][start_mach:end_mach].split(','):
        orig_mach.append(int(v))
    orig_optts = []
    for v in optts[0][start_optts:end_optts].split(','):
        #orig_optts.append(v)
        tmp = []
        if '{' in v: 
            for i in v.replace('{','').replace('}','').split(','):
                tmp.append(int(i))
        if '..' in v: 
            for i in v.split('..'):
                tmp.append(int(i))
        orig_optts.append(tmp)
    
    [optt_dur,optt_dur_scen] = create_scenarios_random(orig_dur,orig_mach,nbScen,orig_optts,no_mach)
    for n in new_dzn: 
        n.write(optt_dur + '\n')
        n.close()
    stoch_dzn.write(optt_dur + '\n')
    stoch_dzn.close()
#The first scenario is the original one

def create_scenarios_random(orig_dur,mach,nbScen,optts,no_mach):
    output_scen = []
    disturbance = 0
    tmp = []
    for idx in range(0,nbScen):
        for o in orig_dur: 
            while(disturbance < 0.9 or disturbance > 2):
                disturbance = random.gauss(1.5, 0.3)
            tmp.append(int(o*disturbance))
            disturbance = 0
        output_scen.append(tmp)
        tmp = []
    return create_output(output_scen) 


def create_output(scen):
    scenarios = []
    de = 'optt_dur = [|'
    s = ''
    for i in scen: 
        for n in i:
            s += str(n) + ', '
            de += str(n) + ', '
        de = de[:-2]+'|\n'
        scenarios.append(s[:-2])
        s = ''
    de = de[:-1] + '];'
    return [de,scenarios]

if __name__ == "__main__":
        main()
