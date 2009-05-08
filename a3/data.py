#!/usr/bin/python

import csv
from commands import getoutput as go

scenes = ['scene1']
thresholds = [2**x for x in range(1,15)]
runs = 3

for s in scenes:

    ocsv = csv.writer(open(s+'.csv','wb'))
    ocsv.writerow( ['#threshold'] + ['real']*runs )

    for t in thresholds:
        cmd = 'time -p ./%s 7 %d +RTS -N7'%(s,t)
        print cmd
        row = [t]
        for run in range(runs):
            done = False
            while not done:
                l = go(cmd).split('\n')
                real = l[0].split()[1]
                try:
                    row.append(float(real))
                    done = True
                except ValueError:
                    print l
        ocsv.writerow(row)
