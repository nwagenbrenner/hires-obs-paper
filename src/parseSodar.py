#!/usr/bin/python

import sys

def Usage():
    print '\n    parseSodar.py file'
    print '\n    file: sodar file to parse'
    print '\n    Parses ASC sodar data from NOAA (e.g., GRI) into a csv file.\n'
    sys.exit(0)

f = None

#=======================================
#   Parse command line options.
#=======================================
if __name__ == '__main__':
    argv = sys.argv
    if argv is None:
        sys.exit(0)   

    i = 1

    while i < len(argv):
        arg = argv[i]
        if f is None:
            f = argv[i] 
        else:
            Usage()
        i = i + 1

    if len(argv) < 2:
        print "\n    Not enough args..."
        Usage()

#=======================================
#   Parse the file.
#=======================================
fin=open(f, 'r')
fout=open(f[f.rfind('/')+1:-3] + 'csv', 'w')

fout.write('HT,SPD,DIR,W,SDW,IW,GPSD,GDIR,U,SDU,NU,IU,SNRU,V,SDV,NV,IV,SNRV,NW,SNRW\n')  

for line in fin:
    if line[0:8] == 'NOAAGRD3':
        datetime = line[32:48]
        for i in range(0, 3):
            fin.next()
    else:
        line = line.strip()
        line = line.replace(" ", ",")
        line = line.replace(",,", ",")
        line = line.replace(",,", ",")
        fout.write(datetime + "," + line + "\n")

fin.close()
fout.close()
    
