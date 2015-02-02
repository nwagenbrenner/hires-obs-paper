#!/usr/bin/python

import sys

def Usage():
    print '\n    parseRadar.py file'
    print '\n    file: radar file to parse'
    print '\n    Parses speed and dir radar profile data from NOAA (e.g., from Sunset Lake) into a csv file.\n'
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

fout.write('datetime,HT,SPD,DIR,RAD,RAD,RAD,CNT,CNT,CNT,SNR,SNR,SNR\n')  

for line in fin:  
    if line.strip() == '$':
        continue
    elif line[1:5] == 'NOAA':
        for i in range(0, 2):
            fin.next()
    elif line.strip() == '':
        continue
    elif line.strip()[-3:]=='420':
        datetime=line[5:7]+'/'+line[8:10]+'/20'+line[2:4]+' '+line[11:13]+':'+line[14:16]
        for i in range(0, 6):
            fin.next()
    else:
        line = line.strip()
        line = line.replace(" ", ",")
        line = line.replace(",,,,,", ",")
        line = line.replace(",,", ",")
        line = line.replace(",,", ",")
        fout.write(datetime + "," + line + "\n")

fin.close()
fout.close()
    
