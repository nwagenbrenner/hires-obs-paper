#!/usr/bin/python

import sys

def Usage():
    print '\n    parseScintechSodar.py file'
    print '\n    file: radar file to parse'
    print '\n    Parses Scintech sodar profile data from WSU into a csv file.\n'
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

fout.write('datetime,HT,SPD,DIR,W,SIGW,BCK,ERROR\n')  

line=fin.readline()
i=1
while line != '# beginning of data block\r\n':
    line=fin.readline()
    i=i+1

for line in fin:  
    if line.strip()== '# beginning of data block':
        continue
    elif line.strip() == '#':
        continue
    elif line.strip() == '':
        continue
    elif line[:2]=='20':
        datetime=line[5:7]+'/'+line[8:10]+'/'+line[0:4]+' '+line[11:13]+':'+line[14:16]
    elif line.strip()[:5]=='#   z':
        continue
    else:
        line = line.strip()
        line = line.replace(" ", ",")
        line = line.replace(",,", ",")
        line = line.replace(",,", ",")
        line = line.replace(",,", ",")
        fout.write(datetime + ',' + line + "\n")

fin.close()
fout.close()
    
