
import sys

file = open(sys.argv[1])
BEGIN="<soapenv:Envelope"
END="</soapenv:Envelope>"
BODY="<soapenv:Body>"

counter = 0
count = False

block = ""
count = 0
start = False
follow_name = False

def get_name(line):
    line = line.strip()
    return line[1:-1].replace(':', '_')

def write_in_file(count, fname, content):
    fname = str(count) + "_" + fname + ".xml"    
    new_file = open(fname, "w+")
    new_file.write(content)
    new_file.close()
    
for line in file:
    if BEGIN in line:
        start = True
    if follow_name == True:
        name = get_name(line)
        follow_name = False
    if start == True:
        if BODY in line:
            follow_name = True        
        block += line
                
    if END in line:
        write_in_file(count, name, block)        
        count += 1        
        block = ""
        start = False
        
file.close()
