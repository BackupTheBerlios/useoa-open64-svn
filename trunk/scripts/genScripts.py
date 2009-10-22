#! /usr/bin/env python
#################################################################
# createRegScripts.py
#
# usage: createRegScripts.py file.test
#
# Creates two regression scripts for the regression tests 
# specified in file.test.  A gen script is created to generate
# the output from specified test inputs.  A regress script is 
# created to run the test driver on the same inputs and do
# a diff with the previously generated output.
#
# The gen script will be named
#   gentestname.script
# The regression script will be named
#   regresstestname.script
#
# In the scripts that are generated...
# The input files are expected to be in inputdir
# where inputdir is one of the pieces of information provided
# in file.test.  The output files will go to outputdir
# and will be named infile.out.  The driver will be called
# with the string specified with driverexec.
#
# Any lines in file.test starting with a pound sign will be 
# ignored as comments.
#
# There is one comma after the input file name which is used
# to separate the file name from the status.  Avoid using
# a comma within the status.
#
##### Example file
#
#   # comment in file.test
#   # Regression tests for MemRefExpr generation
#   testname::MemRefExpr
#   inputdir::TestCases/MemRefExpr
#   outputdir::TestResults/MemRefExpr
#   driverexec::./test-open64 --oa-MPI-ICFG
#   
#   # list of tests
#   #       input file  options      status
#   test::  arrays1.B,  --ccmax 2,   error
#   test::  arrays5.B,               checked by MMS
#   
#################################################################
import sys
import string
import glob

############# just read in all lines, these files aren't that big

# read fields file
if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
    print "The .test filename must be specified\n"
    sys.exit(0)

file = open(filename)
filelines = file.readlines()

##### remove comment lines and blank lines
def notcomment(line):
    if (line[0] == '#') or (string.strip(line) == ''): return 0
    else: return 1
   
filelines = filter(notcomment,filelines)

##### testname keyword
for line in filelines:
    if string.find(line,"testname::") != -1:
        testname = string.strip(string.split(line,"::")[1])

##### inputdir keyword
for line in filelines:
    if string.find(line,"inputdir::") != -1:
        inputdir = string.strip(string.split(line,"::")[1])

##### outputdir keyword
for line in filelines:
    if string.find(line,"outputdir::") != -1:
        outputdir = string.strip(string.split(line,"::")[1])

##### driverexec keyword
for line in filelines:
    if string.find(line,"driverexec::") != -1:
        driverexec = string.strip(string.split(line,"::")[1])

# files for the scripts being generated
genfilename = "gen" + testname + ".script"
genfile = open(genfilename,"w")
regfilename = "regress" + testname + ".script"
regfile = open(regfilename,"w")


##### test keyword
for line in filelines:
    if string.find(line,"test::") != -1:
        # first take off 'test::' then take off inputfile,
        tempstr = string.strip(string.split(line,"::")[1])
        commalist = string.split(tempstr,',')
        inputfile = string.strip(commalist[0])
        outputfile = inputfile
        driverexecfinal = driverexec
        lenCommalist = len(commalist)
        # status is last in commaist
        status = string.strip(commalist[lenCommalist - 1])
        # options are between inputfile and status
        for opNum in range(1,lenCommalist-1):
            opstr = string.strip(commalist[opNum])
            driverexecfinal = driverexecfinal + " " + opstr
            opstr_ = string.replace(opstr," ","-")
            outputfile = outputfile + opstr_
        
        genfile.write("echo \"Generating %s/%s\"\n" \
            % (outputdir,outputfile+".out"))
        ### uncomment this to regenerate B files ################
	inputName=inputfile[:-1]
	fFiles=glob.glob("../"+inputdir+"/"+inputName+"f")
	fFiles=fFiles+glob.glob("../"+inputdir+"/"+inputName+"f90")
        if (len(fFiles) > 1): 
	  print "ambiguous match for "+inputdir+"/"+inputName+"*"
          print fFiles
          sys.exit(-1)
        if (len(fFiles) < 1): 
	  print "no match for "+inputdir+"/"+inputName+"*"
          print fFiles
          sys.exit(-1)
        justfile = inputfile
        if string.find(justfile,"/") != -1:
            # take off subdirs
            parts = string.split(justfile,"/")
            numparts = len(parts)
            justfile = parts[numparts-1]
            #print "justfile is "+justfile
	genfile.write("${OPEN64TARG}/crayf90/sgi/mfef90 -z -F -N132 %s\n" % (fFiles[0][3:]))
	genfile.write("mv -f %s %s/%s\n" % (justfile, inputdir,inputfile))
	regfile.write("${OPEN64TARG}/crayf90/sgi/mfef90 -z -F -N132 %s\n" % (fFiles[0][3:]))
	regfile.write("mv -f %s %s/%s\n" % (inputfile, inputdir,inputfile))
        ### until here ##########################################
        genfile.write("%s %s/%s > %s/%s\n"  \
            % (driverexecfinal,inputdir,inputfile,outputdir,outputfile+".out"))
        regfile.write("echo \"Testing %s/%s, %s/%s %s\"\n" \
            % (inputdir,inputfile,outputdir,outputfile+".out", status))
        regfile.write("%s %s/%s > t\n" \
            % (driverexecfinal,inputdir,inputfile))
        regfile.write("diff -I \"LOC 0 0 source files:\" t %s/%s\n\n" \
            % (outputdir,outputfile+".out"))

# output
print "generated scripts: " + genfilename + " and " + regfilename
