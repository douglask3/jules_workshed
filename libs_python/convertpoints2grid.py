# -*- coding: iso-8859-1 -*-
'''
Crown Copyright 2014, Met Office and licensed under LGPL v3.0 or later: 
https://www.gnu.org/licenses/lgpl.html 

Converts a point-only netcdf file into a grid-based netcdf file using the JULES
python module (which uses IRIS and CDO). This script will only work on points-only files in the same
format as JULES points-only output netcdf files.

Usage: convertpoints2grid.py [-i filename_in | --inputfile=filename_in ] [-o filename_out | --outputfile=filename_out ]

e.g.
python2.7 convertpoints2grid.py -i /data/cr1/kwilliam/JULESPyExampleFiles/wfdei_esm1p0.first_priority_monthly.197901.nc -o $LOCALDATA/out.nc
or
python2.7 convertpoints2grid.py --inputfile=/data/cr1/kwilliam/JULESPyExampleFiles/wfdei_esm1p0.first_priority_monthly.197901.nc --outputfile=$LOCALDATA/out.nc

Karina Williams, Met Office

'''
import getopt
import sys

import jules

def usage():
    return __doc__

class Options:
    ''' stores the input filenames and output filenames that are given on the command line'''
    def __init__(self):
        self.filename_in = None
        self.filename_out = None
    
def parse(argv):
    ''' parse the comand line to get the input and output file names
    '''
    try:
        opts = Options()
        (option_tuple, filenames) = getopt.getopt(argv[1:], "h:i:o:", 
                                   ["help=","inputfile=","outputfile="])
        for (letter, value) in option_tuple:
            if letter in ("-i","--inputfile"):
                opts.filename_in = value
            elif letter in ("-o","--outputfile"):
                opts.filename_out = value
            elif letter in ("-h", "--help"):
                print(usage())
                sys.exit(1)
            else:
                raise UserWarning("unhandled option "+letter)

        if opts.filename_in == None:
            raise UserWarning("no input file given")
        
        if opts.filename_out == None:       
            raise UserWarning("no output file given")
                
        if filenames:
            raise UserWarning("extra filenames on command line")
        return opts
        
    except getopt.GetoptError as err:
        raise UserWarning(err)        

def convert_file(filename_in, filename_out, miss=-1.e+20):
    '''read in a file in land-points-only format, write it out in grid format.
    N.b. requires CDO
    '''

    print("reading from file:")
    print(filename_in)
    
    try: 
        cubelist = jules.load(filename_in, missingdata=miss)
    except:
        raise UserWarning("This simple script has failed to read in the file. "+
            "Probably you need to write a python script that calls jules.load yourself "+
            "so you can play around with the options or pass it lons and lats from another file.")
       
    print("writing to file:")
    print(filename_out)
    
    jules.save(cubelist, filename_out, landpointsonly=False, missingdata=miss )


def main():   
    
    command_line_opts = parse(sys.argv)
    
    convert_file(command_line_opts.filename_in, command_line_opts.filename_out)    

if __name__ == '__main__':

    try:
        main()   
        sys.exit(0)
    except OSError as ex:
        print >> sys.stderr, "Execution failed with OSError:", ex
        sys.exit(1)
    except UserWarning as ex:
        print(usage())   
        print >> sys.stderr, "Execution failed with UserWarning:", ex 
        sys.exit(1)

       