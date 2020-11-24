"""This script processes all files produced by a 2-year-old in a given directory
and outputs the results to a text file containing only the child's productions"""

import os
import argparse

def process(dir : str, out_file : str):
    """Process the data in the input directory"""
    for file in os.listdir(dir):
        # check that the age of the child is valid
        if (file[1] == '2'):
            for line in open(dir +"/"+ file, "r"):
                # check that the speech was produced by a child
                if line[0:4] == "*CHI":
                    with open(out_file, "a") as f:
                        f.write(line)

def main():
    """The main functionality of the program"""
    parser = argparse.ArgumentParser()
    parser.add_argument("directory", type=str, help="The directory to process")
    parser.add_argument("outfile", type=str, help="The file to output the processed data to")
    args = parser.parse_args()
    print(f"Reading in speech from {args.directory}...")
    process(args.directory, args.outfile)
    print(f"Data output to {args.outfile}")

if __name__ == "__main__":
    main()


