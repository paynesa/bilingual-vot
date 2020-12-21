""" Written by Sarah Brogden Payne, Novemeber 2020 
Processes all files produced by a 2-year-old in a given directory
and outputs the results to a text file containing only the child's productions"""

import os
import argparse

def process(dir : str, out_file : str, speakers: int):
    """Process the data in the input directory"""
    for file in os.listdir(dir):
        # check that the age of the child is valid
        if (file[1] == '2'):
            for line in open(dir +"/"+ file, "r"):
                # check that the speech was produced by a child
                if line[0:4] == "*CHI":
                    if (speakers == 1):
                        with open(out_file, "a") as f:
                            f.write(f"{line.strip()}\t{file}\n")
                    else:
                        file_to_open = out_file+"_"+line[4] if line[4] != ":" else out_file+"_1"
                        with open(file_to_open, "a") as f:
                            f.write(f"{line.strip()}\t{file}\n")

def main():
    """The main functionality of the program"""
    parser = argparse.ArgumentParser()
    # we include arguments for the directory of data to parse, the file to output to, and the number of speakers (if there are multiple)
    parser.add_argument("directory", type=str, help="The directory to process")
    parser.add_argument("outfile", type=str, help="The file to output the processed data to")
    parser.add_argument("-num_speakers", type=int, help="The number of children in the data", default=1)
    args = parser.parse_args()
    print(f"Reading in speech from {args.directory}...")
    process(args.directory, args.outfile, args.num_speakers)
    print(f"Data output to {args.outfile}")

if __name__ == "__main__":
    main()


