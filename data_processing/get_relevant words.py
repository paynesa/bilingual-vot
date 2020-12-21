"""Written by Sarah Brodgen Payne, November 2020
Given the text file of the child's input (output from 
parse_childes_dir.py, find all the words in the target 
position"""

import os
# iterate through the parsed files line by line 
for file in os.listdir("../parsed_transcripts"):
    for line in open("../parsed_transcripts/"+file):
        speaker, transcript, filename = line.strip().split("\t")
        # get the first word in the utterance and check if it starts with one of the plosives and is followed by a vowel
        utterance_initial = transcript.split()[0]
        if (utterance_initial[0] in "qtpdgkcb" and utterance_initial[1] in "aeiou"):
            print(f"{file}\t{filename}\t{utterance_initial}\t{transcript}")
