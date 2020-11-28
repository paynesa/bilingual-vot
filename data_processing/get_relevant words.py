import os

for file in os.listdir("../child_speech"):
    for line in open("../child_speech/"+file):
        speaker, transcript, filename = line.strip().split("\t")
        utterance_initial = transcript.split()[0]
        if (utterance_initial[0] in "qtpdgkcb"):
            print(f"{file}\t{filename}\t{utterance_initial}\t{transcript}")