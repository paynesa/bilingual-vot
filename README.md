# Cross-Linguistic Effects of Voicing Contrasts in Simultaneous Spanish-English Bilinguals

This repository contains the code and data for my LING520 final project at the University 
of Pennsylvania. This project examines whether bilabial and alveolar plosives are 
merged in simultaneous Spanish-English bilinguals by extracting tokens containing these words 
from <a href="https://childes.talkbank.org/">CHILDES</a> and measuring VOT. 
English makes a short-lag/long-lag distinction while Spanish 
makes a negative/short-lag distinction. This subtle difference makes the
phonological categories a prime candidate for merging, but as I demonstrate, 
most children are able to retain the two distinctions separately.

In this repository, I have provided the .wav files and TextGrids corresponding to 
my segmentation of the data for both Spanish (`spa_speech`) and English (`eng_speech`). 
Within each of these folders, I have created separate subdirectories for each of the nine 
speakers, eight of which I used for my final project. The `parsed_transcripts` 
directory contains parsed transcripts from the Deuchar and FerFuLice corpora, and 
a complete version of the CSV files for Spanish and English which contain data that was 
not used in my final project. `cleaned_eng.csv` and `cleaned_spa.csv` both contain the 
cleaned versions of these files that I used for my final project. Finally, 
`data_processing` contains the scripts used to parse CHILDES transcriptions (`parse_childes_dir.py`), 
extract relevant words from the output of the parsing (`get_relevant_words.py`),
create plots separately for each speaker (`make_presentation_plots.R`), and create 
plots and run statistical tests over the entire data, as well as different demographic
groupings of the children (`make_paper_plots.R`). 

Feel free to use the data provided here for your own research! 
For a copy of the paper associated with this project, please email me. 
