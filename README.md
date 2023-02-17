# Lattes_collector
This script extract the next information from the XML exported files from Curriculo Lattes:
* List of articles 
* List of book chapters 
* List of books 
* List of in-press article
* List of current and formed phD, master and IC students

The Qualis layers and scores is stimated for each articles, included in-press. 

# How to use

To get started, download the curriculums in XLM format and save them in the same folder as the script. Next, download the specific Qualis for the area you're interested in. Once you have all the necessary files, run the script and specify the path to the Qualis file. The script will then create a series of TSV files with information for each professor and ten dataframes with integrated information extracted from the files.
