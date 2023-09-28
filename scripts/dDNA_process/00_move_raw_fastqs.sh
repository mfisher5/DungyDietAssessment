#!/usr/bin/env bash

# this is the 
DAT_DIR="C:/Users/mfisher5/Documents/DungyDietAssessment/data/raw"
echo "This is the destination for the data files"
echo "${DAT_DIR}"

# navigate to folder
cd ~/Documents/dDNA_data/S-22-1085_Miseq_KCWYV-363579216/FASTQ_Generation_2022-08-29_22_08_57Z-602176724
echo "This is the working directory"
pwd

# loop through all subdirectories copy the files into the directory, copy files over
for d in ./*;do 
cp -R $d/. $DAT_DIR/; 
done


