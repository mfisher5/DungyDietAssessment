#!/bin/bash

# name variables: directory with fastq files, and the file name to store the read counts
fastq_dir=~/Documents/DungyDietAssessment/data/raw/run_1/
fastq_counts=run1_readcounts.csv

# get a list of the fastq.gz files in the given directory
cd "$fastq_dir"
FILES=$(ls *.fastq.gz)

# count the number of lines in each fastq file and divide by 4, to get read counts.
echo "${fastq_dir}${fastq_counts}"
for F in $FILES; do 
	LINES=$(cat $F|wc -l)
	echo $F "," $(( $LINES / 4 ))
	echo $F "," $(( $LINES / 4 )) >> "${fastq_dir}${fastq_counts}"
done 



# name variables: directory with fastq files, and the file name to store the read counts
fastq_dir=~/Documents/DungyDietAssessment/data/raw/run_2/
fastq_counts=run2_readcounts.csv

# get a list of the fastq.gz files in the given directory
cd "$fastq_dir"
FILES=$(ls *.fastq.gz)

# count the number of lines in each fastq file and divide by 4, to get read counts.
echo "${fastq_dir}${fastq_counts}"
for F in $FILES; do 
	LINES=$(cat $F|wc -l)
	echo $F "," $(( $LINES / 4 ))
	echo $F "," $(( $LINES / 4 )) >> "${fastq_dir}${fastq_counts}"
done 