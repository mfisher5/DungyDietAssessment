#!/usr/bin/env bash

cd /mnt/nfs/home/KellyCEG/blastdb/
PATH=$PATH:/mnt/nfs/home/KellyCEG/ncbi-blast-2.13.0+/bin
BLAST_DB='/mnt/nfs/home/KellyCEG/blastdb/nt'

################################################################################
# Denoised Hashes from DADA2 = All Hashes from DADA2
################################################################################
blast_output='/mnt/nfs/home/KellyCEG/mcf/DungyDietAssessment/run1_hash_key_clean_blast_2023-06-19.fasta'
blast_input='/mnt/nfs/home/KellyCEG/mcf/DungyDietAssessment/run1/Hash_Key_clean.fasta'
####################### BLAST CLUSTERS #######################
echo $(date +%H:%M) "BLASTing..."
/mnt/nfs/home/KellyCEG/ncbi-blast-2.13.0+/bin/blastn -query "${blast_input}" -db "${BLAST_DB}" -num_threads 16 -perc_identity "${PERCENT_IDENTITY}" -word_size "${WORD_SIZE}" -evalue "${EVALUE}" -max_target_seqs "${MAXIMUM_MATCHES}" -culling_limit="${CULLING}" -outfmt "6 qseqid sseqid sacc pident length mismatch gapopen qcovus qstart qend sstart send evalue bitscore staxids qlen sscinames sseq" -out "${blast_output}"

