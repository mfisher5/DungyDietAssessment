# borrowed from Moncho. For use in 3_denoise_dada2

how.many <- function(ASVtable, round){
  ASVtable %>% ungroup() %>% 
    summarise(nsamples = n_distinct(Sample_name),  ## changed from n_distinct(sample) | MCF 10/7
              nHashes = n_distinct(Hash),
              nReads = sum(nReads), 
              Stage = paste0("Step_", round)) %>% 
    gather(starts_with("n"), value = "number", key = "Stat")
}
