# borrowed from Moncho. For use in 3_denoise_dada2

tibble_to_matrix <- function (tb) {
  
  tb %>% 
    group_by(Sample_name, sample_id, Hash) %>% 
    summarise(nReads = sum(Normalized.reads)) %>% 
    spread ( key = "Hash", value = "nReads", fill = 0) -> matrix_1
  samples <- pull (matrix_1, Sample_name)
  matrix_1 %>% 
    ungroup() %>% 
    dplyr::select ( - Sample_name) -> matrix_1
  data.matrix(matrix_1) -> matrix_1
  dimnames(matrix_1)[[1]] <- samples
  vegdist(matrix_1) -> matrix_1
}