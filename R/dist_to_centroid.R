# borrowed from Moncho. for use in 3_denoise_dada2

dist_to_centroid <- function (x,y) {
  biol <- rep(y, length(x))
  
  if (length(biol) == 1) {
    output = rep(x[1]/2,2)
    names(output) <- attr(x, "Labels")
  }else{ 
    
    dispersion <- betadisper(x, group = biol)
    output = dispersion$distances
  }
  output
}