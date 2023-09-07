############ Transform GGplot axis ############
#
# Function: published by Huanfa Chen (UCL CASA) https://rpubs.com/huanfaChen/squash_remove_y_axix_ggplot#:~:text=In%20ggplot%3A%20squash%2Fremove%20part%20of%20y%2Daxis&text=If%20one%20wants%20to%20squash,use%20the%20facet_grid()%20function.
#
#
###############################################

squish_trans <- function(from, to, factor) {
  require(scales)
  
  trans <- function(x) {
    if (any(is.na(x))) return(x)
    
    # get indices for the relevant regions
    isq <- x > from & x < to
    ito <- x >= to
    
    # apply transformation
    x[isq] <- from + (x[isq] - from)/factor
    x[ito] <- from + (to - from)/factor + (x[ito] - to)
    
    return(x)
  }
  
  inv <- function(x) {
    
    if (any(is.na(x))) return(x)
    
    # get indices for the relevant regions
    isq <- x > from & x < from + (to - from)/factor
    ito <- x >= from + (to - from)/factor
    
    # apply transformation
    x[isq] <- from + (x[isq] - from) * factor
    x[ito] <- to + (x[ito] - (from + (to - from)/factor))
    
    return(x)
  }
  
  # return the transformation
  return(trans_new("squished", trans, inv))
}