get_unique_taxa <- function(taxa.df, level="all"){
  
  if(level=="all"){
  # get species-level prey for each crab
  spdat <- taxa.df %>% filter(rank=="species") %>% dplyr::select(taxon,genus,family,order,class) %>% distinct() %>%
    pivot_longer(cols=c(genus,family,order,class),names_to="tax.level",values_to="sp.taxonomy") %>%
    rename("sp.taxon"=taxon)
  
  # keep non-specific ids in data set only if that taxon is not represented across the entire dataset
  nonspec_dat <- taxa.df %>% filter(rank!="species") %>% dplyr::select(taxon,rank,species,genus,family,order,class) %>% distinct()
  
  nonspec_dat %<>% pivot_longer(cols=c(genus,family,order,class),names_to="tax.level",values_to="taxonomy") %>%
    filter(tax.level==rank) %>%
    left_join(spdat,by=c("tax.level","taxonomy"="sp.taxonomy")) %>% filter(!is.na(sp.taxon))
  
  dat <- taxa.df %>% anti_join(nonspec_dat,by=c("rank","taxon"="taxonomy"))
  
  } else if(level=="site"){
    spdat <-taxa.df %>% filter(rank=="species") %>% 
      dplyr::select(site,taxon,genus,family,order,class) %>% distinct() %>%
      pivot_longer(cols=c(genus,family,order,class),names_to="tax.level",values_to="sp.taxonomy") %>%
      rename("sp.taxon"=taxon)
    
    nonspec_dat <-taxa.df %>% 
      filter(rank!="species") %>% 
      dplyr::select(site,taxon,rank,species,genus,family,order,class) %>% distinct() %>%
      pivot_longer(cols=c(genus,family,order,class),names_to="tax.level",values_to="taxonomy") %>%
      filter(tax.level==rank) %>%
      left_join(spdat,by=c("site","tax.level","taxonomy"="sp.taxonomy")) %>% filter(!is.na(sp.taxon))
    
   dat <-taxa.df %>%
      anti_join(nonspec_dat,by=c("site","rank","taxon"="taxonomy"))

   
  } else{stop("Please choose level = site or level = all")}
  
  return(dat)
  
}
