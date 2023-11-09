########################## sampling map ###########################

library(here)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(ggspatial)
library(ggrepel)
library(janitor)
library(scales)
library(ggfortify)

## lab metadata (CH,CW,fullness) ##
dat <- read_csv(here('data','metadata','sampling_data.csv'))

# basemap
base.map <- readRDS(here('data','metadata',"PugetSoundN_terrain_background.rds"))
wa.map <- map_data("state") %>%
  filter(region=="washington")


# colors for map points

estuary_colors <- data.frame(estuary=c("Port Susan Bay","Padilla Bay", "Samish Bay"),
                             key=c("#33a02c","#1f78b4","chocolate3"))

site_names <- data.frame(site_code=c("KAY","KAY-shell",
                                "MARPT","PBNERR",
                                "SAMI","SAMT","LAR"),
                         site=c("Kayak Pt 1","Kayak Pt 2",
                                     "March Pt","NERR",
                                     "Samish Isl","Samish Bay","Larrabee"),
                         site_name=c("Kayak Point","Kayak Point",
                                     "March Point","NERR",
                                     "Samish Island","Oyster Creek","Larrabee"),
                         key=c("darkgreen","chartreuse3",
                               "#1f78b4","blue",
                               "chocolate4","chocolate2","darksalmon"))

dat %<>% mutate(site=ifelse(site%in%c("NERR 1","NERR 2"),"NERR",site)) %>% left_join(site_names,by=c("site"))

plotdat <- dat %>% filter(site != "NERR 3") %>% filter(site != "Kayak Pt 2")

ggmap(base.map) +
  geom_point(data=plotdat, aes(x=Longitude,y=Latitude, color=fct_relevel(Bay,estuary_colors$estuary)), stroke=c(3,1,3,1,3,1,3), size=4, pch=1) +
  # order of labels: kayak pt, march pt, 
  #geom_label_repel(data=plotdat, aes(x=Longitude,y=Latitude, label=site_name,segment.colour=fct_relevel(Bay,estuary_colors$estuary)),
  #                force=1, force_pull=0.25, segment.size=1, size=4, min.segment.length=0.1, show.legend=FALSE, nudge_x=0.4, nudge_y=c(0.05,-0.12,0,0,-0.1,0,0.05)) +
  geom_text(aes(x=-122.48, y=48.58,label="Samish\n Bay"), color="chocolate4", fontface='italic', inherit.aes=FALSE) +
  geom_text(aes(x=-122.515, y=48.46,label="Padilla\n Bay"), color="#1f78b4", fontface='italic', inherit.aes=FALSE) +
  geom_text(aes(x=-122.42, y=48.17,label="Port\n Susan\n Bay"), color="darkgreen", fontface='italic', inherit.aes=FALSE) +
  ylab("Latitude") + xlab("Longitude") +
  scale_color_manual(aesthetics=c("color","segment.color"), values=estuary_colors$key, name="Estuary") +
  theme(axis.title=element_text(size=14),
        axis.text=element_text(size=12),
        legend.title=element_blank(),
        legend.text=element_text(size=12), legend.position="none") +
  coord_cartesian(xlim = c(-122.8, -122.1), ylim = c(47.85, 48.75))


png(filename=here('figs','defense_ch1_map_labeled.png'),res=300, height=2000,width=1500)
ggmap(base.map) +
  geom_point(data=plotdat, aes(x=Longitude,y=Latitude, color=fct_relevel(Bay,estuary_colors$estuary)), stroke=c(3,1,3,1,3,1,3), size=4, pch=1) +
  # order of labels: kayak pt, march pt, .... larrabee
  geom_label_repel(data=plotdat, aes(x=Longitude,y=Latitude, label=site_name,segment.colour=fct_relevel(Bay,estuary_colors$estuary)),
                  force=1, force_pull=0.25, segment.size=1, size=4, min.segment.length=0.1, show.legend=FALSE, nudge_x=0.4, nudge_y=c(0.05,-0.12,0,0,-0.1,0,0.05)) +
  geom_text(aes(x=-122.48, y=48.58,label="Samish\n Bay"), color="chocolate4", fontface='italic', inherit.aes=FALSE) +
  geom_text(aes(x=-122.515, y=48.46,label="Padilla\n Bay"), color="#1f78b4", fontface='italic', inherit.aes=FALSE) +
  geom_text(aes(x=-122.42, y=48.17,label="Port\n Susan\n Bay"), color="darkgreen", fontface='italic', inherit.aes=FALSE) +
  ylab("Latitude") + xlab("Longitude") +
  scale_color_manual(aesthetics=c("color","segment.color"), values=estuary_colors$key, name="Estuary") +
  theme(axis.title=element_text(size=14),
        axis.text=element_text(size=12),
        legend.title=element_blank(),
        legend.text=element_text(size=12), legend.position="none") +
  coord_cartesian(xlim = c(-122.8, -122.1), ylim = c(47.85, 48.75))
dev.off()


png(filename=here('figs','defense_ch1_map.png'),res=300, height=2000,width=1500)
ggmap(base.map) +
  geom_point(data=plotdat, aes(x=Longitude,y=Latitude, color=fct_relevel(Bay,estuary_colors$estuary)), stroke=c(3,1,3,1,3,1,3), size=4, pch=1) +
    geom_text(aes(x=-122.48, y=48.58,label="Samish\n Bay"), color="chocolate4", fontface='italic', inherit.aes=FALSE) +
  geom_text(aes(x=-122.515, y=48.46,label="Padilla\n Bay"), color="#1f78b4", fontface='italic', inherit.aes=FALSE) +
  geom_text(aes(x=-122.42, y=48.17,label="Port\n Susan\n Bay"), color="darkgreen", fontface='italic', inherit.aes=FALSE) +
  ylab("Latitude") + xlab("Longitude") +
  scale_color_manual(aesthetics=c("color","segment.color"), values=estuary_colors$key, name="Estuary") +
  theme(axis.title=element_text(size=14),
        axis.text=element_text(size=12),
        legend.title=element_blank(),
        legend.text=element_text(size=12), legend.position="none") +
  coord_cartesian(xlim = c(-122.8, -122.1), ylim = c(47.85, 48.75))
dev.off()









########################## frequency of occurrence ###########################

library(here)
library(tidyverse)
library(magrittr)
library(ggplot2)
library(ggrepel)
library(janitor)
library(scales)
library(ggfortify)
source(here('R','squash_axis.r'))


blastdir   <- 'data/blast'
resultsdir <- 'data/results'
run.nums <- c(1,2)
marker  <- 'lerayXT'


## filtered taxa ##
dat <- read_csv(here(blastdir, paste0(marker,"_","r",paste(run.nums,collapse="-"),"_sample_taxonomy_filtered2_uniqueTaxa.csv")))

dat %<>%
  mutate(site=ifelse(site=="KAY" & crab_num > 18, "KAY-shell",site)) %>%
  mutate(site=ifelse(site=="CLAY","LAR",
                     ifelse(site=="SIN","SAMI",site))) %>%
  mutate(estuary=ifelse(site %in% c("KAY","KAY-shell"),"Port Susan Bay",
                        ifelse(site %in% c("LAR","SAMT","SAMI"), "Samish Bay","Padilla Bay")))

## too small taxa (aka "detritus") ##
smdat <- read_csv(here(blastdir, paste0(marker,"_","r",paste(run.nums,collapse="-"),"_sample_taxonomy_SmallTaxa_uniqueTaxa.csv")))  

smdat %<>%
  mutate(site=ifelse(site=="KAY" & crab_num > 18, "KAY-shell",site)) %>%
  mutate(site=ifelse(site=="CLAY","LAR",
                     ifelse(site=="SIN","SAMI",site))) %>%
  mutate(estuary=ifelse(site %in% c("KAY","KAY-shell"),"Port Susan Bay",
                        ifelse(site %in% c("LAR","SAMT","SAMI"), "Samish Bay","Padilla Bay")))




## Calculate frequency of occurrence of each taxon ##
total.crab <- length(unique(dat$crab_id))

# multi-cellular
plotdat.fo <- dat %>% mutate(plot.group=ifelse(is.na(phylum), class, phylum)) %>%
  group_by(taxon,plot.group) %>%
  summarise(ncrab=length(unique(crab_id))) %>%
  mutate(pcrab=(ncrab/total.crab)*100) %>%
  arrange(plot.group)

# detritus groups
fo.sm <- smdat %>% mutate(plot.group="single-celled organisms") %>%
  filter(crab_id %in% dat$crab_id) %>%
  group_by(taxon,plot.group) %>%
  summarise(ncrab=length(unique(crab_id))) %>%
  mutate(pcrab=(ncrab/total.crab)*100) %>%
  arrange(plot.group)

## prepare for plotting ##
# combine data, order groups
plotdat.fo %<>% bind_rows(fo.sm)

plotdat.fo %<>% left_join(data.frame(plot.group=unique(plotdat.fo$plot.group),
                                     ngroup=order(unique(plotdat.fo$plot.group))),by="plot.group") %>%
  unite(plot.group, ngroup, col="plot.group.num",sep="-", remove=FALSE)


# save groups into dataframe and replace with common names
plot.group.names <- data.frame(plot.group=unique(plotdat.fo$plot.group))
plot.group.names %<>% mutate(plot.group.name=c("Annelid worms","Arthropods","Bryozoans","Green algae",
                                               "Fish & Tunicates","Jellyfishes & Hydrozoans","Sea cucumber",
                                               "Molluscs", "Nematodes","Nemertean worms","Brown algae","Flatworms",
                                               "Sponges","Red algae","single-celled organisms"))


plotdat.fo %<>% left_join(plot.group.names)

# order by prey taxonomic diversity within the given group (high to low)
plot.group.name.order1 <- c("Arthropods","Annelid worms", "Molluscs",
                           "Brown algae","Green algae","Nemertean worms","Jellyfish",
                           "Flatworms","Red algae","Sponges","Nematodes","Fish & Tunicates","Bryozoans","Sea cucumber",
                           "single-celled organisms")
# order by average frequency of occurrence
plot.group.name.order2 <- c("Molluscs","Arthropods","Green algae","Fish & Tunicates",
                            "Brown algae","Sponges","Annelid worms","Bryozoans",
                            "Jellyfishes & Hydrozoans","Nemertean worms","Flatworms","Red algae",
                            "Nematodes","Sea cucumber",
                            "single-celled organisms")


plotdat.fo %>% group_by(plot.group) %>% 
  summarise(med.fo=median(pcrab)) %>% arrange(desc(med.fo))


# Identify the outliers
findoutlier <- function(x,p=4) {
  return(x < quantile(x, .25) - p*IQR(x) | x > quantile(x, .75) + p*IQR(x))
}

plotdat.fo <- plotdat.fo %>%
  group_by(plot.group) %>%
  mutate(outlier = ifelse(findoutlier(pcrab,p=3.5), taxon, NA)) 


plot2a <- 
  ggplot(plotdat.fo, aes(x=fct_relevel(plot.group.name, plot.group.name.order2),y=pcrab)) +
  geom_boxplot() + geom_hline(aes(yintercept=-0.1)) +
  geom_text_repel(aes(label=outlier), na.rm=TRUE,min.segment.length = 0.01,nudge_y=0.5, size=4) +
  scale_y_continuous(trans=squish_trans(from=40,to=70,factor=10),
                     breaks=c(0,10,20,30,40,70,75)) +
  theme_bw() + labs(x="Prey Group", y="Percent of Crabs") + theme(axis.text.x=element_text(angle=45, hjust=1,size=12,vjust=1.05),
                                                                           legend.text=element_text(size=12),
                                                                           axis.title=element_text(size=13))
plot2a


## save ##
png(here('figs','defense_fig2_FO.png'), res=300, height=1600,width=2000)
plot2a
dev.off()


# what is fo 5% in num of crabs?
plotdat.fo %>% filter(round(pcrab,0)==5)






########################## risk ###########################
library(tidyverse)
library(here)
library(ggplot2)
library(ggrepel)
library(cowplot)
library(vegan)
library(magrittr)
library(janitor)
install.packages("ggimage")
library(ggimage)

# User inputs
resultsdir <- 'data/results'
bdir       <- 'data/bioE'
run.nums <- c(1,2)
marker  <- 'lerayXT'
knitting=FALSE



# data --------------------------------------------------------------------
taxdat <- read_csv(here(bdir,'OAimpact_runs1-2_lerayxt_allTaxa.csv'))
ed.dat <- read_csv(here(bdir, 'OAdiets_HiLo_energy_Densities.csv')) %>% dplyr::select(-OA.diet.group)
prey.fo <- read_csv( here(resultsdir,'freqoccur_runs1-2_lerayxt_presence.csv'))
img.dat <- read_csv(here('data','imgs','risk_plot_imgs.csv'))

# exposure index ----------------------------------------------------------
taxdat %<>% mutate(rraw_survival_scalar=raw_survival_scalar*-1)
eindex.dat <- taxdat %>% left_join(prey.fo,by=c("new_taxon"="taxon")) %>%
  mutate(pcrab=pcrab/100) %>%
  mutate(std_survival_scalar=decostand(x=matrix(data=taxdat$rraw_survival_scalar, ncol=1), method="range", MARGIN=2)) %>%
  mutate(eindex=sqrt(pcrab^2 + std_survival_scalar^2))

eindex.dat %<>% mutate(eindex.rescaled=decostand(x=matrix(data=eindex.dat$eindex, ncol=1), method="range", MARGIN=2))


# Combine the exposure index above ($x$) with crab sensitivity to get risk. Sensitivity is defined as the energy density of the prey item. 
risk.dat <- eindex.dat %>%
  left_join(ed.dat %>% dplyr::select(-functional_group,-raw_survival_scalar,-ncrab), by=c("new_taxon")) %>%
  filter(!is.na(ed_JgAFDW))

# standardize energy densities
risk.dat %<>% mutate(std_ed=decostand(matrix(risk.dat$ed_JgAFDW,ncol=1),method="range",MARGIN=2))


# Calculate overall risk

risk.dat %<>% mutate(crab.risk=sqrt(eindex.rescaled^2 + std_ed^2))

risk.dat %<>% mutate(crab.risk=as.numeric(decostand(x=matrix(data=risk.dat$crab.risk, ncol=1), method="range", MARGIN=2)))


### plot

risk.dat %>%
  ggplot(aes(x=std_survival_scalar,y=std_ed, color=crab.risk)) +
  geom_point(size=2) +
  geom_text_repel(aes(label=new_taxon), fontface='italic', min.segment.length = 0.01) +
  scale_color_viridis_c(option="H",begin=0.4, end=0.8, name="Crab Risk") +
  labs(x="Crab Exposure Index", y="Crab Sensitivity")

risk.dat %<>% mutate(std_survival_scalar=as.numeric(std_survival_scalar),
                     eindex=as.numeric(eindex),
                     eindex.rescaled=as.numeric(eindex.rescaled),
                     std_ed=as.numeric(std_ed))


  

tmp <- risk.dat %>%
  ggplot(aes(x=std_survival_scalar,y=std_ed, color=crab.risk)) +
  geom_point(size=3) +
  geom_text_repel(aes(label=new_taxon), fontface='italic', min.segment.length = 0.01) +
  scale_color_viridis_c(option="H",begin=0.4, end=0.8, name="Crab Risk") +
  labs(x="Crab Exposure Index", y="Crab Sensitivity") + theme_bw()

redo_colors <- data.frame(label=ggplot_build(tmp)$data[[2]]$label,
                          ggcol=ggplot_build(tmp)$data[[2]]$colour) %>%
  mutate(labelcol=darken(ggcol,amount=0.3)) %>% 
  right_join(risk.dat %>% dplyr::select(new_taxon), by=c("label"="new_taxon"))

risk.dat %>%
  left_join(redo_colors,by=c("new_taxon"="label")) %>%
  ggplot(aes(x=std_survival_scalar,y=std_ed, color=crab.risk)) +
  geom_point(size=3) +
  geom_text_repel(aes(label=new_taxon), fontface='italic', min.segment.length = 0.01, color=redo_colors$labelcol) +
  geom_image(data=data.frame(x=0.9,y=0.47,image="C:/Users/mfisher5/Documents/DungyDietAssessment/data/imgs/shell.png"),
             aes(x=x,y=y,image=image), inherit.aes=FALSE,size=.05) +
  geom_image(data=data.frame(x=0.23,y=0.23,image="C:/Users/mfisher5/Documents/DungyDietAssessment/data/imgs/algae.png"),
             aes(x=x,y=y,image=image), inherit.aes=FALSE,size=.05) +
  geom_image(data=data.frame(x=0.23,y=0.28,image="C:/Users/mfisher5/Documents/DungyDietAssessment/data/imgs/br_algae.png"),
             aes(x=x,y=y,image=image), inherit.aes=FALSE,size=.05) +
  scale_color_viridis_c(option="H",begin=0.4, end=0.8, name="Crab Risk") +
  labs(x="Crab Exposure Index", y="Crab Sensitivity") + theme_bw()

png(here('figs','defense_oa_risk.png'), res=300,height=1600,width=2200)
risk.dat %>%
  left_join(redo_colors,by=c("new_taxon"="label")) %>%
  ggplot(aes(x=std_survival_scalar,y=std_ed, color=crab.risk)) +
  geom_point(size=3) +
  geom_text_repel(aes(label=new_taxon), fontface='italic', min.segment.length = 0.01, color=redo_colors$labelcol) +
  # geom_image(data=data.frame(x=0.9,y=0.47,image="C:/Users/mfisher5/Documents/DungyDietAssessment/data/imgs/shell.png"),
  #            aes(x=x,y=y,image=image), inherit.aes=FALSE,size=.05) +
  # geom_image(data=data.frame(x=0.23,y=0.23,image="C:/Users/mfisher5/Documents/DungyDietAssessment/data/imgs/algae.png"),
  #            aes(x=x,y=y,image=image), inherit.aes=FALSE,size=.05) +
  # geom_image(data=data.frame(x=0.23,y=0.28,image="C:/Users/mfisher5/Documents/DungyDietAssessment/data/imgs/br_algae.png"),
  #            aes(x=x,y=y,image=image), inherit.aes=FALSE,size=.05) +
  scale_color_viridis_c(option="H",begin=0.4, end=0.8, name="Crab Risk") +
  labs(x="Crab Exposure Index", y="Crab Sensitivity") + theme_bw() + theme(axis.title=element_text(size=14),
                                                                           legend.title=element_text(size=14))
dev.off()






