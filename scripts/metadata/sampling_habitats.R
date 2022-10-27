library(tidyverse)
library(here)
library(janitor)

dat <- read.csv(here('data','eelgrass_survey_data.csv')) %>% clean_names()
library(vegan)
library(labdsv)
library(ggfortify)


sdat_mat <- as.matrix(dat[,3:17])
substrate.pca <- prcomp(x=sdat_mat)

autoplot(substrate.pca, data = dat, colour = 'site', size=5,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 2, main="Substrate") + theme_bw()

edat_mat <- as.matrix(dat[18:27])
eelgrass.pca <- prcomp(x=edat_mat)

autoplot(eelgrass.pca, data = dat, colour = 'site', size=5,
         loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 2, main="Eelgrass Density / Algae Cover") + theme_bw()
