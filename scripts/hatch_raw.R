library(ggpmisc)
library(extrafont)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotrix)

hatch_raw <- function(hatch_raw = "input/hatching_raw.csv") {
  raw <- read.csv(hatch_raw, sep=',', header = TRUE)
  names(raw)[1] <- "well"
  
  #only take data points at t=0 for normalization
  raw0 <- filter(raw, t == 0) 
  
  #combine the t=0 timepoints with all other timepoints
  raw <- left_join(raw, raw0, by = c("well", "treatment"), copy = FALSE, suffix=c("", "_t0")) %>% dplyr::filter(t!=0) 
  raw_h <- subset(raw, 
                select = -c(eggs,
                            t_t0,
                            species_t0))
  
  #remove unnecessary lines
  return(raw_h)
}
