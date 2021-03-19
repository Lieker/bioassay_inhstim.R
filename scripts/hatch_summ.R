library(ggpmisc)
library(extrafont)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotrix)

setwd("C:/Users/levla/github/bioassay.R")


hatch_summ <- function(dpi = 10) {
  source("scripts/hatch_calc.R")
  
  summ <- hatch_calc(dpi = dpi)

  summ <- summ %>% group_by(treatment, species) %>%
    summarize(mean_hatch = mean(hatchingpercent),
              stdev = sd(hatchingpercent),
              sterr = as.numeric(std.error(hatchingpercent)))
  
  return(summ)
}
  