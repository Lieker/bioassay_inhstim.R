library(ggpmisc)
library(extrafont)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotrix)

setwd("C:/Users/levla/github/bioassay.R")


hatch_summ <- function(dpi = "all") {
  source("scripts/hatch_calc.R")
  
  summ <- hatch_calc(dpi = dpi)

  summ <- summ %>% group_by(treatment) %>%
    summarize(mean_hatch = mean(hatchingpercent),
              stdev = sd(hatchingpercent),
              sterr = as.numeric(std.error(hatchingpercent)))
  
  return(summ)
}
  