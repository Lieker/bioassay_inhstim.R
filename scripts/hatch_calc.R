library(ggpmisc)
library(extrafont)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotrix)



hatch_calc <- function(hraw = "input/hatching_raw.csv",
                       dpi = "all") {
  source("scripts/hatch_raw.R")
  raw <- hatch_raw(hatch_raw = hraw)
  
  if (dpi == "all") {
    raw <- hatch_raw(hatch_raw = hraw)
    raw$hatchingpercent <- ((as.numeric(as.character(raw$juvs)) - as.numeric(as.character(raw$juvs_t0))) / as.numeric(as.character(raw$eggs_t0)) * 100)
    h_calc <- subset(raw, select = -c(juvs, eggs_t0, juvs_t0))
    
    } else if (class(dpi) == "integer" | class(dpi) == "numeric") {
      raw <- hatch_raw(hatch_raw = hraw)
      raw <- raw[raw$t == dpi,]
      raw$hatchingpercent <- ((as.numeric(as.character(raw$juvs)) - as.numeric(as.character(raw$juvs_t0))) / as.numeric(as.character(raw$eggs_t0)) * 100)
      h_calc <- subset(raw, select = -c(juvs, eggs_t0, juvs_t0, t))
      }
  return(h_calc)
 }
