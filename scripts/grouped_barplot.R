library(ggpmisc)
library(extrafont)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotrix)

grouped_barplot <- function(dpi = 10,
                            grouping = s$species,
                            height = 100
) {
  source("scripts/hatch_calc.R")
  source("scripts/hatch_summ.R")
  s <- hatch_summ(dpi = dpi)
  c <- hatch_calc(dpi = dpi)
  
  b <- ggplot(s,
              aes(fill = treatment,
                  y = mean_hatch,
                  x = grouping)) +
    geom_bar(position = "dodge",
             stat = "identity") +
    geom_errorbar(aes(x = grouping,
                      y = mean_hatch,
                      ymin = mean_hatch - sterr,
                      ymax = mean_hatch + sterr),
                  position = position_dodge(width = .9),
                  width = 0.2) +
    geom_jitter(data = c,
                aes(x = species,
                   y = hatchingpercent),
                position = position_dodge(width = .9)) +
    theme_minimal() +
    theme(axis.title.x = element_blank()) +
    labs(y = "% hatch") +
    ggtitle(paste("Hatching after", dpi, "days")) +
    scale_y_continuous(limits = c(-5, height), breaks = seq(0, 100, by = 20))
  
  return(b)
  
}
