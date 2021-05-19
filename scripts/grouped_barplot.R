library(ggpmisc)
library(extrafont)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(plotrix)
library(ggtext)

grouped_barplot <- function(dpi = "all",
                            height = 100,
                            method = "hsol",
                            input = "input/hatching_raw.csv",
                            species = "G. rostochiensis"
) {
  source("scripts/hatch_calc.R")
  source("scripts/hatch_summ.R")
  s <- hatch_summ(input = input, dpi = dpi)
  c <- hatch_calc(input = input, dpi = dpi)
  wc <- c[grep("water", c$treatment), ]
  ws <- s[grep("water", s$treatment), ]
  
  if (species == "G. pallida") {
    if (method == "hsol") {
      c <- c[grep("hsol", c$treatment), ]
      c <- rbind(c, wc)
      s <- s[grep("hsol", s$treatment), ]
      s <- rbind(s, ws)
      title <- "Inhibition of high-solA-PRD-induced hatching of G. pallida eggs"
      
    } else if (method == "lsol") {
      c <- c[grep("lsol", c$treatment), ]
      c <- rbind(c, wc)
      s <- s[grep("lsol", s$treatment), ]
      s <- rbind(s, ws)
      title <- "Inhibition of low-solA-PRD-induced hatching of G. pallida eggs"
      
    } else if (method == "solA") {
      c <- c[grep("solA5", c$treatment), ]
      c <- rbind(c, wc)
      s <- s[grep("solA5", s$treatment), ]
      s <- rbind(s, ws)
      title <- "Inhibition of solA-induced hatching of G. pallida eggs"
    }
  } else if(species == "G. rostochiensis"){
    if (method == "hsol") {
     s$treatment <- factor(s$treatment, levels = c("TRD_hsolA", "atrop5", "Ly500", "Akt50", "ketoc100", "dafa50"), ordered = TRUE)
     s <- s[order(s$treatment),]
     title <- "Inhibition of high-solA-PRD-induced hatching of G. rostochiensis eggs"
    
     } else if (method == "lsol") {
     s$treatment <- factor(s$treatment, levels = c("TRD_lsolA", "atrop5", "Ly500", "Akt50", "ketoc100", "dafa50"), ordered = TRUE)
     s <- s[order(s$treatment),]
     title <- "Inhibition of low-solA-PRD-induced hatching of G. rostochiensis eggs"
    
     } else if (method == "solA") {
     s$treatment <- factor(s$treatment, levels = c("solA5", "atrop5", "Ly500", "Akt50", "ketoc100", "dafa50"), ordered = TRUE)
     s <- s[order(s$treatment),]
     title <- "Inhibition of solA-induced hatching of G. rostochiensis eggs"
     }
  }
  
  
  
  b <- ggplot(s,
              aes(x = treatment,
                  y = mean_hatch,
                  fill = treatment)) +
    geom_bar(position = "dodge",
             stat = "identity",
             color = "black") +
    geom_errorbar(aes(x = treatment,
                      y = mean_hatch,
                      ymin = mean_hatch - sterr,
                      ymax = mean_hatch + sterr),
                  position = position_dodge(width = .9),
                  width = 0.2) +
    geom_jitter(data = c,
                aes(x = treatment,
                    y = hatchingpercent),
                position = position_dodge(width = .9)) +
    theme_minimal() +
    theme(legend.text = element_text(size = 15), 
          legend.text.align = 0,
          text = element_text(size = 15),
          axis.text.x = element_text(size = 15,
                                     angle = 45,
                                     hjust = 1)) +
    labs(y = "% hatch", x = "") +
    ggtitle(paste(title)) +
    scale_y_continuous(limits = c(-10, height), breaks = seq(0, 100, by = 20))
  
  return(b)
  
}
