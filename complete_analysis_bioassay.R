install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
setwd("H:/My results/181114_hatching Gros")
bioassay_raw<-read.csv("bioassay 181114_SH.csv", sep=';', header = TRUE)
bioassay_raw0 <- filter(bioassay_raw, t == 0)
names(bioassay_raw)[1] <- "well"
names(bioassay_raw0)[1] <- "well"
bioassay_raw1 <- left_join(bioassay_raw, bioassay_raw0, by = "well", copy=FALSE, suffix=c(".tx", ".t0"))
bioassay_raw2 <- subset(bioassay_raw1, select = -c(treatment.t0, t.t0))
bioassay_raw2$hatchingpercent <- ((bioassay_raw2$juvs.tx - bioassay_raw2$juvs.t0) / bioassay_raw2$eggs.t0 * 100)
bioassay_raw3 <- filter(bioassay_raw2, hatchingpercent>=0)                        
bioassay_raw4 <- subset(bioassay_raw3, select = -c(eggs.tx, juvs.tx, eggs.t0, juvs.t0, well))
bioassay <- bioassay_raw4 %>% group_by(treatment.tx,t.tx) %>%
  summarize(mean_hatch = mean(hatchingpercent), stdev_hatch = sd(hatchingpercent))
dayseq<- unique(bioassay$t.tx)
title<- expression(paste("Hatching of ", italic("G. rostochiensis"), " eggs by root exudate"))
ggplot(data=bioassay, aes(x=t.tx, y=mean_hatch, color = treatment.tx)) + 
  geom_point(aes(shape=treatment.tx)) + 
  geom_line(aes(linetype=treatment.tx)) +
  scale_shape_manual(values=c(0, 1, 2, 3, 4, 5, 11, 7, 10, 12, 13, 14, 9, 8, 6, 15, 16, 17, 18, 19, 20, 21, 22)) +
  ggtitle(title) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) +
  scale_x_continuous(name = "t (days)", limits=c(-0.2,(max(dayseq)+.2)), breaks=c(0:(max(dayseq)+0.1)),1) + 
  scale_y_continuous(name = "hatching (%)", limits=c(-5,60), breaks=c(0,10,20,30,40,50,60)) +
  geom_errorbar(aes(ymin=mean_hatch-stdev_hatch, ymax=mean_hatch+stdev_hatch), width=0.1)
  
