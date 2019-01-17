install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
setwd("D:/KNIME/knime-workspace")
bioassay_raw<-read.csv("gros 01-05-2018 knime.csv", fileEncoding="UTF-8-BOM", sep=';', header = TRUE)
bioassay_raw0 <- filter(bioassay_raw, t == 0)
bioassay_raw1 <- left_join(bioassay_raw, bioassay_raw0, by = "well", copy=FALSE, suffix=c(".tx", ".t0"))
bioassay_raw2 <- subset(bioassay_raw1, select = -c(treatment.t0, t.t0))
bioassay_raw2$hatchingpercent <- ((bioassay_raw2$juvs.tx - bioassay_raw2$juvs.t0) / bioassay_raw2$eggs.t0 * 100)
bioassay_raw3 <- filter(bioassay_raw2, hatchingpercent>=0)                        
bioassay_raw4 <- subset(bioassay_raw3, select = -c(eggs.tx, juvs.tx, eggs.t0, juvs.t0, well))
bioassay <- bioassay_raw4 %>% group_by(treatment.tx,t.tx) %>%
  summarize(mean_hatch = mean(hatchingpercent), stdev_hatch = sd(hatchingpercent)) 


bioassay<-read.csv("gpal010518.csv", fileEncoding="UTF-8-BOM", sep=';', header = TRUE)
dayseq<- unique(bioassay$t.tx)
ggplot(data=bioassay, aes(x=t.tx, y=mean_hatch, color = treatment.tx)) + 
  geom_point(aes(shape=treatment.tx)) + geom_line() + scale_linetype_manual(values=c("twodash", "dotted", "twodash", "longdash", "dotdash", "dashed", "solid", "solid")) +
  scale_shape_manual(values=c(0, 1, 2, 11, 14, 9, 8, 6)) +
  ggtitle("Hatching of G. rostochiensis eggs") +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) +
  scale_x_continuous(name = "t(days)", limits=c(0,(max(dayseq))), breaks=c(0:(max(dayseq)+0.1)),1) + 
  scale_y_continuous(name = "hatching(%)", limits=c(-5,55), breaks=c(0,10,20,30,40,50)) +
  geom_errorbar(aes(ymin=mean_hatch-stdev_hatch, ymax=mean_hatch+stdev_hatch), width=0.1) 
  




  scale_colour_discrete(name ="blank", breaks=c("Hoagland", "PRE", "TRE"), labels=c("Hoagland", "Potato Root Exudate", "Tomato Root Exudate")) +
  scale_shape_discrete(name  ="blank",
                       breaks=c("Hoagland", "PRE", "TRE"),
                       labels=c("Hoagland", "Potato Root Exudate", "Tomato Root Exudate"))


  




scale_colour_discrete(name ="blank", breaks=c("Calciferol20µM", "Calciferol200nM", "Calciferol2nM", "H2O2.5%ethanol", "PRE", "PREpowder", "TREpowder"), labels=c("Calciferol 20 µM", "Calciferol 200 nM", "Calciferol 2 nM", "H2O 2.5% ethanol", "Potato Root Exudate", "PRE Powder", "TRE Powder")) 


  
