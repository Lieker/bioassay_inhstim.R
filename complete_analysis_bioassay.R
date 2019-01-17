install.packages("ggplot2")
library(ggplot2)
install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
setwd("H:/My results/20190101")
bioassay_raw<-read.csv("name.csv", sep=';', header = TRUE)
bioassay_raw0 <- filter(bioassay_raw, t == 0) #only take data points at t=0 for normalization
names(bioassay_raw)[1] <- "well" #sometimes the name of the first column is changed upon importing; here it is changed back
names(bioassay_raw0)[1] <- "well"
bioassay_raw1 <- left_join(bioassay_raw, bioassay_raw0, by = "well", copy=FALSE, suffix=c(".tx", ".t0")) #combine the t=0 timepoints with all other timepoints
bioassay_raw2 <- subset(bioassay_raw1, select = -c(treatment.t0, t.t0)) #remove unnecessary columns
bioassay_raw2$hatchingpercent <- ((bioassay_raw2$juvs.tx - bioassay_raw2$juvs.t0) / bioassay_raw2$eggs.t0 * 100) #calculate hatching percentage
bioassay_raw3 <- filter(bioassay_raw2, hatchingpercent>=0)#only take hatching percentages above 0                        
bioassay_raw4 <- subset(bioassay_raw3, select = -c(eggs.tx, juvs.tx, eggs.t0, juvs.t0, well)) #remove unnecessary columns?
bioassay <- bioassay_raw4 %>% group_by(treatment.tx,t.tx) %>%
  summarize(mean_hatch = mean(hatchingpercent), stdev_hatch = sd(hatchingpercent)) #calculate means and standard deviation
dayseq<- unique(bioassay$t.tx) #put only days of measurements in 'dayseq' (for axis in graph)
title<- expression(paste("Hatching of ", italic("G. rostochiensis"), " eggs by root exudate")) #write the title of the graph (changes for each dataset)
ggplot(data=bioassay, aes(x=t.tx, y=mean_hatch, color = treatment.tx)) + #initiate graph, different colors per treatment
  geom_point(aes(shape=treatment.tx)) + #scatterplot with points, shape depending on treatment
  geom_line(aes(linetype=treatment.tx)) + #connect points with lines, linetype depends on treatment
  scale_shape_manual(values=c(0, 1, 2, 11, 14, 9, 8, 6)) + #say which shapes of point you want
  ggtitle(title) + #add title
  theme_light() + #theme of graph
  theme(plot.title = element_text(hjust = 0.5), legend.title=element_blank()) + #position of title
  scale_x_continuous(name = "t (days)", limits=c(-0.2,(max(dayseq)+.2)), breaks=c(0:(max(dayseq)+0.1)),1) + #format x axis
  scale_y_continuous(name = "hatching (%)", limits=c(-5,60), breaks=c(0,10,20,30,40,50,60)) + #format y axis
  geom_errorbar(aes(ymin=mean_hatch-stdev_hatch, ymax=mean_hatch+stdev_hatch), width=0.1) #add error bars
  
