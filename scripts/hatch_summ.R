hatch_summ <- function(dpi = "all",
                        input = "input/hatching_raw.csv") {
  source("scripts/hatch_calc.R")
  
  summ <- hatch_calc(dpi = dpi, input = input)
  
  summ <- summ %>% group_by(treatment) %>%
    summarize(mean_hatch = mean(hatchingpercent),
              stdev = sd(hatchingpercent),
              sterr = as.numeric(std.error(hatchingpercent)))
  
  return(summ)
}


hatch_summ1 <- function(dpi = "all",
                       input = "input/hatching_raw.csv") {
  source("scripts/hatch_calc.R")
  
  summ <- hatch_calc(dpi = dpi, input = input)

  summ <- summ %>% group_by(treatment, date, concentration.uM.) %>%
    summarize(mean_hatch = mean(hatchingpercent),
              stdev = sd(hatchingpercent),
              sterr = as.numeric(std.error(hatchingpercent)))
  
  return(summ)
}
  
hatch_summ2 <- function(dpi = "all",
                        input = "input/hatching_raw.csv") {
  source("scripts/hatch_calc.R")
  
  summ <- hatch_calc(dpi = dpi, input = input)
  
  summ <- summ %>% group_by(treatment, inhibitor, stimulator) %>%
    summarize(mean_hatch = mean(hatchingpercent),
              stdev = sd(hatchingpercent),
              sterr = as.numeric(std.error(hatchingpercent)))
  
  return(summ)
}

hatch_summ3 <- function(dpi = "all",
                        input = "input/hatching_raw.csv") {
  source("scripts/hatch_calc.R")
  
  summ <- hatch_calc(dpi = dpi, input = input)
  
  summ <- summ %>% group_by(treatment, inhibitor, stimulator, conc_inh) %>%
    summarize(mean_hatch = mean(hatchingpercent),
              stdev = sd(hatchingpercent),
              sterr = as.numeric(std.error(hatchingpercent)))
  
  return(summ)
}

hatch_summ4 <- function(dpi = "all",
                        input = "input/hatching_raw.csv") {
  source("scripts/hatch_calc.R")
  
  summ <- hatch_calc(dpi = dpi, input = input)
  
  summ <- summ %>% group_by(treatment, stimulator, conc_inh) %>%
    summarize(mean_hatch = mean(hatchingpercent),
              stdev = sd(hatchingpercent),
              sterr = as.numeric(std.error(hatchingpercent)))
  
  return(summ)
}

hatch_summ5 <- function(dpi = "all",
                        input = "input/hatching_raw.csv") {
  source("scripts/hatch_calc.R")
  
  summ <- hatch_calc(dpi = dpi, input = input)
  
  summ <- summ %>% group_by(treatment, inhibitor, stimulator, conc_inh, DA, conc_stim) %>%
    summarize(mean_hatch = mean(hatchingpercent),
              stdev = sd(hatchingpercent),
              sterr = as.numeric(std.error(hatchingpercent)))
  
  return(summ)
}
