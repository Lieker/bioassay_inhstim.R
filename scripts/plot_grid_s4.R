plotgrid <- function(c, s){
  x <- ggplot(data = s, aes(x = conc_inh, y = mean_hatch)) +
    geom_bar(position = "dodge",
             stat = "identity",
             color = "black",
             fill = "lightgrey") +
    geom_errorbar(aes(x = conc_inh,
                      y = mean_hatch,
                      ymin = mean_hatch - stdev,
                      ymax = mean_hatch + stdev),
                  position = position_dodge(width = .9), width = 0.2) + 
    geom_jitter(data = c,
                aes(x = conc_inh,
                    y = hatchingpercent),
                position = position_dodge(width = .9)) +
    theme_minimal() +
    theme(legend.text = element_text(size = 15),
          legend.text.align = 0,
          text = element_text(size = 15),
          axis.text.x = element_text(size = 8,
                                     angle = 45,
                                     hjust = 1)) +
    labs(y = "% hatch", x = "") +
    scale_y_continuous(limits = c(-10, 95), breaks = seq(0, 100, by = 10)) +
    facet_grid(.~stimulator, scales = "free_x", space = "free")
  return(x)}

plotgrid2 <- function(c, s){
  x <- ggplot(data = s, aes(x = treatment, y = mean_hatch)) +
    geom_bar(position = "dodge",
             stat = "identity",
             color = "black",
             fill = "lightgrey") +
    geom_errorbar(aes(x = treatment,
                      y = mean_hatch,
                      ymin = mean_hatch - stdev,
                      ymax = mean_hatch + stdev),
                  position = position_dodge(width = .9), width = 0.2) + 
    geom_jitter(data = c,
                aes(x = treatment,
                    y = hatchingpercent),
                position = position_dodge(width = .9)) +
    theme_minimal() +
    theme(legend.text = element_text(size = 15),
          legend.text.align = 0,
          text = element_text(size = 15),
          axis.text.x = element_text(size = 8,
                                     angle = 45,
                                     hjust = 1)) +
    labs(y = "% hatch", x = "") +
    scale_y_continuous(limits = c(-10, 95), breaks = seq(0, 100, by = 10)) +
    facet_grid(.~stimulator, scales = "free_x", space = "free")
  return(x)}


plotgrid3 <- function(c, s){
  x <- ggplot(data = s, aes(x = treatment, y = mean_hatch)) +
    geom_bar(stat = "identity",
             color = "black",
             fill = "lightgrey") +
    geom_errorbar(aes(x = treatment,
                      y = mean_hatch,
                      ymin = mean_hatch - stdev,
                      ymax = mean_hatch + stdev), width = 0.2) + 
    geom_jitter(data = c,
                aes(x = treatment,
                    y = hatchingpercent),
                position = position_dodge(width = .9)) +
    theme_minimal() +
    theme(legend.text = element_text(size = 15),
          legend.text.align = 0,
          text = element_text(size = 15),
          axis.text.x = element_text(size = 8,
                                     angle = 45,
                                     hjust = 1)) +
    labs(y = "% hatch", x = "") +
    scale_y_continuous(limits = c(-10, 95), breaks = seq(0, 100, by = 10))
  
  return(x)}
