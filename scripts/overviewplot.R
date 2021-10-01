overview <- function(atrop_c, atrop_s, xf = 50){
  x <- ggplot(data = atrop_s, aes(x = treatment, y = mean_hatch)) +
    geom_bar(position = "dodge",
             stat = "identity",
             color = "black",
             fill = "lightgrey") +
    geom_errorbar(aes(x = treatment,
                      y = mean_hatch,
                      ymin = mean_hatch - stdev,
                      ymax = mean_hatch + stdev),
                  position = position_dodge(width = .9), width = 0.2) + 
    geom_jitter(data = atrop_c,
                aes(x = treatment,
                    y = hatchingpercent),
                position = position_dodge(width = .9)) +
    geom_point(data = atrop_s, aes(x = treatment, y = concentration.uM./xf), color = "red") +
    theme_minimal() +
    theme(legend.text = element_text(size = 15),
          legend.text.align = 0,
          text = element_text(size = 15),
          axis.text.x = element_text(size = 8,
                                     angle = 45,
                                     hjust = 1)) +
    labs(y = "% hatch", x = "") +
    scale_y_continuous(limits = c(-10, 100), breaks = seq(0, 100, by = 10),
                       sec.axis = sec_axis(trans=~.*50, name="atropine conc")) +
    facet_grid(~date, scales = "free", space = "free")
  return(x)}