hs <- hatch_summ(input = "input/hatchingRNAseq.csv")
hc <- hatch_calc(input = "input/hatchingRNAseq.csv")

hs$t <- factor(hs$t, levels = c(1,2,3,5,7,10,14,17,20), ordered = TRUE)
hs <- hs[order(hs$t),]
hc$t <- factor(hc$t, levels = c(1,2,3,5,7,10,14,17,20), ordered = TRUE)
hc <- hc[order(hc$t),]

treatmentslevels <- c("Sol_8nM",
                      "solA4nM",
                      "solA2nM",
                      "solA1nM",
                      "solA0.5nM",
                      "PRE",
                      "TRE",
                      "1%EtOH",
                      "water")
hs$treatment <- factor(hs$treatment, levels = treatmentslevels, ordered = TRUE)
hs <- hs[order(hs$treatment),]
hc$treatment <- factor(hc$treatment, levels = treatmentslevels, ordered = TRUE)
hc <- hc[order(hc$treatment),]

b <- ggplot(hs,
            aes(x = treatment,
                y = mean_hatch,
                fill = t)) +
  geom_bar(position = "dodge",
           stat = "identity",
           color = "black") +
  geom_errorbar(aes(x = treatment,
                    y = mean_hatch,
                    ymin = mean_hatch - sterr,
                    ymax = mean_hatch + sterr),
                position = position_dodge(width = .9),
                width = 0.2) +
  geom_jitter(data = hc,
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
  scale_y_continuous(limits = c(-2, 55), breaks = seq(0, 100, by = 20)) +
  scale_x_discrete(labels = c("Sol_8nM" = "8nM solA",
                              "solA4nM" = "4nM solA",
                              "solA2nM" = "2nM solA",
                              "solA1nM" = "1nM solA",
                              "solA0.5nM" = "0.5nM solA",
                              "PRE",
                              "TRE",
                              "1%EtOH" = "1% EtOH in water",
                              "water")) +
  guides(fill=guide_legend(title="t (days)")) +
  scale_fill_grey(start=1, end=0.2)
b

#change hatch_summ function to NOT include t in group_by()
hs1 <- hatch_summ(dpi = 20, input = "input/hatchingRNAseq.csv")
hc1 <- hatch_calc(dpi = 20, input = "input/hatchingRNAseq.csv")

treatmentslevels <- c("Sol_8nM",
                      "solA4nM",
                      "solA2nM",
                      "solA1nM",
                      "solA0.5nM",
                      "PRE",
                      "TRE",
                      "1%EtOH",
                      "water")
hs1$treatment <- factor(hs1$treatment, levels = treatmentslevels, ordered = TRUE)
hs1 <- hs1[order(hs1$treatment),]
hc1$treatment <- factor(hc1$treatment, levels = treatmentslevels, ordered = TRUE)
hc1 <- hc1[order(hc1$treatment),]

b1 <- ggplot(hs1,
            aes(x = treatment,
                y = mean_hatch)) +
  geom_bar(position = "dodge",
           stat = "identity",
           color = "black") +
  geom_errorbar(aes(x = treatment,
                    y = mean_hatch,
                    ymin = mean_hatch - sterr,
                    ymax = mean_hatch + sterr),
                position = position_dodge(width = .9),
                width = 0.2) +
  geom_jitter(data = hc1,
              aes(x = treatment,
                  y = hatchingpercent),
              position = position_dodge(width = 2)) +
  theme_minimal() +
  theme(legend.text = element_text(size = 15), 
        legend.text.align = 0,
        text = element_text(size = 15),
        axis.text.x = element_text(size = 15,
                                   angle = 45,
                                   hjust = 1)) +
  labs(y = "% hatch", x = "") +
  scale_y_continuous(limits = c(-2, 55), breaks = seq(0, 100, by = 20)) +
  scale_x_discrete(labels = c("Sol_8nM" = "8nM solA",
                              "solA4nM" = "4nM solA",
                              "solA2nM" = "2nM solA",
                              "solA1nM" = "1nM solA",
                              "solA0.5nM" = "0.5nM solA",
                              "PRE",
                              "TRE",
                              "1%EtOH" = "1% EtOH in water",
                              "water")) +
  guides(fill=guide_legend(title="t (days)")) +
  scale_fill_grey(start=1, end=0.2)
b1
