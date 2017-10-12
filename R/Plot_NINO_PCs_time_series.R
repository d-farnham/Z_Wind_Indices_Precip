load("data/Processed data/JFM_U_preds.RData")
load('data/Processed data/climate_ind.Rdata')

# identify the El Nino events
ENSO_phase = climate_ind %>% dplyr::mutate(NINO3.4_smoothed = as.numeric(stats::filter(NINO3.4, rep(1, 3)/3, sides = 2)),
                                     NINO3_smoothed = as.numeric(stats::filter(NINO3, rep(1, 3)/3, sides = 2)),
                                     NINO4_smoothed = as.numeric(stats::filter(NINO4, rep(1, 3)/3, sides = 2))) %>%
  dplyr::filter(month == 1) %>%
  dplyr::mutate(ENSO_phase = ifelse(NINO3.4_smoothed > 1, "warm",
                             ifelse(NINO3.4_smoothed < (-1), "cool", "neutral"))) %>%
  dplyr::select(year, ENSO_phase)

NINO_3.4_JFM_phase = climate_ind %>% dplyr::mutate(NINO3.4_smoothed = as.numeric(stats::filter(NINO3.4, rep(1, 3)/3, sides = 2))) %>%
  dplyr::filter(month == 2) %>%
  dplyr::select(year, NINO3.4_smoothed)

JFM_U_preds_ENSO = merge(merge(JFM_U_preds, ENSO_phase, by = "year"), NINO_3.4_JFM_phase, by = "year") %>%
                        dplyr::filter(!is.na(ENSO_phase)) %>%
                        dplyr::mutate(ENSO_phase = factor(ENSO_phase, levels = c("warm", "neutral", "cool")))

JFM_U_preds_ENSO_long = melt(JFM_U_preds_ENSO, id.vars = c("year", "ENSO_phase"))


NINO3.4_phase_plot =
ggplot(JFM_U_preds_ENSO %>% dplyr::filter(!is.na(ENSO_phase))) +
  geom_bar(aes(year, y = NINO3.4_smoothed, fill = ENSO_phase),stat = "identity") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(data = JFM_U_preds_ENSO %>% dplyr::filter(year == 2016),
            aes(year, NINO3.4_smoothed+0.2, label = year)) +
  geom_text(data = JFM_U_preds_ENSO %>% dplyr::filter(year == 1998),
            aes(year, NINO3.4_smoothed+0.2, label = year)) +
  geom_text(data = JFM_U_preds_ENSO %>% dplyr::filter(year == 1983),
            aes(year, NINO3.4_smoothed+0.2, label = year)) +
  geom_text(data = JFM_U_preds_ENSO %>% dplyr::filter(year == 2003),
            aes(year, NINO3.4_smoothed+0.2, label = year)) +
  scale_fill_discrete(name = "JFM ENSO \n Phase") +
  labs(y = "JFM NINO 3.4") +
  theme_bw() +
  theme(legend.position = "bottom") +
  scale_x_continuous(breaks = seq(1950,2015, by = 10))

# compute the percentiles of value by PC number and ENSO phase
JFM_U_preds_ENSO_long_precentile = JFM_U_preds_ENSO_long %>% dplyr::group_by(ENSO_phase, variable) %>%
                                                             dplyr::summarise(bot_12.5 = quantile(value, prob = 0.125),
                                                                              top_12.5 = quantile(value, prob = 0.875),
                                                                              median = quantile(value, prob = 0.5))

library(GGally)
PC_par_coord_plot = 
  ggplot(JFM_U_preds_ENSO_long_precentile %>% dplyr::filter(variable != "NINO3.4_smoothed")) +
  geom_pointrange(aes(variable, y = median, ymin = bot_12.5, ymax = top_12.5, col = ENSO_phase), 
                  position=position_dodge(width=0.3), size = 0.65) + 
  geom_line(data = JFM_U_preds_ENSO_long %>% dplyr::filter(variable != "NINO3.4_smoothed" &
                                                    year %in% c(1983,1998,2003,2016)),
            aes(variable, y = value, group = year, linetype = factor(year)), size = 1.05) +
  geom_text(data = JFM_U_preds_ENSO_long %>% dplyr::filter(variable %in% c("PC1") &
                                                             year %in% c(1983,2016)),
            aes(variable, y = value+0.5, label = year), size = 4, hjust = 1.25) +
  geom_text(data = JFM_U_preds_ENSO_long %>% dplyr::filter(variable %in% c("PC1") &
                                                             year %in% c(1998,2003)),
            aes(variable, y = value-0.5, label = year), size = 4, hjust = 1.25) +
  scale_fill_discrete(guide = "none") +
  scale_color_discrete(name = "JFM ENSO Phase", guide = "none") +
  scale_linetype_manual(name = "Year",values = c("solid", "dotdash", "longdash", "dotted")) +
  labs(y = "Unscaled PC values",
       x = "PC") +
  theme_bw() 

pdf(file = "Final figures/Figure_3.pdf", width = 8, height = 7)
grid.arrange(NINO3.4_phase_plot,
             PC_par_coord_plot,
             nrow = 2)
dev.off()


rm(list = ls())
