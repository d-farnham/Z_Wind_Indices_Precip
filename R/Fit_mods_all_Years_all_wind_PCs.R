load("data/Processed data/us_precip.Rdata")
load("data/Processed data/climate_ind.Rdata")
load("data/Processed data/JFM_U_preds.RData")

# load functions to calcuate model skill
source("R/Get_skill.R")

# identify the ENSO phase
ENSO_phase = climate_ind %>% dplyr::mutate(NINO3.4_smoothed = as.numeric(stats::filter(NINO3.4, rep(1, 3)/3, sides = 2))) %>%
  dplyr::filter(month == 1,
                !is.na(NINO3.4_smoothed)) %>%
  dplyr::mutate(EN_phase = ifelse(NINO3.4_smoothed > 1, "warm",
                            ifelse(NINO3.4_smoothed < (-1), "cool", "neutral"))) %>%
  dplyr::select(year, EN_phase)

climate_ind_JFM = climate_ind %>% dplyr::filter(month %in% c(1,2,3)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(NINO3.4 = mean(NINO3.4),
                   NINO3 = mean(NINO3),
                   NINO4 = mean(NINO4),
                   NINO1_2 = mean(NINO1_2),
                   PNA = mean(PNA),
                   NAO = mean(NAO),
                   PDO = mean(PDO))
JFM_preds = merge(climate_ind_JFM, JFM_U_preds,by = c("year")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise_each(funs(mean))

# find the grids inside the US and inside of the WESTERN STATES

usa = map("world",regions = "usa", plot = FALSE, fill = TRUE)
load(file = "data/Processed data/western_states.RData")

require(sp)
require(maptools)

usa_IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
west_IDs <- sapply(strsplit(west$names, ":"), function(x) x[1])

usa <- map2SpatialPolygons(usa, IDs = usa_IDs, proj4string = CRS("+proj=longlat +datum=WGS84"))
west <- map2SpatialPolygons(west, IDs = west_IDs, proj4string = CRS("+proj=longlat +datum=WGS84"))


source("R/Get_skill.R")

cand_preds_xval = list(c("PC1"),
                       c("PC1","PC2","PC3","PC4", "PC5", "PC6"),
                       c("NINO3.4"),
                       c("NINO3.4","PNA"),
                       c("NINO3.4","NAO","PDO"))

MAE_SS_us = MAE_SS_west = matrix(nrow = length(cand_preds_xval), ncol = nrow(JFM_preds))

HEIDKE_SS_us = HEIDKE_SS_west  = matrix(nrow = length(cand_preds_xval), ncol = nrow(JFM_preds))

for(yyear in 1:nrow(JFM_preds)){
	JFM_mean_precip = us_precip %>% data.table() %>%
		dplyr::filter(year != JFM_preds$year[yyear]) %>%
		dplyr::filter(month %in% c(1,2,3)) %>%
		dplyr::filter(!is.na(precip)) %>%
		dplyr::group_by(latitude,longitude,year) %>%
		dplyr::summarise(precip = sum(precip)) %>%
		dplyr::group_by(latitude,longitude) %>%
		dplyr::summarise(lower_tercile = quantile(precip,prob = 0.333),
										 upper_tercile = quantile(precip,prob = 0.667),
										 mean_precip = mean(precip), 
										 sd_precip = sd(precip)) %>%
		data.frame()
	
	JFM_precip = us_precip %>% data.table() %>%
	  dplyr::filter(year != JFM_preds$year[yyear]) %>%
	  dplyr::filter(month %in% c(1,2,3)) %>%
		dplyr::filter(!is.na(precip)) %>%
		dplyr::group_by(latitude,longitude,year) %>%
		dplyr::summarise(precip = sum(precip)) %>%
		data.frame()
	
	JFM_held_out_year_precip = us_precip %>% data.table() %>%
		dplyr::filter(year == JFM_preds$year[yyear] &
		              month %in% c(1,2,3) &
		              !is.na(precip)) %>%
	  dplyr::group_by(latitude,longitude) %>%
	  dplyr::summarise(precip = sum(precip)) %>%
	  data.frame()
	
	
	
	precip_pred = merge(JFM_precip,JFM_preds[JFM_preds$year != JFM_preds$year[yyear], ], by = "year")
	
	
	for(preds in 1:length(cand_preds_xval)){
	  precip_pred_sub = precip_pred[ , c("latitude", "longitude", "precip", c(cand_preds_xval[[preds]]))] 
	  
	  reg_all = plyr::ddply(precip_pred_sub, c("latitude","longitude"), function(x) coefficients(lm(precip ~., data=x[,-c(1,2)])))
	  
	  
	  pred_2016 = JFM_preds[JFM_preds$year == JFM_preds$year[yyear], c(cand_preds_xval[[preds]])]
	  coefs_reg_all = reg_all[ ,c(cand_preds_xval[[preds]])]
	  
	  precip_2016_preds_all = data.frame(reg_all[, c("latitude", "longitude")], 
	                                     reg_all$`(Intercept)` +  as.matrix(coefs_reg_all) %*% t(as.matrix(pred_2016)))
	  colnames(precip_2016_preds_all)[3] = "pred"
	  precip_2016_preds_all = merge(merge(JFM_mean_precip, precip_2016_preds_all, by = c("latitude", "longitude")), 
	                                JFM_held_out_year_precip, by = c("latitude", "longitude"))
	  
	  
	  skill_dat = precip_2016_preds_all
	  
		usa_pts <- SpatialPoints(skill_dat[,c("longitude","latitude")],proj4string = CRS(proj4string(usa)))
		usa_ii = !is.na(over(usa_pts,usa))
		
		west_pts <- SpatialPoints(skill_dat[,c("longitude","latitude")],proj4string = CRS(proj4string(west)))
		west_ii = !is.na(over(west_pts,west))
		
		MAE_SS_us[preds,yyear] = MAE_SS(skill_dat[c(usa_ii),]$pred, 
																		skill_dat[c(usa_ii),]$mean_precip, 
																		skill_dat[c(usa_ii),]$precip,
																		skill_dat[c(usa_ii),]$sd_precip)

		
		MAE_SS_west[preds,yyear] = MAE_SS(skill_dat[c(west_ii),]$pred, 
																			skill_dat[c(west_ii),]$mean_precip, 
																			skill_dat[c(west_ii),]$precip,
																			skill_dat[c(west_ii),]$sd_precip)
		
		HEIDKE_SS_us[preds,yyear] = HEIDKE_SS(skill_dat[c(usa_ii),]$pred, 
																					skill_dat[c(usa_ii),]$lower_tercile, 
																					skill_dat[c(usa_ii),]$upper_tercile,
																					skill_dat[c(usa_ii),]$precip)
		
		HEIDKE_SS_west[preds,yyear] = HEIDKE_SS(skill_dat[c(west_ii),]$pred, 
																						skill_dat[c(west_ii),]$lower_tercile, 
																						skill_dat[c(west_ii),]$upper_tercile,
																						skill_dat[c(west_ii),]$precip)
	}
	print(paste0("Completed x-val on EN year ",JFM_preds$year[yyear]))
}

HEIDKE_SS_us_df = data.frame(t(HEIDKE_SS_us), year = JFM_preds$year)
colnames(HEIDKE_SS_us_df) = c("PC1", "PC1-PC6", "NINO3.4", "NINO3.4 & PNA", "NINO3.4 & NAO & PDO", "year")
HEIDKE_SS_us_df = melt(HEIDKE_SS_us_df, id.vars = "year")
HEIDKE_SS_us_df$variable = factor(HEIDKE_SS_us_df$variable)

HEIDKE_SS_us_df = HEIDKE_SS_us_df %>% dplyr::group_by(variable) %>%
                                      dplyr::mutate(value_mean = mean(value))

HEIDKE_SS_us_df = merge(HEIDKE_SS_us_df, ENSO_phase, by = "year") %>%
                                      dplyr::group_by(EN_phase,variable) %>%
                                      dplyr::mutate(value_mean_phase = mean(value))

HEIDKE_us_plot = 
  ggplot() + 
  geom_vline(xintercept = c(ENSO_phase$year[ENSO_phase$EN_phase == "warm"]), col = "grey", alpha = 0.75, size = 3) +
  geom_vline(xintercept = 2018, linetype = "dashed") +
  geom_text(aes(x = 2022, y = (-0.075), label = "Mean skill")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_bar(data = HEIDKE_SS_us_df, aes(x = 2022, y = value_mean, fill = variable), stat = "identity", position = "dodge", width = 3) +
  geom_line(data = HEIDKE_SS_us_df, aes(x=year, y = value, col = variable)) +
  geom_point(data = HEIDKE_SS_us_df, aes(x=year, y = value, col = variable)) +
  scale_y_continuous(limits = c(min(HEIDKE_SS_us_df$value) - 0.05,
                                max(HEIDKE_SS_us_df$value) + 0.05)) +
  scale_x_continuous(breaks = seq(1950, 2017, by = 5),
                     labels = seq(1950, 2017, by = 5)) +
  theme_bw() +
  ylab("Heidke Skill Score") +
  xlab("Year") +
  scale_color_discrete(name = "Predictors") +
  ggtitle("US") +
  scale_fill_discrete(name = "Predictors")

HEIDKE_us_plot_density = 
  ggplot() + 
  geom_density(data = HEIDKE_SS_us_df %>% dplyr::filter(variable %in% c("PC1", "PC1-PC6", "NINO3.4")), aes(x = value, col = EN_phase)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(data = HEIDKE_SS_us_df %>% dplyr::filter(variable %in% c("PC1", "PC1-PC6", "NINO3.4")),
             aes(xintercept = value_mean_phase, col = EN_phase), linetype = "dashed", size = 0.75, alpha = 0.75) +
  theme_bw() +
  xlab("Heidke Skill Score") +
  ylab("Prob. Density") +
  scale_color_manual(name = "ENSO Phase", values = c("blue", "purple", "red")) +
  ggtitle("US") +
  facet_wrap(~variable)


HEIDKE_SS_west_df = data.frame(t(HEIDKE_SS_west), year = JFM_preds$year)
colnames(HEIDKE_SS_west_df) = c("PC1", "PC1-PC6", "NINO3.4", "NINO3.4 & PNA", "NINO3.4 & NAO & PDO", "year")
HEIDKE_SS_west_df = melt(HEIDKE_SS_west_df, id.vars = "year")
HEIDKE_SS_west_df$variable = factor(HEIDKE_SS_west_df$variable)

HEIDKE_SS_west_df = HEIDKE_SS_west_df %>% dplyr::group_by(variable) %>%
                                          dplyr::mutate(value_mean = mean(value))


HEIDKE_SS_west_df = merge(HEIDKE_SS_west_df, ENSO_phase, by = "year") %>%
  dplyr::group_by(EN_phase,variable) %>%
  dplyr::mutate(value_mean_phase = mean(value))

HEIDKE_west_plot = 
  ggplot() + 
  geom_vline(xintercept = c(ENSO_phase$year[ENSO_phase$EN_phase == "warm"]), col = "grey", alpha = 0.75, size = 3) +
  geom_vline(xintercept = 2018, linetype = "dashed") +
  geom_text(aes(x = 2022, y = (-0.075), label = "Mean skill")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_bar(data = HEIDKE_SS_west_df, aes(x = 2022, y = value_mean, fill = variable), stat = "identity", position = "dodge", width = 3) +
  geom_line(data = HEIDKE_SS_west_df, aes(x=year, y = value, col = variable)) +
  geom_point(data = HEIDKE_SS_west_df, aes(x=year, y = value, col = variable)) +
  scale_y_continuous(limits = c(min(HEIDKE_SS_west_df$value) - 0.05,
                                max(HEIDKE_SS_west_df$value) + 0.05)) +
  scale_x_continuous(breaks = seq(1950, 2017, by = 5),
                     labels = seq(1950, 2017, by = 5)) +
  theme_bw() +
  ylab("Heidke Skill Score") +
  xlab("Year") +
  scale_color_discrete(name = "Predictors") +
  ggtitle("West") +
  scale_fill_discrete(name = "Predictors")

HEIDKE_west_plot_density = 
  ggplot() + 
  geom_density(data = HEIDKE_SS_west_df %>% dplyr::filter(variable %in% c("PC1", "PC1-PC6", "NINO3.4")), aes(x = value, col = EN_phase)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(data = HEIDKE_SS_west_df %>% dplyr::filter(variable %in% c("PC1", "PC1-PC6", "NINO3.4")),
             aes(xintercept = value_mean_phase, col = EN_phase), linetype = "dashed", size = 0.75, alpha = 0.75) +
  theme_bw() +
  xlab("Heidke Skill Score") +
  ylab("Prob. Density") +
  scale_color_manual(name = "ENSO Phase", values = c("blue", "purple", "red")) +
  ggtitle("West") +
  facet_wrap(~variable)


MAE_SS_us_df = data.frame(t(MAE_SS_us), year = JFM_preds$year)
colnames(MAE_SS_us_df) = c("PC1", "PC1-PC6", "NINO3.4", "NINO3.4 & PNA", "NINO3.4 & NAO & PDO", "year")
MAE_SS_us_df = melt(MAE_SS_us_df, id.vars = "year")
MAE_SS_us_df$variable = factor(MAE_SS_us_df$variable)

MAE_SS_us_df = MAE_SS_us_df %>% dplyr::group_by(variable) %>%
                                dplyr::mutate(value_mean = mean(value))

MAE_SS_us_df = merge(MAE_SS_us_df, ENSO_phase, by = "year") %>%
  dplyr::group_by(EN_phase,variable) %>%
  dplyr::mutate(value_mean_phase = mean(value))

MAE_us_plot = 
  ggplot() + 
  geom_vline(xintercept = c(ENSO_phase$year[ENSO_phase$EN_phase == "warm"]), col = "grey", alpha = 0.75, size = 3) +
  geom_vline(xintercept = 2018, linetype = "dashed") +
  geom_text(aes(x = 2022, y = (-0.075), label = "Mean skill")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_bar(data = MAE_SS_us_df, aes(x = 2022, y = value_mean, fill = variable), stat = "identity", position = "dodge", width = 3) +
  geom_line(data = MAE_SS_us_df, aes(x=year, y = value, col = variable)) +
  geom_point(data = MAE_SS_us_df, aes(x=year, y = value, col = variable)) +
  scale_y_continuous(limits = c(min(MAE_SS_us_df$value) - 0.05,
                                max(MAE_SS_us_df$value) + 0.05)) +
  scale_x_continuous(breaks = seq(1950, 2017, by = 5),
                     labels = seq(1950, 2017, by = 5)) +
  theme_bw() +
  ylab("Mean Absolute Error Skill Score") +
  xlab("Year") +
  scale_color_discrete(name = "Predictors") +
  ggtitle("US") +
  scale_fill_discrete(name = "Predictors")

MAE_us_plot_density = 
  ggplot() + 
  geom_density(data = MAE_SS_us_df %>% dplyr::filter(variable %in% c("PC1", "PC1-PC6", "NINO3.4")), aes(x = value, col = EN_phase)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(data = MAE_SS_us_df %>% dplyr::filter(variable %in% c("PC1", "PC1-PC6", "NINO3.4")),
             aes(xintercept = value_mean_phase, col = EN_phase), linetype = "dashed", size = 0.75, alpha = 0.75) +
  theme_bw() +
  xlab("Mean Absolute Error Skill Score") +
  ylab("Prob. Density") +
  scale_color_manual(name = "ENSO Phase", values = c("blue", "purple", "red")) +
  ggtitle("US") +
  facet_wrap(~variable)


MAE_SS_west_df = data.frame(t(MAE_SS_west), year = JFM_preds$year)
colnames(MAE_SS_west_df) = c("PC1", "PC1-PC6", "NINO3.4", "NINO3.4 & PNA", "NINO3.4 & NAO & PDO", "year")
MAE_SS_west_df = melt(MAE_SS_west_df, id.vars = "year")
MAE_SS_west_df$variable = factor(MAE_SS_west_df$variable)

MAE_SS_west_df = MAE_SS_west_df %>% dplyr::group_by(variable) %>%
                                    dplyr::mutate(value_mean = mean(value))

MAE_SS_west_df = merge(MAE_SS_west_df, ENSO_phase, by = "year") %>%
  dplyr::group_by(EN_phase,variable) %>%
  dplyr::mutate(value_mean_phase = mean(value))

MAE_west_plot = 
  ggplot() + 
  geom_vline(xintercept = c(ENSO_phase$year[ENSO_phase$EN_phase == "warm"]), col = "grey", alpha = 0.75, size = 3) +
  geom_vline(xintercept = 2018, linetype = "dashed") +
  geom_text(aes(x = 2022, y = (-0.075), label = "Mean skill")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_bar(data = MAE_SS_west_df, aes(x = 2022, y = value_mean, fill = variable), stat = "identity", position = "dodge", width = 3) +
  geom_line(data = MAE_SS_west_df, aes(x=year, y = value, col = variable)) +
  geom_point(data = MAE_SS_west_df, aes(x=year, y = value, col = variable)) +
  scale_y_continuous(limits = c(min(MAE_SS_west_df$value) - 0.05,
                                max(MAE_SS_west_df$value) + 0.05)) +
  scale_x_continuous(breaks = seq(1950, 2017, by = 5),
                     labels = seq(1950, 2017, by = 5)) +
  theme_bw() +
  ylab("Mean Absolute Error Skill Score") +
  xlab("Year") +
  scale_color_discrete(name = "Predictors") +
  ggtitle("West") +
  scale_fill_discrete(name = "Predictors")

MAE_west_plot_density = 
  ggplot() + 
  geom_density(data = MAE_SS_west_df %>% dplyr::filter(variable %in% c("PC1", "PC1-PC6", "NINO3.4")), aes(x = value, col = EN_phase)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(data = MAE_SS_west_df %>% dplyr::filter(variable %in% c("PC1", "PC1-PC6", "NINO3.4")),
             aes(xintercept = value_mean_phase, col = EN_phase), linetype = "dashed", size = 0.75, alpha = 0.75) +
  theme_bw() +
  xlab("Mean Absolute Error Skill Score") +
  ylab("Prob. Density") +
  scale_color_manual(name = "ENSO Phase", values = c("blue", "purple", "red")) +
  ggtitle("West") +
  facet_wrap(~variable)



# check how many of teh year's have positive skill scores
aggregate(data = HEIDKE_SS_us_df %>% dplyr::mutate(value_pos = ifelse(value > 0, 1, 0)), value_pos ~ variable, FUN = sum)
#                 variable value_pos
# 1                 PC1        50
# 2             PC1-PC6        64
# 3             NINO3.4        45
# 4       NINO3.4 & PNA        47
# 5 NINO3.4 & NAO & PDO        44

aggregate(data = HEIDKE_SS_west_df %>% dplyr::mutate(value_pos = ifelse(value > 0, 1, 0)), value_pos ~ variable, FUN = sum)
#                  variable value_pos
# 1                 PC1        48
# 2             PC1-PC6        63
# 3             NINO3.4        44
# 4       NINO3.4 & PNA        46
# 5 NINO3.4 & NAO & PDO        45

aggregate(data = MAE_SS_us_df %>% dplyr::mutate(value_pos = ifelse(value > 0, 1, 0)), value_pos ~ variable, FUN = sum)
#                   variable value_pos
# 1                 PC1        44
# 2             PC1-PC6        54
# 3             NINO3.4        42
# 4       NINO3.4 & PNA        47
# 5 NINO3.4 & NAO & PDO        38

aggregate(data = MAE_SS_west_df %>% dplyr::mutate(value_pos = ifelse(value > 0, 1, 0)), value_pos ~ variable, FUN = sum)
#                   variable value_pos
# 1                 PC1        43
# 2             PC1-PC6        53
# 3             NINO3.4        46
# 4       NINO3.4 & PNA        44
# 5 NINO3.4 & NAO & PDO        39

load(file = "data/Processed data/MAE_by_loc_plot.RData")
load(file = "data/Processed data/HEIDKE_by_loc_plot.RData")


plot_layout = rbind(c(1,2,3,4),
                    c(5,5,5,5),
                    c(6,6,6,6))

pdf("Final figures/Figure_2.pdf", width = 16, height = 10)
grid.arrange(HEIDKE_by_loc_plot,
             HEIDKE_us_plot,
             HEIDKE_west_plot,
             ncol = 1)
dev.off()

pdf("Final figures/Figure_S3.pdf", width = 16, height = 10)
grid.arrange(MAE_by_loc_plot,
             MAE_us_plot,
             MAE_west_plot,
             ncol = 1)
dev.off()

pdf("Final figures/Figure_S4.pdf", width = 10, height = 10)
grid.arrange(HEIDKE_us_plot_density,
             HEIDKE_west_plot_density,
             MAE_us_plot_density,
             MAE_west_plot_density,
             ncol = 1)
dev.off()

rm(list = ls())
