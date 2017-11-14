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
																									 PNA = mean(PNA),
																									 NAO = mean(NAO))
JFM_preds = merge(climate_ind_JFM, 
									JFM_U_preds,
									by = c("year")) %>%
	dplyr::group_by(year) %>%
	dplyr::summarise_each(funs(mean))


# try x-val for the NINO, PC1, and PC1-PC6 models

cand_preds_xval = list(c("PC1"),
                       c("PC1","PC2","PC3","PC4", "PC5", "PC6"),
											 c("NINO3.4"))

precip_pred_obs = data.frame(
              year = rep(JFM_preds$year, each = nrow(unique(us_precip[,c("latitude", "longitude")]))),
              unique(us_precip[,c("latitude", "longitude")]),
              pred = NA,
              mean_precip = NA,
              precip = NA,
              sd_precip = NA,
              lower_tercile = NA,
              upper_tercile = NA)

precip_pred_obs = precip_pred_obs %>% dplyr::arrange(year, longitude, -latitude)

precip_pred_obs_list = list(precip_pred_obs,precip_pred_obs,precip_pred_obs)

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
		
		precip_2016_preds_all = precip_2016_preds_all %>% dplyr::arrange(longitude, -latitude)
		
		precip_pred_obs_list[[preds]][precip_pred_obs_list[[preds]]$year == JFM_preds$year[yyear],"pred"] = precip_2016_preds_all$pred
		precip_pred_obs_list[[preds]][precip_pred_obs_list[[preds]]$year == JFM_preds$year[yyear],"mean_precip"] = precip_2016_preds_all$mean_precip
		precip_pred_obs_list[[preds]][precip_pred_obs_list[[preds]]$year == JFM_preds$year[yyear],"precip"] = precip_2016_preds_all$precip
		precip_pred_obs_list[[preds]][precip_pred_obs_list[[preds]]$year == JFM_preds$year[yyear],"sd_precip"] = precip_2016_preds_all$sd_precip
		precip_pred_obs_list[[preds]][precip_pred_obs_list[[preds]]$year == JFM_preds$year[yyear],"lower_tercile"] = precip_2016_preds_all$lower_tercile
		precip_pred_obs_list[[preds]][precip_pred_obs_list[[preds]]$year == JFM_preds$year[yyear],"upper_tercile"] = precip_2016_preds_all$upper_tercile
	}
	print(paste0("Completed x-val on EN year ",JFM_preds$year[yyear]))
}


# now calcualte the skill by lat/lon
skill_lat_lon = data.frame(unique(precip_pred_obs_list[[1]] %>% dplyr::select(c(latitude, longitude))),
                           MAE = NA,
                           HEIDKE = NA)


cand_preds_names = c("PC1", "PC1-PC6", "NINO3.4")

skill_lat_lon_list = list()
for(ii in 1:length(cand_preds_names)) skill_lat_lon_list[[ii]] = skill_lat_lon

for(ppred in 1:3){
for(lloc in 1:nrow(skill_lat_lon)){
tmp_data = precip_pred_obs_list[[ppred]] %>% dplyr::filter(latitude == skill_lat_lon$latitude[lloc] &
                                                           longitude == skill_lat_lon$longitude[lloc])

skill_lat_lon_list[[ppred]]$MAE[lloc] = MAE_SS(tmp_data$pred, 
       tmp_data$mean_precip, 
       tmp_data$precip,
       tmp_data$sd_precip)

skill_lat_lon_list[[ppred]]$HEIDKE[lloc] = HEIDKE_SS(tmp_data$pred, 
                                                        tmp_data$lower_tercile, 
                                                        tmp_data$upper_tercile,
                                                        tmp_data$precip)

skill_lat_lon_list[[ppred]]$preds = cand_preds_names[ppred]
}
}
skill_lat_lon_long = do.call("rbind", skill_lat_lon_list) %>% melt(id.vars = c("latitude", "longitude","preds"))

us <- data.frame(map("state", plot=FALSE)[c("x","y")])

# find the grids inside CONUS
usa = map("world",regions = "usa", plot = FALSE, fill = TRUE)

usa_IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs = usa_IDs, proj4string = CRS("+proj=longlat +datum=WGS84"))
usa_pts <- SpatialPoints(skill_lat_lon_long[,c("longitude","latitude")],proj4string = CRS(proj4string(usa)))
usa_ii = !is.na(over(usa_pts,usa))

MAE_by_loc_plot =
 ggplot() + 
	geom_tile(data = skill_lat_lon_long[c(usa_ii),] %>% dplyr::filter(variable == "MAE"), aes(x = (longitude),y = latitude, fill=value)) +
	geom_path(data=us, aes(x,y)) + 
	scale_y_continuous(limits = c(25,50)) + 
	scale_x_continuous(limits = c(-125,-66)) + 
	xlab("lon") + 
	ylab("lat") +
	scale_fill_gradient2(name="Skill", limits = c(-max(abs(skill_lat_lon_long[c(usa_ii),]$value)),max(abs(skill_lat_lon_long[c(usa_ii),]$value))), 
											 midpoint = 0,low="red", mid = "white",  high = "green",na.value = "grey") +
  theme_bw() +
  facet_grid(~preds) +
  ggtitle("MAE Skill Score by location")

HEIDKE_by_loc_plot =
  ggplot() + 
  geom_tile(data = skill_lat_lon_long[c(usa_ii),] %>% dplyr::filter(variable == "HEIDKE"), aes(x = (longitude),y = latitude, fill=value)) +
  geom_path(data=us, aes(x,y)) + 
  scale_y_continuous(limits = c(25,50)) + 
  scale_x_continuous(limits = c(-125,-66)) + 
  xlab("lon") + 
  ylab("lat") +
  scale_fill_gradient2(name="Skill", limits = c(-max(abs(skill_lat_lon_long[c(usa_ii),]$value)),max(abs(skill_lat_lon_long[c(usa_ii),]$value))), 
                       midpoint = 0,low="red", mid = "white",  high = "green",na.value = "grey") +
  theme_bw() +
  facet_grid(~preds) +
  ggtitle("HEIDKE Skill Score by location")

save(MAE_by_loc_plot, file = "data/Processed data/MAE_by_loc_plot.RData")
save(HEIDKE_by_loc_plot, file = "data/Processed data/HEIDKE_by_loc_plot.RData")

rm(list = ls())
