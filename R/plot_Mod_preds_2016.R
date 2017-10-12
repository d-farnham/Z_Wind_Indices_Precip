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

# try x-val for the NINO, PC1, and NAO models
cand_preds_xval = list(c("PC1"),
                       c("NINO3.4"),
                       c("PNA"),
                       c("NAO"))

# for(yyear in 1:nrow(JFM_preds)){
yyear = 67

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
  
  JFM_2016_precip = us_precip %>% data.table() %>%
    dplyr::filter(year == JFM_preds$year[yyear] &
                    month %in% c(1,2,3) &
                    !is.na(precip)) %>%
    dplyr::group_by(latitude,longitude) %>%
    dplyr::summarise(precip = sum(precip)) %>%
    data.frame()
  
  
  
  precip_pred = merge(JFM_precip,JFM_preds[JFM_preds$year != JFM_preds$year[yyear], ], by = "year")
  
  precip_2016_preds_all_list = list()
  
  for(preds in 1:length(cand_preds_xval)){
    precip_pred_sub = precip_pred[ , c("latitude", "longitude", "precip", c(cand_preds_xval[[preds]]))] 
    
    reg_all = plyr::ddply(precip_pred_sub, c("latitude","longitude"), function(x) coefficients(lm(precip ~., data=x[,-c(1,2)])))
    
    
    pred_2016 = JFM_preds[JFM_preds$year == JFM_preds$year[yyear], c(cand_preds_xval[[preds]])]
    coefs_reg_all = reg_all[ ,c(cand_preds_xval[[preds]])]
    
    precip_2016_preds_all = data.frame(reg_all[, c("latitude", "longitude")], 
                                       reg_all$`(Intercept)` +  as.matrix(coefs_reg_all) %*% t(as.matrix(pred_2016)))
    colnames(precip_2016_preds_all)[3] = "pred"
    precip_2016_preds_all_list[[preds]] = merge(merge(JFM_mean_precip, precip_2016_preds_all, by = c("latitude", "longitude")), 
                                  JFM_2016_precip, by = c("latitude", "longitude")) %>% dplyr::mutate(mod = cand_preds_xval[[preds]])
    
 #   precip_2016_preds_all_list[[preds]] = precip_2016_preds_all_list[[preds]] %>%
  }



  precip_2016_preds_all_list_collapse = do.call(rbind, precip_2016_preds_all_list)



usa_pts <- SpatialPoints(precip_2016_preds_all_list_collapse[,c("longitude","latitude")],proj4string = CRS(proj4string(usa)))
usa_ii = !is.na(over(usa_pts,usa))

us <- data.frame(map("state", plot=FALSE)[c("x","y")])
JFM_2016_obs_plot = ggplot() + 
  geom_tile(data = precip_2016_preds_all_list_collapse[c(usa_ii), ], aes(x = (longitude),y = latitude,fill=(precip - mean_precip)/sd_precip)) +
  geom_path(data=us, aes(x,y)) + 
  scale_y_continuous(limits = c(25,50)) + 
  scale_x_continuous(limits = c(-125,-66)) + 
  xlab("lon") + 
  ylab("lat") +
  scale_fill_gradient2(name="JFM P Std Anom", limits = c(-5,5), 
                       midpoint = 0,low="red", mid = "white",  high = "blue",na.value = "grey") +
  ggtitle("2016 Observation (JFM)") +
  theme_bw() +
#  facet_wrap(~mod) +
  theme(legend.position = "bottom")

JFM_2016_pred_plot = ggplot() + 
  geom_tile(data = precip_2016_preds_all_list_collapse[c(usa_ii), ], aes(x = (longitude),y = latitude,fill=(pred - mean_precip)/sd_precip)) +
  geom_path(data=us, aes(x,y)) + 
  scale_y_continuous(limits = c(25,50)) + 
  scale_x_continuous(limits = c(-125,-66)) + 
  xlab("lon") + 
  ylab("lat") +
  scale_fill_gradient2(name="JFM P Std Anom", limits = c(-2.75,2.75), 
                       midpoint = 0,low="red", mid = "white",  high = "blue",na.value = "grey") +
  ggtitle("2016 Pred (JFM)") +
  theme_bw() +
  facet_wrap(~mod) +
  theme(legend.position = "bottom")

JFM_2016_PU_SU_plot = ggplot() + 
  geom_tile(data = precip_2016_pred_PU_SU[c(usa_ii), ], aes(x = (longitude),y = latitude,fill=(pred - mean_precip)/sd_precip)) +
  geom_path(data=us, aes(x,y)) + 
  scale_y_continuous(limits = c(25,50)) + 
  scale_x_continuous(limits = c(-125,-66)) + 
  xlab("lon") + 
  ylab("lat") +
  scale_fill_gradient2(name="JFM P Std Anom", limits = c(-2.75,2.75), 
                       midpoint = 0,low="red", mid = "white",  high = "blue",na.value = "grey") +
  ggtitle("2016 PC1-PC6 Pred (JFM)") +
  theme_bw() +
  theme(legend.position = "bottom")

JFM_2016_NINO_plot = ggplot() + 
  geom_tile(data = precip_2016_pred_NINO[c(usa_ii), ], aes(x = (longitude),y = latitude,fill=(pred - mean_precip)/sd_precip)) +
  geom_path(data=us, aes(x,y)) + 
  scale_y_continuous(limits = c(25,50)) + 
  scale_x_continuous(limits = c(-125,-66)) + 
  xlab("lon") + 
  ylab("lat") +
  scale_fill_gradient2(name="JFM P Std Anom", limits = c(-2.75,2.75), 
                       midpoint = 0,low="red", mid = "white",  high = "blue",na.value = "grey") +
  ggtitle("2016 Nino3.4 Pred (JFM)") +
  theme_bw() +
  theme(legend.position = "bottom")



rm(list = ls())

