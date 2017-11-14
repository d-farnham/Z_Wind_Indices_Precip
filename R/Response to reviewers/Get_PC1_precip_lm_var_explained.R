load("data/Processed data/us_precip.Rdata")
load("data/Processed data/climate_ind.Rdata")
load("data/Processed data/JFM_U_preds.RData")

# # load functions to calcuate model skill
# source("R/Get_skill.R")
# 
# # identify the El Nino events
# warm_years = climate_ind %>% dplyr::mutate(NINO3.4_smoothed = stats::filter(NINO3.4, rep(1, 3)/3, sides = 2)) %>%
#   dplyr::filter(month == 1 &
#                   NINO3.4_smoothed > 1) %>%
#   dplyr::select(year)

climate_ind_JFM = climate_ind %>% dplyr::filter(month %in% c(1,2,3)) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(NINO3.4 = mean(NINO3.4),
                   PNA = mean(PNA),
                   NAO = mean(NAO),
                   PDO = mean(PDO))
JFM_preds = merge(climate_ind_JFM, 
                  JFM_U_preds,
                  by = c("year")) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise_each(funs(mean))



JFM_mean_precip = us_precip %>% data.table() %>%
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
    dplyr::filter(month %in% c(1,2,3)) %>%
    dplyr::filter(!is.na(precip)) %>%
    dplyr::group_by(latitude,longitude,year) %>%
    dplyr::summarise(precip = sum(precip)) %>%
    data.frame()
  
  
  precip_pred = merge(JFM_precip,JFM_preds, by = "year")
  
  
  precip_pred_sub = precip_pred[ , c("latitude", "longitude", "precip", "PC1", "NINO3.4")] 
  
  precip_pred_sub_long = melt(precip_pred_sub, id.vars = c("latitude", "longitude", "precip"))
  
  r_sq_all = plyr::ddply(precip_pred_sub_long, c("variable","latitude","longitude"), function(x) summary(lm(x$precip ~ x$value))$"r.squared") %>% 
    setNames(c("variable","latitude","longitude","r_sq"))
  
  p_val_all = plyr::ddply(precip_pred_sub_long, c("variable","latitude","longitude"), function(x) summary(lm(x$precip ~ x$value))$coefficients[2,"Pr(>|t|)"]) %>% 
    setNames(c("variable","latitude","longitude","p_val"))
  # only select the locations inside of CONUS
  
  us <- data.frame(map("state", plot=FALSE)[c("x","y")])
  
  # find the grids inside CONUS
  usa = map("world",regions = "usa", plot = FALSE, fill = TRUE)
  require(sp)
  require(maptools)
  
  usa_IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
  usa <- map2SpatialPolygons(usa, IDs = usa_IDs, proj4string = CRS("+proj=longlat +datum=WGS84"))
  usa_pts <- SpatialPoints(p_val_all[,c("longitude","latitude")],proj4string = CRS(proj4string(usa)))
  usa_ii = !is.na(over(usa_pts,usa))
  
  
  p_val_CONUS = p_val_all[c(usa_ii),]

# what is the proportion of grid cells inside of CONUS where JFM PC1 and NINO3.4 "significantly" explains JFM precip
  p_val_CONUS %>% dplyr::mutate(sig = ifelse(p_val < 0.05, 1, 0)) %>%
                  dplyr::group_by(variable) %>%
                  dplyr::summarise(prop.sig = mean(sig))
  # # A tibble: 2 × 2
  # variable  prop.sig
  # <fctr>     <dbl>
  #   1      PC1 0.5676647
  # 2  NINO3.4 0.4754491  
  
  
  r_sq_CONUS = r_sq_all[c(usa_ii),]

  
  
# what are the stistics of the percent of varaince explained 
r_sq_CONUS %>% dplyr::group_by(variable) %>%
               dplyr::summarise(percent_explained_mean = mean(r_sq)*100,
                                percent_explained_median = median(r_sq)*100,
                                percent_explained_1 = quantile(r_sq, probs = 0.1)*100,
                                percent_explained_9 = quantile(r_sq, probs = 0.9)*100)
  
# # A tibble: 2 × 5
# variable percent_explained_mean percent_explained_median percent_explained_1 percent_explained_9
# <fctr>                  <dbl>                    <dbl>               <dbl>               <dbl>
#   1      PC1               10.82915                 8.216420           0.4011893            25.83535
# 2  NINO3.4                7.58008                 5.318904           0.1818065            18.36060

rm(list = ls())
