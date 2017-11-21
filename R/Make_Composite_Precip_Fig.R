load("data/Processed data/us_precip.Rdata")
load("data/Processed data/climate_ind.Rdata")


JFM_precip = us_precip %>% data.table() %>%
  dplyr::filter(month %in% c(1,2,3) &
                year > 1949) %>%
  dplyr::filter(!is.na(precip)) %>%
  dplyr::group_by(latitude,longitude,year) %>%
  dplyr::summarise(precip = sum(precip)) %>%
  dplyr::group_by(latitude,longitude) %>%
  dplyr::mutate(precip_clim = mean(precip),
                precip_anom = (precip - precip_clim)/sd(precip)) %>%
  data.frame()


# identify the warm/cool events
ENSO_phase = climate_ind %>% dplyr::mutate(NINO3.4_smoothed = as.numeric(stats::filter(NINO3.4, rep(1, 3)/3, sides = 2))) %>%
  dplyr::filter(month == 1) %>%
  dplyr::mutate(ENSO_phase = ifelse(NINO3.4_smoothed > 1, "warm",
                                    ifelse(NINO3.4_smoothed < (-1), "cool", "neutral"))) %>%
  dplyr::select(year, ENSO_phase, NINO3.4_smoothed)

# how many EN/LN/neutral events
aggregate(year ~ ENSO_phase, FUN = length, data = ENSO_phase)
#     ENSO_phase year
# 1       cool   11
# 2    neutral   45
# 3       warm   11

# compute the composite average JFM precip by ENSO phase (excluding 2016)
JFM_ENSO_precip_non_2016 = merge(JFM_precip, ENSO_phase, by = "year") %>%
  dplyr::filter(!is.na(ENSO_phase) &
                  year != 2016) %>%
  dplyr::mutate(ENSO_phase = factor(ENSO_phase, levels = c("warm", "neutral", "cool"))) %>%
  dplyr::group_by(latitude, longitude, ENSO_phase) %>%
  dplyr::summarise(precip_anom = mean(precip_anom)) %>%
  data.frame()

# now retain the JFM precip for individual years of interest
JFM_precip_2016 = JFM_precip %>% dplyr::filter(year == 2016) %>%
                                 dplyr::mutate(ENSO_phase = 2016) %>%
                                 dplyr::select(latitude, longitude, ENSO_phase, precip_anom) %>%
                                 data.frame()

JFM_precip_1998 = JFM_precip %>% dplyr::filter(year == 1998) %>%
                                 dplyr::mutate(ENSO_phase = 1998) %>%
                                 dplyr::select(latitude, longitude, ENSO_phase, precip_anom) %>%
                                 data.frame()

JFM_precip_1983 = JFM_precip %>% dplyr::filter(year == 1983) %>%
                                 dplyr::mutate(ENSO_phase = 1983) %>%
                                 dplyr::select(latitude, longitude, ENSO_phase, precip_anom) %>%
                                 data.frame()

JFM_precip_2003 = JFM_precip %>% dplyr::filter(year == 2003) %>%
                                 dplyr::mutate(ENSO_phase = 2003) %>%
                                 dplyr::select(latitude, longitude, ENSO_phase, precip_anom) %>%
                                 data.frame()

# concatanate the data.frames for plotting
JFM_ENSO_precip_all = rbind(JFM_precip_1998,JFM_precip_2016,JFM_ENSO_precip_non_2016,JFM_precip_1983,JFM_precip_2003) %>%
                          dplyr::filter(ENSO_phase != "neutral")



# only include cells inside of CONUS
usa = map("world",regions = "usa", plot = FALSE, fill = TRUE)
require(sp)
require(maptools)
usa_IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs = usa_IDs, proj4string = CRS("+proj=longlat +datum=WGS84"))
usa_pts <- SpatialPoints(JFM_ENSO_precip_all[,c("longitude","latitude")],proj4string = CRS(proj4string(usa)))
usa_ii = !is.na(over(usa_pts,usa))


world <- data.frame(map("world", plot=FALSE)[c("x","y")])
state <- data.frame(map("state", plot=FALSE)[c("x","y")])

JFM_ENSO_precip_all = JFM_ENSO_precip_all %>% dplyr::mutate(ENSO_phase = factor(ENSO_phase, levels = c("warm", "cool", "1983", "2003", "1998", "2016")))

EN_LN_1983_1998_2003_2016_precip_plot = 
ggplot() + 
  geom_tile(data = JFM_ENSO_precip_all[c(usa_ii),], aes(x = (longitude),y = latitude, fill=precip_anom)) +
  geom_path(data=world, aes(x,y), size = 0.33) + 
  geom_path(data=state, aes(x,y), size = 0.33) + 
  scale_y_continuous(limits = c(25,50)) +
  scale_x_continuous(limits = c(-125,-66), labels = NULL) +
  xlab("lon") + 
  ylab("lat") +
  scale_fill_gradient2(name="JFM P std anom", limits = c(-4.4,4.4), 
                       midpoint = 0,low="sienna4", mid = "white",  high = "springgreen4",na.value = "white") +
  theme_bw() +
  theme(legend.position = "bottom") +
  ggtitle("Precipitation by ENSO phase and 1983/1998/2003/2016") +
  facet_wrap(~ENSO_phase,
             ncol = 2)

pdf(file = 'Final figures/Figure_S1.pdf', width = 8, height = 8)
print(EN_LN_1983_1998_2003_2016_precip_plot)
dev.off()

rm(list = ls())
