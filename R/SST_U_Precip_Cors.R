load('data/Processed data/SST_long.RData')

SST_long_d = SST_long %>% data.table() %>%
  mutate(year = year(date) + 1) %>%
  dplyr::filter(month %in% c(12)) %>%
  dplyr::group_by(latitude, longitude, year) %>%
  dplyr::summarise(SST = mean(SST)) %>%
  dplyr::group_by(latitude, longitude) %>%
  dplyr::mutate(SST_clim = mean(SST),
                SST_anom = SST - SST_clim) %>%
  data.frame()

# now let's detrend the SST
source("R/linear.detrend.R")

# NEED TO CHECK THAT THE BELOW WORKS HOW I THINK IT DOES
SST_long_d_detrend = SST_long_d %>% dplyr::filter(latitude > (-25) &
                                                    latitude < 65) %>%
  dplyr::group_by(latitude, longitude) %>%
  dplyr::filter(!is.na(SST_anom)) %>%
  dplyr::mutate(SST_anom_ld = linear.detrend(y = SST_anom, year = year))


load('data/Processed data/climate_ind.Rdata')

NINO_ind_d = climate_ind %>% dplyr::filter(month %in% c(12)) %>%
  dplyr::mutate(year = year +1) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(NINO3.4 = mean(NINO3.4),
                   NINO4 = mean(NINO4))

load("data/Processed data/JFM_U_preds.RData")

SST_dec_U_preds = merge(SST_long_d_detrend, JFM_U_preds, by = "year") %>%
  melt(id.vars = c("year", "latitude", "longitude", "PC1","SST", "SST_anom_ld", "SST_anom"))

SST_U_NINO_preds = merge(SST_dec_U_preds, NINO_ind_d, by = "year", all = T) %>%
  dplyr::select(year, latitude, longitude, SST, SST_anom_ld,SST_anom, NINO3.4, PC1) %>%
  dplyr::mutate(ENSO_phase = ifelse(NINO3.4 > quantile(NINO3.4, prob = 2/3, na.rm = TRUE), "warm", ifelse(NINO3.4 <= quantile(NINO3.4, prob = 1/3, na.rm = TRUE), "cool", "neutral")),
                PC1_phase = ifelse(PC1 > quantile(PC1, prob = 1/2, na.rm = TRUE), "high", ifelse(PC1 < quantile(PC1, prob = 1/2, na.rm = TRUE), "low", NA))) %>%
  dplyr::group_by(latitude, longitude, ENSO_phase, PC1_phase) %>%
  dplyr::summarise(SST_avg = mean(SST_anom, na.rm = TRUE),
                   n = length(SST_anom)/6)


SST_U_NINO_preds$PC1_phase = factor(SST_U_NINO_preds$PC1_phase, levels = c("low", "med", "high"))
SST_U_NINO_preds$PC1_phase_lab = SST_U_NINO_preds$PC1_phase
mask_nh = data.frame(ymin = 20, ymax = 60, xmin = 50, xmax = 359.999)


levels(SST_U_NINO_preds$PC1_phase_lab)<- c("PC1 < P[50](PC1)", NA, "PC1 > P[50](PC1)")

world <- data.frame(map("world", plot=FALSE)[c("x","y")])
world[world$x < 0 & !is.na(world$x),]$x = world[world$x < 0 & !is.na(world$x),]$x + 360

SST_by_ENSO_PC1_plot = 
ggplot() +
  geom_tile(data = SST_U_NINO_preds %>% dplyr::filter(!is.na(PC1_phase)), aes(x = (longitude),y = latitude, fill=(SST_avg))) +
  geom_contour(data = SST_U_NINO_preds %>% dplyr::filter(!is.na(PC1_phase)), aes(x = (longitude),y = latitude, z = SST_avg), breaks = c(seq(0.4,4,by = 0.4)), col = "black", linetype = "solid") +
  geom_contour(data = SST_U_NINO_preds %>% dplyr::filter(!is.na(PC1_phase)), aes(x = (longitude),y = latitude, z = SST_avg), breaks = c(seq(-0.4,-4,by = -0.4)), col = "black", linetype = "dashed") +
  geom_path(data=world, aes(x,y)) +
  geom_rect(data = mask_nh, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "grey", alpha = 0.75) +
  scale_y_continuous(limits = c(-20,60)) +
  scale_x_continuous(limits = c(50,359.999)) +
  geom_text(data = SST_U_NINO_preds %>% dplyr::filter(!is.na(PC1_phase)), aes(x = 95, y = 50, label = paste0("n = ",n)), size = 6) +
  xlab("lon") + 
  ylab("lat") +
  theme_bw() +
  scale_fill_gradient2(name="SST anom \n (deg C)", limits = c(-2,2), 
                       midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "white") +
  facet_grid(ENSO_phase~PC1_phase_lab, labeller=label_parsed) +
  theme(legend.position = "bottom") +
  ggtitle("Dec")

SST_U_NINO_dif =
merge(SST_U_NINO_preds %>% dplyr::filter(PC1_phase == "high") %>% 
        dplyr::select(latitude, longitude, ENSO_phase,SST_avg) %>%
        setNames(c("latitude", "longitude", "ENSO_phase","PC1_high_SST_avg")),
      SST_U_NINO_preds %>% dplyr::filter(PC1_phase == "low") %>% 
        dplyr::select(latitude, longitude, ENSO_phase,SST_avg) %>%
        setNames(c("latitude", "longitude", "ENSO_phase","PC1_low_SST_avg")),
      by = c("latitude", "longitude", "ENSO_phase")) %>% dplyr::filter(latitude > (-20) &
                                                                       latitude < 20) %>% 
  dplyr::mutate(PC1_SST_avg_dif = PC1_high_SST_avg - PC1_low_SST_avg) %>%
  dplyr::group_by(ENSO_phase) %>%
  dplyr::mutate(mean_PC1_SST_avg_dif = mean(PC1_SST_avg_dif)) %>%
  dplyr::mutate(PC1_SST_avg_dif2 = PC1_SST_avg_dif - mean_PC1_SST_avg_dif)
  

SST_dif_by_ENSO_PC1_plot = 
ggplot() +
  geom_tile(data = SST_U_NINO_dif, aes(x = (longitude),y = latitude, fill=(PC1_SST_avg_dif2))) +
  geom_contour(data = SST_U_NINO_dif, aes(x = (longitude),y = latitude,z = PC1_SST_avg_dif2), breaks = c(seq(0.4,2,by = 0.4)), col = "black", linetype = "solid") +
  geom_contour(data = SST_U_NINO_dif, aes(x = (longitude),y = latitude,z = PC1_SST_avg_dif2), breaks = c(seq(-0.4,-2,by = -0.4)), col = "black", linetype = "dashed") +
  geom_path(data=world, aes(x,y)) +
  geom_rect(data = mask_nh, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "grey", alpha = 0.75) +
  scale_y_continuous(limits = c(-20,60)) +
  scale_x_continuous(limits = c(50,359.999)) +
  xlab("lon") + 
  ylab("lat") +
  theme_bw() +
  scale_fill_gradient2(name="SST* anom \n (deg C)", limits = c(-0.75,0.75), 
                       midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "white") +
  facet_wrap(~ENSO_phase,
             ncol = 1) +
  theme(legend.position = "bottom") +
  ggtitle("Difference between PC1 phases (Dec)")

pdf(file = 'Final figures/Figure_4.pdf', width = 12, height = 6)
grid.arrange(SST_by_ENSO_PC1_plot,
             SST_dif_by_ENSO_PC1_plot,
             layout_matrix = rbind(c(1,1,2)))
dev.off()




# now lets compute the correlations b/t Dec SSTs and JFM zonal wind indices
load('data/Processed data/SST_long.RData')

SST_long_dec = SST_long %>% data.table() %>%
  mutate(year = year(date) + 1) %>%
  dplyr::filter(year > 1949 &
                  year < 2017) %>%
  dplyr::filter(month %in% c(12)) %>%
  dplyr::group_by(latitude, longitude, year) %>%
  dplyr::summarise(SST = mean(SST)) %>%
  dplyr::group_by(latitude, longitude) %>%
  dplyr::mutate(SST_clim = mean(SST),
                SST_anom = SST - SST_clim) %>%
  data.frame()

# let's save this dec SST time-series
save(SST_long_dec, file = 'data/Processed data/SST_long_dec.RData')

load("data/Processed data/JFM_U_preds.RData")

SST_dec_U_preds = merge(SST_long_dec, JFM_U_preds, by = "year") %>%
  melt(id.vars = c("year", "latitude", "longitude", "SST", "SST_clim", "SST_anom"))

# now check the rank cors after detrending both SST and PU/SU
source("R/linear.detrend.R")

SST_dec_U_preds = SST_dec_U_preds %>% dplyr::group_by(latitude, longitude, variable) %>%
  dplyr::filter(!is.na(SST_anom)) %>%
  dplyr::mutate(SST_anom_ld = linear.detrend(y = SST_anom,year = year),
                value_ld = linear.detrend(y = value,year = year))

# now check the rank cors
SST_U_cors = ddply(SST_dec_U_preds %>% dplyr::filter(!is.na(SST_anom_ld)), .(latitude, longitude, variable), summarize, cor = cor(SST_anom_ld, value_ld, method = "spearman")) %>%
  dplyr::mutate(var = paste0(variable," & SST or ", variable," & Precip"))


SST_U_cors_p = ddply(SST_dec_U_preds %>% dplyr::filter(!is.na(SST_anom_ld)), .(latitude, longitude, variable), 
                      function(x) cor.test(x$SST_anom_ld,x$value_ld, method = "spearman", use = "complete.obs")$p.value) %>%
  setNames(c("latitude", "longitude", "variable", "p_value")) %>%
  dplyr::mutate(var = paste0(variable," & SST or ", variable," & Precip"))

SST_PC_cors = merge(SST_U_cors,SST_U_cors_p, by = c("latitude", "longitude", "var", "variable"))

# now do the same for precipitation and PU/SU
load('data/Processed data/us_precip.Rdata')

JFM_precip = us_precip %>% data.table() %>%
  dplyr::filter(month %in% c(1,2,3)) %>%
  dplyr::filter(!is.na(precip)) %>%
  dplyr::group_by(latitude,longitude,year) %>%
  dplyr::summarise(precip = sum(precip)) %>%
  data.frame()

precip_U_preds = merge(JFM_precip, JFM_U_preds, by = "year")  %>%
  melt(id.vars = c("year", "latitude", "longitude", "precip"))

source("R/linear.detrend.R")
precip_U_preds = precip_U_preds %>% dplyr::group_by(latitude, longitude, variable) %>%
  dplyr::mutate(precip_ld = linear.detrend(y = precip, year = year),
                value_ld = linear.detrend(y = value,year = year)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(longitude = ifelse(longitude > 180, longitude - 360, longitude)) %>%
  data.frame()




# now check the rank cors
precip_U_cors = ddply(precip_U_preds, .(latitude, longitude, variable), summarize, cor = cor(precip_ld, value_ld, method = "spearman")) %>%
  dplyr::mutate(var = paste0(variable," & SST or ", variable," & Precip"))


precip_U_cors_p = ddply(precip_U_preds %>% dplyr::filter(!is.na(precip_ld)), .(latitude, longitude, variable),
                        function(x) cor.test(x$precip_ld,x$value_ld, method = "spearman", use = "complete.obs")$p.value) %>%
  setNames(c("latitude", "longitude", "variable", "p_value")) %>%
  dplyr::mutate(var = paste0(variable," & SST or ", variable," & Precip"))

precip_PC_cors = merge(precip_U_cors, precip_U_cors_p, by = c("latitude", "longitude", "var", "variable"))


us <- data.frame(map("state", plot=FALSE)[c("x","y")])
# find the grids inside the US
usa = map("world",regions = "usa", plot = FALSE, fill = TRUE)
require(sp)
require(maptools)

usa_IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs = usa_IDs, proj4string = CRS("+proj=longlat +datum=WGS84"))
usa_pts <- SpatialPoints(precip_PC_cors[,c("longitude","latitude")],proj4string = CRS(proj4string(usa)))
usa_ii = !is.na(over(usa_pts,usa))

precip_PC_cors = precip_PC_cors[c(usa_ii),]

precip_PC_cors[precip_PC_cors$longitude < 0 & !is.na(precip_PC_cors$longitude),]$longitude = precip_PC_cors[precip_PC_cors$longitude < 0 & !is.na(precip_PC_cors$longitude),]$longitude + 360

mask_nh = data.frame(ymin = 20, ymax = 60, xmin = 50, xmax = 359.999)

world <- data.frame(map("world", plot=FALSE)[c("x","y")])
world[world$x < 0 & !is.na(world$x),]$x = world[world$x < 0 & !is.na(world$x),]$x + 360

SST_Dec_U_cor_plot = 
  ggplot() +
  geom_tile(data = SST_PC_cors, aes(x = (longitude),y = latitude, fill=(cor))) +
  geom_point(data = SST_PC_cors %>% dplyr::filter(p_value < 0.05), 
             aes(x = (longitude),y = latitude), shape = "x", size = 0.5, alpha = 0.5) +
  geom_rect(data = mask_nh, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "grey", alpha = 0.75) +
  geom_point(data = precip_PC_cors, aes(x = (longitude),y = latitude, col=(cor)), shape = 15, size = 0.2) +
  geom_point(data = precip_PC_cors %>% dplyr::filter(p_value < 0.05),
             aes(x = (longitude),y = latitude), shape = "x", size = 0.5, alpha = 0.5) +
  geom_path(data=world, aes(x,y)) +
  scale_y_continuous(limits = c(-20,60)) +
  scale_x_continuous(limits = c(50,359.999)) +
  xlab("lon") + 
  ylab("lat") +
  theme_bw() +
  scale_color_gradient2(name="P rank cor", limits = c(-0.75,0.75),
                        midpoint = 0,low="sienna4", mid = "white",  high = "springgreen4",na.value = "white") +
  scale_fill_gradient2(name="rank cor", limits = c(-1,1), 
                       midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "white") +
  facet_wrap(~var,
             ncol = 3) +
  ggtitle("Rank correlation")


# now look at the partial correlation of SST with U preds given NINO3.4
load('data/Processed data/climate_ind.Rdata')
NINO_ind_D = climate_ind %>% dplyr::filter(month %in% c(12)) %>%
  dplyr::group_by(year = year + 1) %>%
  dplyr::summarise(NINO3.4 = mean(NINO3.4),
                   NINO4 = mean(NINO4))

SST_dec_U_NINO_preds = merge(SST_dec_U_preds, NINO_ind_D, by = "year", all = T) %>%
  dplyr::select(year, latitude, longitude, SST_anom_ld, variable, value_ld, NINO3.4, NINO4) 

library(ppcor)
SST_dec_PC1_NINO_cors = ddply(SST_dec_U_NINO_preds %>% dplyr::filter(variable == "PC1" &
                                                               !is.na(SST_anom_ld)), .(latitude, longitude, variable), function(x) pcor.test(x$SST_anom_ld,x$value_ld,x$NINO3.4, method = "spearman")) %>%
  dplyr::mutate(var = paste0(variable," & SST | Nino3.4 or ", variable," & Precip | Nino3.4"))

SST_dec_PC_NINO_cors = SST_dec_PC1_NINO_cors


precip_U_NINO_preds = merge(precip_U_preds, NINO_ind_D, by = "year", all = T)

library(ppcor)
precip_PC1_NINO3.4_cors = ddply(precip_U_NINO_preds %>% dplyr::filter(variable == "PC1" &
                                                                        !is.na(precip_ld)), .(latitude, longitude, variable), function(x) pcor.test(x$precip_ld,x$value_ld,x$NINO3.4, method = "spearman")) %>%
  dplyr::mutate(var = paste0(variable," & SST | Nino3.4 or ", variable," & Precip | Nino3.4"))

precip_PC_NINO_cors = precip_PC1_NINO3.4_cors

us <- data.frame(map("state", plot=FALSE)[c("x","y")])
# find the grids inside the US
usa = map("world",regions = "usa", plot = FALSE, fill = TRUE)

usa_IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs = usa_IDs, proj4string = CRS("+proj=longlat +datum=WGS84"))
usa_pts <- SpatialPoints(precip_PC_NINO_cors[,c("longitude","latitude")],proj4string = CRS(proj4string(usa)))
usa_ii = !is.na(over(usa_pts,usa))

precip_PC_NINO_cors = precip_PC_NINO_cors[c(usa_ii),]

precip_PC_NINO_cors[precip_PC_NINO_cors$longitude < 0 & !is.na(precip_PC_NINO_cors$longitude),]$longitude = precip_PC_NINO_cors[precip_PC_NINO_cors$longitude < 0 & !is.na(precip_PC_NINO_cors$longitude),]$longitude + 360

SST_Dec_NINO_cor_plot =
  ggplot() +
  geom_tile(data = SST_dec_PC_NINO_cors, aes(x = (longitude),y = latitude, fill=(estimate))) +
  geom_point(data = SST_dec_PC_NINO_cors %>% dplyr::filter(p.value < 0.05),
             aes(x = (longitude),y = latitude), shape = "x", size = 0.5, alpha = 0.5) +
  geom_rect(data = mask_nh, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "grey", alpha = 0.75) +
  geom_point(data = precip_PC_NINO_cors, aes(x = (longitude),y = latitude, col=(estimate)), shape = 15, size = 0.2) +
  geom_point(data = precip_PC_NINO_cors %>% dplyr::filter(p.value < 0.05),
             aes(x = (longitude),y = latitude), shape = "x", size = 0.5, alpha = 0.5) +
  geom_path(data=world, aes(x,y)) +
  scale_y_continuous(limits = c(-20,60)) +
  scale_x_continuous(limits = c(50,359.999)) +
  xlab("lon") +
  ylab("lat") +
  theme_bw() +
  scale_color_gradient2(name="P rank cor", limits = c(-0.75,0.75),
                        midpoint = 0,low="sienna4", mid = "white",  high = "springgreen4",na.value = "white") +
  scale_fill_gradient2(name="SST rank cor", limits = c(-0.65,0.65),
                       midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "white") +
  facet_wrap(~var,
             ncol = 1) +
  ggtitle("Partial rank correlation")

layout_mat = cbind(1,1,1,1,2,2,2)
pdf(file = 'Final figures/Figure_S8.pdf', width = 16, height = 3.5)
grid.arrange(SST_Dec_U_cor_plot,
             SST_Dec_NINO_cor_plot,
             layout_matrix = layout_mat)
dev.off()

rm(list = ls())
