load("data/Processed data/us_precip.Rdata")
load("data/Processed data/climate_ind.Rdata")
load("data/Processed data/JFM_U_preds.RData")

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


# cors of PC1 with PNA, NAO, and NINO3.4
cor.test(JFM_preds$PNA, JFM_preds$PC1, method = "pearson") # 0.6198233
cor.test(JFM_preds$NAO, JFM_preds$PC1, method = "pearson") # -0.488492
cor.test(JFM_preds$NINO3.4, JFM_preds$PC1, method = "pearson") # 0.6564319 


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
  
  
precip_pred_sub = precip_pred[ , c("latitude", "longitude", "precip", "PC1", "PC2", "PC3", "PC4", "PC5", "PC6", "NINO3.4", "PNA", "NAO", "PDO")] 
  
precip_pred_sub_long = melt(precip_pred_sub, id.vars = c("latitude", "longitude", "precip"))
  
cor_all_est = plyr::ddply(precip_pred_sub_long, c("variable","latitude","longitude"), function(x) cor.test(x$value, x$precip)$"estimate") %>% 
    setNames(c("variable","latitude","longitude","cor"))
  
cor_all_p = plyr::ddply(precip_pred_sub_long, c("variable","latitude","longitude"), function(x) cor.test(x$value, x$precip)$"p.value")  %>% 
    setNames(c("variable","latitude","longitude","p"))
  
cor_all = merge(cor_all_est, cor_all_p, by = c("variable", "latitude", "longitude")) %>%
                  dplyr::mutate(sig = ifelse(p < 0.05, 1, 0))

us <- data.frame(map("state", plot=FALSE)[c("x","y")])

# find the grids inside CONUS
usa = map("world",regions = "usa", plot = FALSE, fill = TRUE)
require(sp)
require(maptools)

usa_IDs <- sapply(strsplit(usa$names, ":"), function(x) x[1])
usa <- map2SpatialPolygons(usa, IDs = usa_IDs, proj4string = CRS("+proj=longlat +datum=WGS84"))
usa_pts <- SpatialPoints(cor_all[,c("longitude","latitude")],proj4string = CRS(proj4string(usa)))
usa_ii = !is.na(over(usa_pts,usa))

col_lim = max(abs(cor_all[c(usa_ii),]$cor))

pred_cor_sig_plot =
ggplot() + 
  geom_tile(data = cor_all[c(usa_ii),], aes(x = (longitude),y = latitude, fill=cor)) +
  geom_path(data=us, aes(x,y)) + 
  geom_point(data = cor_all[c(usa_ii),] %>% dplyr::filter(sig == 1), aes(x = (longitude),y = latitude), shape="x") +
  scale_y_continuous(limits = c(25,50)) + 
  scale_x_continuous(limits = c(-125,-66)) + 
  xlab("lon") + 
  ylab("lat") +
  scale_fill_gradient2(name="cor", limits = c(-col_lim,col_lim), 
                       midpoint = 0,low="sienna4", mid = "white",  high = "springgreen4",na.value = "white") +
  theme_bw() +
  facet_wrap(~variable, ncol = 5)

pdf("Final figures/Figure_S2.pdf", width = 18, height = 4.95)
grid.arrange(pred_cor_sig_plot)
dev.off()

rm(list = ls())
