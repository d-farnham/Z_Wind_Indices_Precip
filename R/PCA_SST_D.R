# load the SST and compute the PCA
load("data/Processed data/SST_long.RData")

# run PCA on U data
SST_long_D = SST_long %>% data.table() %>%
  mutate(year = year(date)) %>%
  dplyr::filter(latitude > -10) %>%
  dplyr::filter(latitude < 10) %>%
  dplyr::filter(longitude > 40) %>%
  dplyr::filter(longitude < 295) %>%
  dplyr::filter(month %in% c(12)) %>%
  dplyr::group_by(latitude, longitude) %>%
  dplyr::mutate(SST_clim = mean(SST),
                SST_anom = SST - SST_clim) %>%
  dplyr::filter(!is.na(SST)) %>%
  data.frame()

# let's detrend the SST now
source("R/linear.detrend.R")

SST_long_D = SST_long_D %>% dplyr::group_by(latitude, longitude) %>%
  dplyr::mutate(SST_ld = linear.detrend(y = SST_anom, year = year)) 

D_SST = dcast(data = SST_long_D, year ~ longitude + latitude, value.var = "SST_ld")

D_SST_PCA = prcomp(D_SST[,-1], scale. = TRUE)


100*round((D_SST_PCA$sdev/sum(D_SST_PCA$sdev))[1:10],3)
# [1] 19.1  8.3  7.3  6.1  4.8  4.6  3.8  3.5  3.3  2.8

lat_lon = data.frame(matrix(unlist(strsplit(names(D_SST)[-1],"_")), ncol = 2, byrow=TRUE))
colnames(lat_lon) = c("longitude","latitude")
lat_lon$longitude = as.numeric(as.character(lat_lon$longitude))
lat_lon$latitude = as.numeric(as.character(lat_lon$latitude))

# how many PCs to retain?
pc.retain = 4

D_loadings = cbind(lat_lon,D_SST_PCA$rotation[,1:pc.retain])
D_loadings_long = melt(D_loadings, id.vars = c("longitude","latitude"))

world <- data.frame(map("world", plot=FALSE)[c("x","y")])
world[world$x < 0 & !is.na(world$x),]$x = world[world$x < 0 & !is.na(world$x),]$x + 360

state <- data.frame(map("state", plot=FALSE)[c("x","y")])
state[state$x < 0 & !is.na(state$x),]$x = state[state$x < 0 & !is.na(state$x),]$x + 360

D_SST_PCA_plot = ggplot() +
  geom_tile(data = D_loadings_long, aes(x = (longitude),y = latitude,fill=(value))) +
  geom_path(data=world, aes(x,y), size = 0.25) +  
  scale_y_continuous(limits = c(-15,35)) + 
  scale_x_continuous(limits = c(1,360)) + 
  xlab("lon") + 
  ylab("lat") +
  #coord_map("ortho", orientation=c(40, 260, 0)) +
  scale_fill_gradient2(name="loading", limits = c(-0.1,0.1), 
                       midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "grey") +
  ggtitle("SST EOF") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~variable,
             ncol = pc.retain)

# make the SST predictors # !!! NOTE THAT I SCALED THE U FIELD HERE
years = D_SST$year
D_SST_preds = as.matrix(scale(D_SST[,-1])) %*% D_SST_PCA$rotation[,1:pc.retain] %>% 
  data.table() %>%
  dplyr::mutate(year = years) %>%
  data.frame()

D_SST_preds_long = melt(D_SST_preds, id.vars = "year")

D_SST_PCA_plot_ts =
  ggplot(data = D_SST_preds_long, aes(year, value)) +
  geom_line() +
  geom_point(data = D_SST_preds_long %>% dplyr::filter(year %in% c(1982,1997,2002,2015)), aes(year, value)) +
  theme_bw() +
  ggtitle(expression("SST PCs")) +
  facet_wrap(~variable,
             ncol = pc.retain,
             scales = "free_y") +
  scale_x_continuous(breaks = seq(1950,2020, by = 10))

pdf(file = "Final figures/Figure_S6.pdf", width = 10, height = 3)
grid.arrange(D_SST_PCA_plot,
             D_SST_PCA_plot_ts,
             nrow = 2)
dev.off()


D_SST_preds = D_SST_preds %>% setNames(c("SST_PC1", "SST_PC2", "SST_PC3", "SST_PC4","year")) %>%   
                              dplyr::mutate(year = year + 1)

save(D_SST_preds, file = "data/Processed data/D_SST_preds.RData")


# check the cors between SST PCs and NINO indices
load(file = "data/Processed data/climate_ind.Rdata")

climate_ind_D = climate_ind %>% dplyr::filter(month %in% c(12)) %>%
  dplyr::mutate(year = year + 1) %>%
  dplyr::group_by(year) %>%
  dplyr::summarise(NINO3.4 = mean(NINO3.4)) %>%
  dplyr::mutate(NINO3.4_ld = linear.detrend(y = NINO3.4, year = year)) 


D_SST_NINO = merge(D_SST_preds, climate_ind_D, by = "year")

M = cor(D_SST_NINO)
M_rank = cor(D_SST_NINO, method = "pearson",use = "complete.obs")

pval <- psych::corr.test(D_SST_NINO, adjust="none", method="spearman")$p

pdf("Ref figures/D_SST_PCs_NINO_cor.pdf", width = 12, height = 5)
corrplot(M_rank, p.mat=pval, sig.level = 0.05)
dev.off()

rm(list = ls())


