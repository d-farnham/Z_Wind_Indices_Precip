# load the U_500 and compute the PCA
load("data/Processed data/U_long.RData")

# run PCA on U data
U_long_JFM = U_long %>% data.table() %>%
	mutate(year = year(date)) %>%
	dplyr::filter(year > 1949) %>%
	dplyr::filter(latitude > 20) %>%
	dplyr::filter(latitude < 55) %>%
	dplyr::filter(longitude > 200) %>%
	dplyr::filter(longitude < 320) %>%
	dplyr::filter(month %in% c(1,2,3)) %>%
	dplyr::group_by(latitude, longitude, year) %>%
	dplyr::summarise(u_500 = mean(u_500)) %>%
	data.frame()

JFM_U = dcast(data = U_long_JFM, year ~ longitude + latitude, value.var = "u_500")

JFM_U_PCA = prcomp(JFM_U[,-1], scale. = TRUE)

round((JFM_U_PCA$sdev/sum(JFM_U_PCA$sdev))[1:10], digits = 3) * 100
# [1] 16.2 10.5  8.6  6.9  6.4  5.6  4.1  3.9  2.5  2.4

lat_lon = data.frame(matrix(unlist(strsplit(names(JFM_U)[-1],"_")), ncol = 2, byrow=TRUE))
colnames(lat_lon) = c("longitude","latitude")
lat_lon$longitude = as.numeric(as.character(lat_lon$longitude))
lat_lon$latitude = as.numeric(as.character(lat_lon$latitude))


JFM_loadings = cbind(lat_lon,JFM_U_PCA$rotation[,1:6])
JFM_loadings_long = melt(JFM_loadings, id.vars = c("longitude","latitude"))

world <- data.frame(map("world", plot=FALSE)[c("x","y")])
world[world$x < 0 & !is.na(world$x),]$x = world[world$x < 0 & !is.na(world$x),]$x + 360

state <- data.frame(map("state", plot=FALSE)[c("x","y")])
state[state$x < 0 & !is.na(state$x),]$x = state[state$x < 0 & !is.na(state$x),]$x + 360

# shade in the 'western' states as we have defined them
load(file = "data/Processed data/western_states.RData")
west[west$x < 0 & !is.na(west$x)]$x = west[west$x < 0 & !is.na(west$x)]$x + 360

west_df = data.frame(x = west$x, 
                     y = west$y)

JFM_U_PCA_plot = ggplot() +
  geom_tile(data = JFM_loadings_long, aes(x = (longitude),y = latitude,fill=(value))) +
	geom_path(data=world, aes(x,y)) +  
  geom_polygon(data=west_df, aes(x,y), alpha = 0.25) +
  geom_path(data=state, aes(x,y), size = 0.25, alpha = 0.25) +
	scale_y_continuous(limits = c(15,60)) + 
	scale_x_continuous(limits = c(180,330)) + 
	xlab("lon") + 
	ylab("lat") +
	coord_map("ortho", orientation=c(40, 260, 0)) +
	scale_fill_gradient2(name="loading", limits = c(min(JFM_loadings_long$value),max(JFM_loadings_long$value)), 
											 midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "grey") +
	ggtitle(expression("JFM U"[500] ~"EOFs")) +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~variable, ncol = 3)


# make the U predictors #
years = JFM_U$year
JFM_U_preds = as.matrix(scale(JFM_U[,-1])) %*% JFM_U_PCA$rotation[,1:6] %>% 
	data.table() %>%
	dplyr::mutate(year = years) %>%
	data.frame()

JFM_U_preds = JFM_U_preds %>% dplyr::select(year, PC1, PC2, PC3, PC4, PC5, PC6) 
save(JFM_U_preds, file = "data/Processed data/JFM_U_preds.RData")


JFM_U_preds_long = melt(JFM_U_preds, id.vars = "year")

JFM_U_PCA_plot_ts =
ggplot(data = JFM_U_preds_long, aes(year, value)) +
  geom_line() +
  theme_bw() +
  ggtitle(expression("JFM U"[500] ~"PCs")) +
  facet_wrap(~variable, ncol = 3)


pdf("Final figures/Figure_1.pdf", width = 8, height = 8)
grid.arrange(JFM_U_PCA_plot,
             JFM_U_PCA_plot_ts,
             layout_matrix = rbind(1,1,1,2,2))
dev.off()


rm(list = ls())

