load('data/Processed data/SST_long.RData')

SST_long = SST_long %>% data.table() %>%
  mutate(year = year(date)) %>%
  dplyr::filter(month %in% c(1,2,3)) %>%
  dplyr::group_by(latitude, longitude, year) %>%
  dplyr::summarise(SST = mean(SST)) %>%
  dplyr::group_by(latitude, longitude) %>%
  dplyr::mutate(SST_clim = mean(SST),
                SST_anom = SST - SST_clim) %>%
  data.frame()


# now let's detrend the SST
source("R/linear.detrend.R")

SST_long_detrend = SST_long %>% dplyr::filter(latitude > (-25) &
                                              latitude < 65) %>%
                                dplyr::group_by(latitude, longitude) %>%
                                dplyr::filter(!is.na(SST_anom)) %>%
                                dplyr::mutate(SST_anom_ld = linear.detrend(y = SST_anom, year = year))

SST_long_detrend_subset = SST_long_detrend %>% dplyr::filter(year %in% c(1983, 1998, 2003, 2016)) %>%
                                               dplyr::select(year,longitude, latitude, SST_anom_ld)

world <- data.frame(map("world", plot=FALSE)[c("x","y")])
world[world$x < 0 & !is.na(world$x),]$x = world[world$x < 0 & !is.na(world$x),]$x + 360


SST_anom_plot =
  ggplot(data = SST_long_detrend_subset, 
         aes(x = (longitude),y = latitude)) +
  geom_tile(aes(fill=(SST_anom_ld))) +
  geom_contour(aes(z = SST_anom_ld), breaks = c(1,2,3), col = "black", linetype = "solid") +
  geom_contour(aes(z = SST_anom_ld), breaks = c(-1,-2,-3), col = "black", linetype = "dashed") +
  geom_path(data=world, aes(x,y)) +
  scale_y_continuous(limits = c(-20,60)) +
  scale_x_continuous(limits = c(50,359.999)) +
  xlab("lon") + 
  ylab("lat") +
  theme_bw() +
  scale_fill_gradient2(name="SST anom (deg C)", limits = c(-4.25,4.25), 
                       midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "white") +
  facet_wrap(~year,
             ncol = 1) +
  ggtitle("Jan-Mar")


load('data/Processed data/SST_long.RData')

SST_long_d = SST_long %>% data.table() %>%
                          dplyr::mutate(year = year(date)) %>%
                          dplyr::filter(month %in% c(12)) %>%
                          dplyr::group_by(latitude, longitude, year) %>%
                          dplyr::summarise(SST = mean(SST)) %>%
                          dplyr::group_by(latitude, longitude) %>%
                          dplyr::mutate(SST_clim = mean(SST),
                                        SST_anom = SST - SST_clim) %>%
                          data.frame()

# now let's detrend the SST
source("R/linear.detrend.R")

SST_long_d_detrend = SST_long_d %>% dplyr::filter(latitude > (-25) &
                                                    latitude < 65) %>%
                                    dplyr::group_by(latitude, longitude) %>%
                                    dplyr::filter(!is.na(SST_anom)) %>%
                                    dplyr::mutate(SST_anom_ld = linear.detrend(y = SST_anom, year = year))


SST_long_d_detrend_subset = SST_long_d_detrend %>% dplyr::filter(year %in% c(1982, 1997, 2002, 2015)) %>%
                                                   dplyr::select(year,longitude, latitude, SST_anom_ld)

world <- data.frame(map("world", plot=FALSE)[c("x","y")])
world[world$x < 0 & !is.na(world$x),]$x = world[world$x < 0 & !is.na(world$x),]$x + 360


SST_d_anom_plot =
  ggplot(data = SST_long_d_detrend_subset, 
         aes(x = (longitude),y = latitude)) +
  geom_tile(aes(fill=(SST_anom_ld))) +
  geom_contour(aes(z = SST_anom_ld), breaks = c(1,2,3), col = "black", linetype = "solid") +
  geom_contour(aes(z = SST_anom_ld), breaks = c(-1,-2,-3), col = "black", linetype = "dashed") +
  geom_path(data=world, aes(x,y)) +
  scale_y_continuous(limits = c(-20,60)) +
  scale_x_continuous(limits = c(50,359.999)) +
  xlab("lon") + 
  ylab("lat") +
  theme_bw() +
  scale_fill_gradient2(name="SST anom (deg C)", limits = c(-5.25,5.25), 
                       midpoint = 0,low="blue", mid = "white",  high = "red",na.value = "white") +
  facet_wrap(~year,
             ncol = 1) +
  ggtitle("Dec")


pdf(file = 'Final figures/Figure_S5.pdf', width = 14, height = 8)
grid.arrange(SST_d_anom_plot,
             SST_anom_plot,
             ncol = 2)
dev.off()

rm(list = ls())
