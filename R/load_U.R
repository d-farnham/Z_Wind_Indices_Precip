# now load and save U_500
U0 = nc_open("data/u500.nc")

U = ncvar_get(U0, varid = "uwnd")
lon = ncvar_get(U0, varid = "lon")
lat = ncvar_get(U0, varid = "lat")
time = ncvar_get(U0, varid = "time")

U_long = U %>% melt() 
colnames(U_long) = c("lon","lat","time","u_500")

U_long$longitude = lon[U_long$lon]
U_long$latitude = lat[U_long$lat]
U_long$date = as.Date(time[U_long$time]/(24), origin = "1800-01-01")

U_long = U_long %>% dplyr::select(c(date,longitude,latitude,u_500))

U_long = U_long %>% dplyr::mutate(month = month(date))

U_long = U_long %>% dplyr::group_by(latitude,longitude, month) %>%
	dplyr::mutate(u_500_clim = mean(u_500),
								u_500_anom = u_500 - u_500_clim) %>%
  dplyr::filter(longitude > 180 &
                lubridate::year(date) > 1949 &
                lubridate::year(date) < 2017)

# just include 1950 to 2016 data

save(U_long, file = "data/Processed data/U_long.RData")

rm(list = ls())