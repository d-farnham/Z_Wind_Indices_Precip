# load the monthly precipitation data
gpcc.precip0 = nc_open("data/gpcc_10_combined.nc")
gpcc.precip = ncvar_get(gpcc.precip0, varid = "prcp")

lon = ncvar_get(gpcc.precip0, varid = "lon") 
lat = ncvar_get(gpcc.precip0, varid = "lat")
time = ncvar_get(gpcc.precip0, varid = "time")

precip0 = gpcc.precip %>% melt() 
colnames(precip0) = c("lon","lat","date","precip")

precip = precip0 
precip$longitude = lon[precip0$lon]
precip$latitude = lat[precip0$lat]
precip$date = as.Date(time[precip0$date], origin = "1901-01-01")

precip = precip %>% dplyr::select(c(date,longitude,latitude,precip))

# just grab the US precip
us_precip = precip %>% data.table() %>%
	mutate(month = month(date)) %>%
	mutate(year = year(date)) %>%
	dplyr::filter(longitude > (-127.5)) %>%
	dplyr::filter(longitude < (-67.5)) %>%
	dplyr::filter(latitude > 25) %>%
	dplyr::filter(latitude < 52.5) %>%
	dplyr::filter(!is.na(precip) &
	              year > 1949 &
	              year < 2017) %>%
	data.frame()

save(us_precip, file = "data/Processed data/us_precip.Rdata")

rm(list = ls())