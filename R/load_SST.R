# first load and save Surface Temp
SST0 = nc_open("data/ersstv5.nc")


lon = ncvar_get(SST0, varid = "lon")
lat = ncvar_get(SST0, varid = "lat")
time = ncvar_get(SST0, varid = "time")

# need to manually define the date that runs from jan 1854 to sep 2017
dates = c(as.Date(paste0(rep(1854:2016, each = 12),"-",rep(1:12,(2016-1854)),"-01")),
          as.Date(paste0(rep(2017, each = 9),"-",rep(1:9,1),"-01")))
# only include months after Nov 1949 and before april 2016
dates_keep = dates > "1949-11-01" & dates < "2016-04-01"

# format: sst[lon,lat,lev,time]

SST = ncvar_get(SST0, varid = "sst", start = c(1,1,1,min(which(dates_keep))), 
                                     count = c(-1,-1,1,length(which(dates_keep))))

SST_long = SST %>% melt() 
colnames(SST_long) = c("lon","lat","time","SST")

SST_long$longitude = lon[SST_long$lon]
SST_long$latitude = lat[SST_long$lat]


SST_long$date = dates[dates_keep][SST_long$time]

SST_long = SST_long %>% dplyr::select(c(date,longitude,latitude,SST))

SST_long = SST_long %>% dplyr::mutate(month = month(date))

save(SST_long, file = "data/Processed data/SST_long.RData")

