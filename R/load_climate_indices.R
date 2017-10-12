# NINO3.4 -- http://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino34.long.anom.data
nino0 = read.table('http://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino34.long.anom.data', 
										 header=FALSE, skip = 1, fill=TRUE) %>% setNames(c("year", 1:12))

# get rid of lines below the year 2017 -- these lines are just informational and are not data
nino = melt(nino0[1:which(nino0$year == 2017),], id.vars = "year") %>% 
						setNames(c("year","month","NINO3.4")) %>%
						dplyr::arrange(year,month) %>%
						dplyr::mutate(NINO3.4 = ifelse(NINO3.4 != "-99.99", as.numeric(NINO3.4), NA))

# NINO1+2 -- https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino12.long.anom.data
nino120 = read.table('https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino12.long.anom.data', 
                   header=FALSE, skip = 1, fill=TRUE) %>% setNames(c("year", 1:12))

# get rid of lines below the year 2017 -- these lines are just informational and are not data
nino12 = melt(nino120[1:which(nino120$year == 2017),], id.vars = "year") %>% 
  setNames(c("year","month","NINO1_2")) %>%
  dplyr::arrange(year,month) %>%
  dplyr::mutate(NINO1_2 = ifelse(NINO1_2 != "-99.99", as.numeric(NINO1_2), NA))


# NINO3 -- http://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino3.long.anom.data
nino30 = read.table('http://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino3.long.anom.data', 
									 header=FALSE, skip = 1, fill=TRUE) %>% setNames(c("year", 1:12))

# get rid of lines below the year 2017 -- these lines are just informational and are not data
nino3 = melt(nino30[1:which(nino30$year == 2017),], id.vars = "year") %>% 
	setNames(c("year","month","NINO3")) %>%
	dplyr::arrange(year,month) %>%
	dplyr::mutate(NINO3 = ifelse(NINO3 != "-99.99", as.numeric(NINO3), NA))


# NINO4 -- https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino4.long.anom.data
nino40 = read.table('https://www.esrl.noaa.gov/psd/gcos_wgsp/Timeseries/Data/nino4.long.anom.data', 
                    header=FALSE, skip = 1, fill=TRUE) %>% setNames(c("year", 1:12))

# get rid of lines below the year 2017 -- these lines are just informational and are not data
nino4 = melt(nino40[1:which(nino40$year == 2017),], id.vars = "year") %>% 
  setNames(c("year","month","NINO4")) %>%
  dplyr::arrange(year,month) %>%
  dplyr::mutate(NINO4 = ifelse(NINO4 != "-99.99", as.numeric(NINO4), NA))


# PNA -- http://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.pna.monthly.b5001.current.ascii.table
pna0 = read.table('http://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.pna.monthly.b5001.current.ascii.table', 
                   header=FALSE, skip = 0, fill=TRUE) %>% setNames(c("year", 1:12))

pna = melt(pna0, id.vars = "year") %>% 
  setNames(c("year","month","PNA")) %>%
  dplyr::arrange(year,month)

# NAO -- http://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table
nao0 = read.table('http://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table', 
                  header=FALSE, skip = 0, fill=TRUE) %>% setNames(c("year", 1:12))

nao = melt(nao0, id.vars = "year") %>% 
  setNames(c("year","month","NAO")) %>%
  dplyr::arrange(year,month)


# PDO -- https://www.esrl.noaa.gov/psd/data/correlation/pdo.data
pdo0 = read.table('https://www.esrl.noaa.gov/psd/data/correlation/pdo.data', 
                   header=FALSE, skip = 1, fill=TRUE) %>% setNames(c("year", 1:12))

# get rid of lines below the year 2017 -- these lines are just informational and are not data
pdo = melt(pdo0[1:which(pdo0$year == 2017),], id.vars = "year") %>% 
  setNames(c("year","month","PDO")) %>%
  dplyr::arrange(year,month) %>%
  dplyr::mutate(PDO = ifelse(PDO != "-9.9", as.numeric(PDO), NA))

# put all of the climate indices together
climate_ind = merge(merge(merge(merge(merge(merge(nino,nino3, by = c("year","month"),all=TRUE),
                                      nino4, by = c("year","month"),all=TRUE), 
																      pna, by = c("year","month"),all=TRUE),
																		  nao, by = c("year","month"),all=TRUE),
                                      pdo, by = c("year","month"),all=TRUE),
                                      nino12, by = c("year","month"),all=TRUE) %>%
              dplyr::mutate(year = as.numeric(as.character(year)),
                            month = as.numeric(as.character(month))) %>%
              dplyr::arrange(year,month) %>%
              dplyr::filter(year > 1948 &
                            year < 2017)

save(climate_ind, file = "data/Processed data/climate_ind.Rdata")

rm(list = ls())
