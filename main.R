rm(list = ls())
package.list <- list("dplyr", "ggplot2", "readr", "tidyr", "ggthemes", "readr", "ggmap", "lubridate",
                     "maps", "reshape2", "ncdf4", "gridExtra", "data.table", "magrittr", "maptools", "plyr")
source('R/load_packages.R') # clear workspace, clear console, load packages

# run the scripts to load and process the data
source('R/load_U.R') # load the U_500 field (should take about 30 sec)
source('R/load_climate_indices.R') # load the climate indices
source('R/load_conus_precip.R') # load the CONUS precip (should take about 80 sec)

# define the western states
west = map("state",regions= c("cali", "ariz", "new mex", "oregon", "wash", "wyoming", "montana", 
                              "north dakota", "south dakota", "nevada", "colorado","utah","texas",
                              "oklahoma","idaho", "nebraska","kansas"), plot = FALSE, fill = TRUE)

save(west, file = "data/Processed data/western_states.RData")

# run the script that runs PCA on the U field and saves the PCs
source('R/PCA_U_JFM.R') # Fig 1 (should take about 30 s)

# run a script that plots a composite for the past EN events, LN events, neutral events, and some select years
source('R/Make_Composite_Precip_Fig.R') # Fig S1

# plott he correaltions between the zonal wind PCs, NINO indices, NAO, PNA, and PDO with gridded precipitation
source('R/Get_Plot_pred_precip_Coefs.R') # Fig S2

# now let's fit 3 models and compute the skill score by lat/lon and save these maps
source("R/Get_gridded_skill.R")

# now fit the models and compute the skill scores by year left out
source('R/Fit_mods_all_Years_all_wind_PCs.R') # Figs 2, S3, S4 (can take > 10 min)

# now make Figure 3
source("R/Plot_NINO_PCs_time_series.R") # Fig 3

# add a script to load the monthly sea surface temps
# source('R/load_SST.R') # This cannot be run because the raw file is too large for Github 
# (the processed file is already present in the data/"Processed data directory)

# plot the SSTs from the 1983, 1998, 2003, and 2016 events
source("R/plot_SST_ex_years.R") # Fig S5

# compute PCA on dec SSTs and plot EOFs
source("R/PCA_SST_D.R") # Fig S6

# Fit the KNN models and make the figure summarizing the results
source("R/PC1_Pred_KNN_SST.R") # Fig S7

# make the Figure 4 and S8
source("R/SST_U_Precip_Cors.R") # Figs 4 and S8

# update the figure files in the manuscript folder (run the following line in terminal)
# cp -a /Users/davidfarnham/Google\ Drive/Zonal_Wind_Precip_Paper/Final\ figures/. /Users/davidfarnham/Google\ Drive/Manuscripts/JET_ENSO_PRECIP/figs/

