## mon_regional_fldmean.R
## Calculate the weighted average for speficied regions of the globe. 

# 0. Set Up --------------------------------------------------------------------------------------
# Load required libs 
library(tibble)
library(readr)
library(data.table)
library(ncdf4)
library(dplyr)
library(lubridate)
library(foreach)
source('/pic/projects/GCAM/Dorheim/CMIPcarbon/R/functions.R') # The carbon project specific functions 
devtools::load_all('/pic/projects/GCAM/Dorheim/cdoR')         # Other cdo functions 

# Define the path to the CDO on PIC. 
CDO        <- "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo" 

# Define the dirs. 
base_dir   <- here::here()
input_dir  <- file.path(base_dir, 'input')
output_dir <- file.path(base_dir, 'pic_data', 'mon_regional'); dir.create(output_dir, showWarnings = FALSE)
inter_dir  <- file.path(output_dir,'scratch'); dir.create(inter_dir, showWarnings = FALSE)

# The variables to process and the final otuput files. 
vars_to_process      <- c('rh')
mon_regional_fldmean <- file.path(output_dir, 'mon_regional_fldmean.rds')

# Define the regions to process 
# Define the regions to process. 
latlon_df <- tibble::tibble(name = c('NH', 'SH'),
                            lon1 = c( 0,    0),
                            lon2 = c(360, 360),
                            lat1 = c(0,   -90),
                            lat2 = c(90,   0))


# 1. Select and Process Files -------------------------------------------------------------------- 
data <- foreach(index = 1:nrow(carbon_files), .combine = 'rbind') %do% {

  # Extract and format the information
  dt        <- carbon_files[index, ]
  cmip_info <- dt[ , names(dt) %in% cmip6_info]
  basename  <- paste(cmip_info[1,], collapse = '_')
  
  # Calculate the land area and then calcualte the weighted area mean for the defined regions.  
  area <- cdoR::cdo_land_area(dt = dt, intermed_dir = inter_dir)
  data <- cdo_sellonlat_fldmean(basename = basename,  info = cmip_info, in_nc = dt$file, area_nc = area, 
                                latlon_df = latlon_df, intermed_dir = inter_dir, cleanUP = FALSE)
  
  out_file <- paste0(values_regionalRh, basename, '.rds')
  saveRDS(data, file = out_file)
  out_file
}

data <- bind_rows(lapply(data, readRDS)) 
saveRDS(data, file = mon_regional_fldmean)
message('done!')

# TODO add code to clean up the intermediate dir

