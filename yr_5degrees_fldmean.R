## yr_5degrees_fldmean.R
## Calculate the weighted average for every 5 degrees of Lat/Lon. 
## This is for the Will C project. 

# 0. Set Up --------------------------------------------------------------------------------------
# Load required libs 
library(tibble)
library(dplyr)
library(foreach)
source('/pic/projects/GCAM/Dorheim/CMIPcarbon/R/functions.R') # The carbon project specific functions 
devtools::load_all('/pic/projects/GCAM/Dorheim/cdoR')         # Other cdo functions 

# Define the dirs. 
base_dir   <- here::here()
output_dir <- file.path(base_dir, 'pic_data', 'yr_5degrees'); dir.create(output_dir, showWarnings = FALSE)
inter_dir  <- file.path(output_dir,'scratch'); dir.create(inter_dir, showWarnings = FALSE)

# The variables to process and the final otuput files. 
vars_to_process      <- c('rh')
mon_regional_fldmeanCSV <- file.path(output_dir, 'yr_5degress_fldmean.csv')


# Define the regions to process, we want to process the annual average every 5 degrees. 
all <- seq(from = -90, to = 90, by = 5)
lat1 <- all[1:length(all)-1]
lat2 <- all[2:length(all)]

latlon_df <- tibble::tibble(lon1 = 0,
                            lon2 = 360,
                            lat1 = lat1,
                            lat2 = lat2, 
                            name = paste0('name_', 1:length(lat1)))

# 1. Select and Process Files ------------------------------------------------------------------
cmip6_archive   <- readr::read_csv(url("https://raw.githubusercontent.com/JGCRI/CMIP6/master/cmip6_archive_index.csv"))
meta_data_files <- find_land_meta_files(cmip6_archive)
carbon_files    <- find_carbon_data_files(df = cmip6_archive, meta = meta_data_files, vars = vars_to_process) 
cat(nrow(carbon_files), ' to process over ', nrow(latlon_df), ' regions\n')


files <- foreach(index = 1:nrow(carbon_files)) %do% {

  # Extract and format the information
  dt        <- carbon_files[index, ]
  cmip_info <- dt[ , names(dt) %in% c(cmip6_info, 'variable')]
  basename  <- paste(cmip_info[1,], collapse = '_')  
  out_file  <- file.path(inter_dir, paste0('mon_regional_fldmean', basename, '.rds'))
  
  if(!file.exists(out_file)){

    # Calculate the land area and then calcualte the weighted area mean for the defined regions.  
    area <- cdoR::cdo_land_area(dt = dt, intermed_dir = inter_dir)
    data <- cdo_sellonlat_fldmean(basename = basename,  info = cmip_info, in_nc = dt$file, area_nc = area, 
                                  latlon_df = latlon_df, intermed_dir = inter_dir)
    
    saveRDS(data, file = out_file)
  } 
  else {
    message(out_file, ' already exists...')
  }

  out_file
}



# 2. Save Results ------------------------------------------------------------------
data <- bind_rows(lapply(files,  readRDS))
write.csv(data, file = mon_regional_fldmeanCSV, row.names = FALSE)


