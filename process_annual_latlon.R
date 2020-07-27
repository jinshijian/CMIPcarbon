## process_annual_latlon.R
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
output_dir <- file.path(base_dir, 'pic_data'); dir.create(output_dir, showWarnings = FALSE)
inter_dir  <- file.path(output_dir,'yr_5degrees'); dir.create(inter_dir, showWarnings = FALSE)

# The variables to process and the final otuput files. 
vars_to_process      <- c('rh', 'gpp', 'npp')
exp_to_process       <- c('historical')
yr_5degress_fldmeanCSV <- file.path(output_dir, 'yr_latlon_fldmean.csv')


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
cmip6_archive %>% 
  dplyr::filter(variable %in% vars_to_process & experiment %in% exp_to_process) -> 
  carbon_files

carbon_files <- dplyr::inner_join(carbon_files, meta_data_files, by = c("model", "experiment", "ensemble", "grid")) 

cat(nrow(carbon_files), ' to process over ', nrow(latlon_df), ' regions\n')


latlon_df <- latlon_df[c(1:3, 19:18), ]
carbon_files <- carbon_files[1:3, ]
files <- foreach(index = 1:nrow(carbon_files)) %do% {
  
  # Extract and format the information
  dt        <- carbon_files[index, ]
  cmip_info <- dt[ , names(dt) %in% c(cmip6_info, 'variable', 'time')]
  basename  <- paste(cmip_info[1,], collapse = '_')  
  out_file  <- file.path(inter_dir, paste0('mon_5degress_fldmean', basename, '.rds'))
  
  if(!file.exists(out_file)){
    
      # Calculate the land area and then calcualte the weighted area mean for the defined regions.  
      area   <- cdoR::cdo_land_area(dt = dt, intermed_dir = inter_dir)
      annual <- cdo_yearmonmean(name = basename, dt$file, cdo_exe, intermed_dir = inter_dir)
      out <- cdo_sellonlat_fldmean(basename = basename,  info = cmip_info, in_nc = annual, area_nc = area, latlon_df = latlon_df, intermed_dir = inter_dir)
      file.remove(c(area, annual))
      
      saveRDS(out, file = out_file)

  } else {
    message(out_file, ' already exists...')
  }
  
  out_file
}
print('done processing')
# 2. Save Results ------------------------------------------------------------------
files <- list.files(inter_dir, '.rds', full.names = TRUE)
data <- bind_rows(lapply(files,  readRDS))
write.csv(data, file = yr_5degress_fldmeanCSV, row.names = FALSE)




