## mon_global_fldmean.R
## Calculate the monthly area weighted global field mean for some carbon variables. 
## How long this takes to run depends on the number of files that will be processed. 


# 0. Set Up --------------------------------------------------------------------------
# Load the required pacakges 
source('./R/packages.R')
source('./R/functions.R')

# Define the path to the CDO 
CDO        <- "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo" 

# Define the directories
inter_dir  <- '/pic/scratch/dorh012'
base_dir   <- here::here()
input_dir  <- file.path(base_dir, 'input')
output_dir <- file.path(base_dir, 'pic_data'); dir.create(output_dir, showWarnings = FALSE)

# A vector of the carbon files to process and location of where the final results 
# should be saved. 
carbon_vars <- c('gpp', 'raRoot', 'rh', 'rhSoil')
weighted_global_meanRDS <- file.path(output_dir, 'mon_global_fldmean.rds')

# A true false indicator to determine if the intermediate files should be removed or not. 
CleanUP <- FALSE

# 1. Process the Global Weighted Mean --------------------------------------------------------------------------
cmip6_archive   <- readr::read_csv(url("https://raw.githubusercontent.com/JGCRI/CMIP6/master/cmip6_archive_index.csv"))
meta_data_files <- find_land_meta_files(cmip6_archive)
carbon_files    <- find_carbon_data_files(df = cmip6_archive, meta = meta_data_files, vars = carbon_vars)
message(cat(nrow(carbon_files), ' netcdfs to be processed.'))


global_means     <- weighted_land_mean(dataframe = carbon_files, intermed_dir = inter_dir, cdo_exe = CDO, cleanup = CleanUP)
global_means_out <- saveRDS(global_means, file = weighted_global_meanRDS)
message('done!')

