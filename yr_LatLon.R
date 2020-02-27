## yr_LatLon.R
## Extract carbon data at specific Lat and Lon coordinates, these coordinates are 
## going to refer to observations from the various soil databases. 


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
carbon_vars <- c('gpp', 'raRoot', 'rhSoil')
values_LatLonCSV  <- file.path(output_dir, 'annual_values_LatLon.csv')


# 1. Extract the Lat & Lon Coords --------------------------------------------------------------------------
cmip6_archive   <- readr::read_csv(url("https://raw.githubusercontent.com/JGCRI/CMIP6/master/cmip6_archive_index.csv"))
meta_data_files <- find_land_meta_files(cmip6_archive)
carbon_files    <- find_carbon_data_files(df = cmip6_archive, meta = meta_data_files, vars = carbon_vars)

# Start by determiningit ag which files should be processed (this makes is easier to
# test the RStoGPP functions by limiting the number of rows in to_process_ratios).
to_process_ratios = prep_RStoGPP_data(carbon_files) %>% na.omit 
cat(nrow(to_process_ratios), ' netcdfs to be processed.')
        
# Extract the values at the coords corresponding to the database observations. 
coords        <- format_LatLon(input_dir)
values_LatLon <- extract_LatLon(dataframe = to_process_ratios, coord = coords, intermed_dir = inter_dir, cdo_exe = CDO)
write.csv(values_LatLon, file = values_LatLonCSV)
values_LatLon_out <- saveRDS(values_LatLon, file = file_out(values_LatLonRDS))
mesage('done!')