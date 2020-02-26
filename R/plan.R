
# Okay looking over this I am not that happy with how things are turnning out... I think that his code is really 
# messy and I have found lots of errors. I think that moving forward I reall need to plan naming conventions better 
# and I am annoyed becasue I can't tell if drake is going to be a good thing for me to be useing or not. 

##########################################################################################
## CDO PROCESSING SET UP 
##########################################################################################
INTERMED   <- '/pic/scratch/dorh012'
CDO        <- "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo" 
base_dir   <- here::here()
input_dir  <- file.path(base_dir, 'input')
output_dir <- file.path(base_dir, 'pic_data'); dir.create(output_dir, showWarnings = FALSE)
inter_dir  <- file.path(output_dir,'scratch'); dir.create(inter_dir, showWarnings = FALSE)

weighted_global_meanRDS <- file.path(output_dir, 'weighted_global_mean.rds')
gridcell_ratioRDS       <- file.path(output_dir, 'gridcell_ratio.rds')
values_LatLonRDS        <- file.path(output_dir, 'values_LatLon.rds')

# This first drake plan must be run on constance (pic) where it can acess the cmip netcdfs and 
# cdo to process the netcdf information.
cdo_processing <- drake_plan(

  # Determine which CMIP files need to be processed, parse out the meta data files that will be
  # used to calculate the land area weights from the CMIP data files that we want to process. 
  cmip6_archive = target(
    readr::read_csv(url("https://raw.githubusercontent.com/JGCRI/CMIP6/master/cmip6_archive_index.csv")),
    # This makes it run every time
    trigger = trigger(condition = TRUE)), 
  meta_data_files = find_land_meta_files(cmip6_archive), 
  carbon_files = find_carbon_data_files(df = cmip6_archive, meta = meta_data_files, vars = c('gpp', 'raRoot', 'rh', 'rhSoil')), 
  
  # Weighted Averages ############################################################   
  global_means = weighted_land_mean(dataframe = carbon_files, intermed_dir = INTERMED, 
                                    cdo_exe = CDO, cleanup = FALSE),
  global_means_out = saveRDS(global_means, file = file_out(weighted_global_meanRDS)), 
  
  # RS to GPP Ratio ##############################################################
  # Start by determiningit ag which files should be processed (this makes is easier to
  # test the RStoGPP functions by limiting the number of rows in to_process_ratios).
  to_process_ratios = prep_RStoGPP_data(carbon_files),

  # # Calculate the ratio at each grid cell and calculate the global average. We are interested
  # # in asking how different are these restults from the ratio of the global mean, how senstive
  # # is the ratio to the percision of the units?
  # mean_gridcell_ratio = calculate_RStoGPP_gridcell(dataframe = to_process_ratios, intermed_dir = INTERMED, cdo_exe = CDO),
  # mean_gridcell_ratio_out = saveRDS(mean_gridcell_ratio, file = file_out(gridcell_ratioRDS)),

  coords = format_LatLon(input_dir),
  values_LatLon = extract_LatLon(dataframe = to_process_ratios, coord = coords, 
                                 intermed_dir = inter_dir, cdo_exe = CDO), 
  values_LatLon_out = saveRDS(values_LatLon, file = file_out(values_LatLonRDS))

) 

