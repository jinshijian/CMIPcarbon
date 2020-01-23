# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html

# This first drake plan must be run on constance (pic) where it can acess the cmip netcdfs and 
# cdo, the second drake plan can be run a local machine or on pic, so long as the intermeidate 
# data is avaiable. 
plan <- drake_plan(

  # Determine which CMIP files need to be processed, parse out the meta data files that will be
  # used to calculate the land area weights from the CMIP data files that we want to process. 
  cmip6_archive = target(
    readr::read_csv(url("https://raw.githubusercontent.com/JGCRI/CMIP6/master/cmip6_archive_index.csv")),
    # This makes it run every time
    trigger = trigger(condition = TRUE)), 
  meta_data_files = find_land_meta_files(cmip6_archive), 
  carbon_files = find_carbon_data_files(df = cmip6_archive, meta = meta_data_files, vars = c('gpp', 'raRoot', 'rh', 'rhSoil')), 

  # Process the CMIP data files. 
  INTERMED = '/pic/scratch/dorh012', 
  CDO = "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo", 
  # Land area weighted global mean 
    dataframe = carbon_files,
    global_means = weighted_global_mean(dataframe, intermed_dir = INTERMED, cdo_exe = CDO, cleanup = FALSE),
  # The grid cell ratio 
    gridcell_ratio = calculate_GPPtoRS_gridcell(dataframe, intermed_dir = INTERMED, cdo_exe = CDO),
  
  # Save pic output. 
  pic_out = { 
    dir.create(file_out("pic_data"))
    save(global_means, file = './pic_data/weighted_global_mean.rda')
    save(gridcell_ratio, file = './pic_data/gridcell_ratio.rda')
  }
)

