# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html

plan <- drake_plan(
  
  # Determine which CMIP files need to be processed. 
  cmip6_archive = readr::read_csv(url("https://raw.githubusercontent.com/JGCRI/CMIP6/master/cmip6_archive_index.csv")), 
  meta_data_files = find_land_meta_files(cmip6_archive), 
  carbon_files = find_carbon_data_files(df = cmip6_archive, meta = meta_data_files, vars = c('gpp', 'raRoot', 'rh', 'rhSoil')), 
  
  
  # 
  INTERMED = '/pic/scratch/dorh012', 
  CDO = "/share/apps/netcdf/4.3.2/gcc/4.4.7/bin/cdo", 
  dataframe = carbon_files[1:5, ],
  global_means = weighted_global_mean(dataframe, intermed_dir = INTERMED, cdo_exe = CDO, cleanup = FALSE)
  
  
)
