# Custom functions are an important part of a drake workflow.
# This is where you write them.
# Details: https://books.ropensci.org/drake/plans.html#functions


# Subset the archive data so that it include the meta data files required to 
# calculate the land area weights. 
# Arguments 
#  df: A dataframe containing all of the CMIP arhcive index  
# Returns: A dataframe of the cell area and land fraction files that can be used to calculate the land area weights. 
find_land_meta_files <- function(df){
  
  vars <- c('sftlf', 'areacella')
  assertthat::assert_that(all(vars %in% df$variable))
  assertthat::assert_that(all(c('file', 'type', 'domain', 'variable', 'model', 'experiment', 'ensemble', 'grid', 'time') %in% names(df)))
  
  df %>% 
    dplyr::filter(variable %in% vars) %>% 
    dplyr::select(-type, -domain, -time) %>%  
    tidyr::spread(variable, file) %>% 
    na.omit 
  
}

# Find the carbon variables to process and make sure the we have sufficent meta data to calculate the land area weights. 
# Arguments 
#  df: A dataframe containing all of the CMIP arhcive index  
#  meta: A dataframe containing the required meta data, these files will be used to calculate the area weights. 
#  vars: A vector of CMIP output variable names these should not be meta data variable names. 
# Returns: A dataframe of the carbon data files to be processed along with the appropirate meta data files to be used as area weights. 
find_carbon_data_files <- function(df, meta, vars){
  
  assertthat::assert_that(all(vars %in% df$variable))
  assertthat::assert_that(all(c('file', 'type', 'domain', 'variable', 'model', 'experiment', 'ensemble', 'grid', 'time') %in% names(df)))
  
  df %>% 
    dplyr::filter(variable %in% vars & experiment == 'historical') %>%
    dplyr::inner_join(meta, by = c("model", "experiment", "ensemble", "grid")) 

}

# Calculate the land area to be used in as the weighted average. 
# Arguments 
#   df: A dataframe containing the CMIP data process and the land fraction and cell area netcdfs. 
#   basename: The base name to use when naming the netcdf files, this should refelct the model / variable / exerpiment / ensemble realization. 
#   intermed_dir: The path to the directory as to where to store the intermediate netcdf files. 
#   cdo_exe: The path to the CDO EXE 
# Returns: The path to the netcdf that contains the area weights. 
generate_land_area <- function(df, basename, intermed_dir, cdo_exe){
  
  # Make sure that there is only meta data information that can be used to claculate the land area wights for a single land area data file.
  sftlf     <- unique(df[['sftlf']])
  areacella <- unique(df[['areacella']])
  assertthat::assert_that(length(sftlf) == 1)
  assertthat::assert_that(length(areacella) == 1)
  
  # Define the paths to intermediate and file output files. 
  PercentLand_nc <- file.path(intermed_dir, paste0(basename, '_PercentLand.nc'))
  LandArea_nc    <- file.path(intermed_dir, paste0(basename, '_LandArea.nc'))

  
  system2(cdo_exe, args = c("-divc,100", sftlf, PercentLand_nc), stdout = TRUE, stderr = TRUE)
  system2(cdo_exe, args = c("-mul", areacella, PercentLand_nc, LandArea_nc), stdout = TRUE, stderr = TRUE)

  assertthat::assert_that(file.exists(LandArea_nc))
  LandArea_nc
  
}


# Calculate the weighted global mean for the land variables 
# Arguments 
#   dataframe: A dataframe containing the CMIP data process and the land fraction and cell area netcdfs (because of the way that this function is set up 
#              it will only calculate the weighted mean for over the land areas, will need to modify to allow for fexlibility to process area over oceans)
#   intermed_dir: The path to the directory as to where to store the intermediate netcdf files. 
#   cdo_exe: The path to the CDO EXE 
#   cleanup: A TRUE/FASLE argument that controls if the intermeidate netcdfs are cleaned  up or not, the default is set to leave intermediates in place.
# Returns: A data frame of the area weighted global average.  
weighted_global_mean <- function(dataframe, intermed_dir, cdo_exe, cleanup = FALSE){
  
  # Check the inputs. 
  assertthat::assert_that(dir.exists(intermed_dir)) 
  assertthat::assert_that(file.exists(cdo_exe))
  assertthat::assert_that(all(c('file', 'type', 'domain', 'variable', 'model', 'experiment', 'ensemble', 'grid', 'time', 'areacella', 'sftlf') %in% names(dataframe)))
  
  split(dataframe, interaction(dataframe$model, dataframe$experiment, dataframe$ensemble, dataframe$variable, drop = TRUE)) %>% 
    lapply(X=., function(input = X){
      
      
      # Define the base name to use for the intermediate files that are saved during this process. 
      info <- distinct(input[, which(names(input) %in% c("variable", "domain", "model", "experiment", "ensemble", "grid"))])
      assertthat::assert_that(nrow(info) == 1)
      
      basename     <- paste(info, collapse = '_')

      area_weights <- generate_land_area(input, basename, intermed_dir, cdo_exe)
      
      DataGridArea_nc <-  file.path(intermed_dir, paste0(basename, '_DataGridArea.nc'))
      WeightedMean_nc <-  file.path(intermed_dir, paste0(basename, '_WeightedMean.nc'))
      Final_nc        <-  file.path(intermed_dir, paste0(basename, '_Final.nc'))
      
      apply(input, 1, function(df){
        
        system2(cdo_exe, args = c(paste0("setgridarea,", area_weights), df[['file']], DataGridArea_nc), stdout = TRUE, stderr = TRUE)
        system2(cdo_exe, args = c('fldmean', DataGridArea_nc, WeightedMean_nc), stdout = TRUE, stderr = TRUE )
        system2(cdo_exe, args = c('-a', '-copy', WeightedMean_nc, Final_nc), stdout = TRUE, stderr = TRUE )
        
        # Extract data and format output.
        assertthat::assert_that(file.exists(Final_nc))
        nc <- nc_open(Final_nc)
        data.frame(value = ncvar_get(nc, df[['variable']]), 
                   units = ncatt_get(nc, df[['variable']])$units, 
                   time = ncvar_get(nc, 'time')) %>% 
          mutate( variable = df[["variable"]], 
                  domain = df[["domain"]], 
                  model = df[["model"]], 
                  experiment = df[["experiment"]],
                  ensemble = df[["ensemble"]], 
                  grid = df[["grid"]]) %>% 
          mutate(year = substr(time, 1, 4), 
                 month = substr(time, 5, 6), 
                 day = substr(time, 7, 8))
        
      }) %>% 
        bind_rows() 
      

    }) %>%  
    bind_rows() -> 
    output 
  
  
  if(cleanup){
    file.remove(list.files(intermed_dir, '.nc', full.names = TRUE)) 
  }

  output
  
} 

# Calculate the land ratio GPP to Soil Resipriation ratio per grid cell 
# Arguments 
#   dataframe: A dataframe containing the CMIP data process and the land fraction and cell area netcdfs (because of the way that this function is set up 
#              it will only calculate the weighted mean for over the land areas, will need to modify to allow for fexlibility to process area over oceans)
#   intermed_dir: The path to the directory as to where to store the intermediate netcdf files. 
#   cdo_exe: The path to the CDO EXE 
# Returns: A data frame of the area weighted GPP to Rs ratio calculated by grid cell. 
calculate_GPPtoRS_gridcell <- function(dataframe, intermed_dir, cdo_exe){
  
  vars <- c('gpp', 'raRoot', 'rhSoil')
  assertthat::assert_that(all(vars %in% dataframe$variable), msg = 'Missing variable, cannot calcualte the ratio.')
  
  dataframe %>% 
    filter(variable %in% vars) %>% 
    select(file, variable, model, experiment, ensemble, grid, time, areacella, sftlf) %>% 
    tidyr::spread(variable, file) %>% 
    na.omit -> 
    to_process
  
  apply(to_process, 1, function(input){
    
    # Define the base name to use for the intermediate files that are saved during this process. 
    base_name <- paste(input[["model"]], input[["experiment"]], input[["ensemble"]], input[["grid"]],input[["time"]], sep  = '_')
    
    # Messages
    print('------------------------------------------------') 
    print(base_name)  
    

    time_nc         <- file.path(intermed_dir, paste0(base_name, '_time.nc'))
    system2(cdo_exe, args = c('-a', '-copy', input[['gpp']], time_nc), stdout = TRUE, stderr = TRUE )
    nc <- nc_open(time_nc)
    file.remove(time_nc)
    
    # Import all of the data from the netcdfs. 
    input[['raRoot']] %>% 
      nc_open %>% 
      ncvar_get('raRoot') -> 
      raRoot 
    
    input[['rhSoil']] %>% 
      nc_open %>% 
      ncvar_get('rhSoil') -> 
      rhSoil
    
    input[['gpp']] %>% 
      nc_open %>% 
      ncvar_get('gpp') -> 
      gpp
    
    input[['sftlf']] %>%  
      nc_open %>% 
      ncvar_get('sftlf') -> 
      sftlf
    
    input[['areacella']] %>% 
      nc_open %>% 
      ncvar_get('areacella') -> 
      areacella
    
    # Define what we expect 0 to be. 
    practically_zero <- 3e-11
    
    # Identify all of the 0s in the netcdfs, these 
    # index values will be used to replace value from the 
    # ratio with 0s.
    raRoot_zero <- which(abs(raRoot) <= practically_zero)
    rhSoil_zero <- which(abs(rhSoil) <= practically_zero)
    gpp_zero    <- which(abs(gpp) <= practically_zero)
    zeros       <- c(raRoot_zero, rhSoil_zero, gpp_zero)
    
    rs_total     <- raRoot + rhSoil
    ratio        <- rs_total / gpp
    ratio[zeros] <- 0 
    
    
    # Calculate the land area with in each grid cell, this will 
    # be used as weights for the global weighted mean. 
    land_area <- areacella * (sftlf / 100)
    
    # Calculate the weighted global average. 
    value <- apply(ratio, MARGIN = 3, weighted.mean, w = land_area) 
    
    # Format the output. 
    data.frame(value = value, 
               units = NA, 
               time = ncvar_get(nc, 'time')) %>%
      mutate(era = 'cmip6', 
             variable = 'ratio', 
             model = input[["model"]], 
             experiment = input[["experiment"]],
             ensemble = input[["ensemble"]], 
             grid = input[["grid"]]) %>%  
      mutate(year = substr(time, 1, 4)) %>% 
      select(-time) 
    
  }) 
  
}

  




