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
# Returns: A dataframe of the carbon data files to be processed along with the appropirate meta data files to be used as area ihts. 
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
  #system2(cdoR::cdo_exe, args = c("-addc,1","-mulc,-0.01", sftlf, PercentLand_nc), stdout = TRUE, stderr = TRUE)
  
  system2(cdo_exe, args = c("-mul", areacella, PercentLand_nc, LandArea_nc), stdout = TRUE, stderr = TRUE)
  
  assertthat::assert_that(file.exists(LandArea_nc))
  LandArea_nc
  
}

# Extract and format output time results. 
# Arguments 
#   nc: the nc data of the file with the time information to extract. 
# Returns: A data frame of the time formated as time date information. 
format_time <- function(nc){
  
  # Make sure that the object being read in is a netcdf file.
  assertthat::assert_that(class(nc) == "ncdf4")
  
  # Convert from relative time to absoulte time using lubridate.
  time_units <- ncdf4::ncatt_get(nc, 'time')$units
  time_units <- gsub(pattern = 'days since ', replacement = '', time_units)
  time <- lubridate::as_date(ncdf4::ncvar_get(nc, 'time'), origin = time_units)

  data.frame(datetime = time,
             year = lubridate::year(time),
             month = lubridate::month(time))
  
}

# Calculate the wighted field mean for a netcdf, without using cdo. 
# TODO see if we can resolve the issues with the cdo setarea grid command. 
# Arguments
#  dataframe: A dataframe containing the CMIP data process and the land fraction and cell area netcdfs (because of the way that this function is set up
#             it will only calculate the weighted mean for over the land areas, will need to modify to allow for fexlibility to process area over oceans)
#  intermed_dir: The path to the directory as to where to store the intermediate csv files. 
#             this helps saves the progress if for some reason the code terminates early.
#  cleanup: A TRUE/FASLE argument that controls if the intermeidate netcdfs are cleaned up or not, 
#           the default is set to leave intermediates in place.
# Returns: A data frame of the area weighted global average.
weighted_land_mean <- function(dataframe, intermed_dir,cdo_exe, cleanup = FALSE){

  # Check the inputs.
  assertthat::assert_that(dir.exists(intermed_dir))
  assertthat::assert_that(all(c('file', 'type', 'domain', 'variable', 'model', 'experiment', 'ensemble',
                                'grid', 'time', 'areacella', 'sftlf') %in% names(dataframe)))

  apply(dataframe, 1, function(x){
    
    # TODO this could be replaced with another function... I have to do it a lot when working 
    # with CMIP data in tibble form + applies 
    info <- tibble::tibble(model = x[['model']], 
                           variable = x[['variable']], 
                           domain = x[['domain']], 
                           experiment = x[['experiment']], 
                           ensemble = x[['ensemble']], 
                           grid = x[['grid']])
    
    basename <- paste(info, collapse = '_')
    print('--------')
    print(basename)
    
    # Calculate the area weights. 
    area_weights <- generate_land_area(x, basename, intermed_dir, cdo_exe)
    area <- ncdf4::ncvar_get(ncdf4::nc_open(area_weights), 'areacella')
    total_area <- sum(area)
    
    nc <- ncdf4::nc_open(x$file)
    time <- format_time(nc)
    data <- ncdf4::ncvar_get(nc, info$variable)
    
    # This assertthat may be useful when we start looking into the lat/lon 
    # patterns. 
    assertthat::assert_that(all(dim(data)[1:2] == dim(area)))
    
    mean <- apply(data, 3, weighted.mean, w = area)
    
   rslt <-  cbind(time,
          value = mean,
          units = ncdf4::ncatt_get(nc, info$variable)$unit,
          info)
   write.csv(rslt, file = file.path(intermed_dir, paste0(basename, x$time, 'Mean.csv')), row.names = FALSE)
   
   rslt
   
  }) %>% 
    dplyr::bind_rows(.) -> 
    out
  
  if(cleanup){
    file.remove(list.files(intermed_dir, '.csv', full.names = TRUE))
  }
  out
} 
  


# Prep the CMIP dataframe to calcualte the Rs to GPP ratio. This function is useful in that it 
# preps the RStoGPP ratio functions such that the input can easily be index to be tested. 
# 
# Arguments 
#   dataframe: A dataframe of cmip data to process before calcaulting the Rs to GPP ratio. 
prep_RStoGPP_data <- function(dataframe){
  
  vars <- c('gpp', 'raRoot', 'rhSoil')
  assertthat::assert_that(all(vars %in% dataframe$variable), msg = 'Missing variable, cannot calcualte the ratio.')
  
  dataframe %>% 
    dplyr::filter(variable %in% vars) %>% 
    dplyr::select(file, variable, model, experiment, ensemble, grid, time, areacella, sftlf) %>% 
    tidyr::spread(variable, file) 
  
}


# Calculate the land ratio GPP to Soil Resipriation ratio per grid cell 
# Arguments 
#   dataframe: A dataframe containing the CMIP data process and the land fraction and cell area netcdfs (because of the way that this function is set up 
#              it will only calculate the weighted mean for over the land areas, will need to modify to allow for fexlibility to process area over oceans)
#   intermed_dir: The path to the directory as to where to store the intermediate netcdf files. 
#   cdo_exe: The path to the CDO EXE 
# Returns: A data frame of the area weighted GPP to Rs ratio calculated by grid cell. 
calculate_RStoGPP_gridcell <- function(dataframe, intermed_dir, cdo_exe){
  
  # Define the base name to use for the intermediate files that are saved during this process.
  req_cols <- c("model", "experiment", "ensemble", "grid", "time", "areacella", "sftlf", "gpp", "raRoot", "rhSoil")
  assertthat::assert_that(all(req_cols %in% names(dataframe)))
  
  apply(dataframe, 1, function(input){
    
    info      <- data.frame(model = input[['model']], experiment = input[['experiment']], ensemble = input[['ensemble']], grid = input[['grid']])
    base_name <- paste(c(input[['model']], input[['experiment']], input[['ensemble']], input[['grid']]), collapse = '_')
    
    print('------------------------------------------------')
    print(base_name)
    
    tryCatch({
      # Extract the time
      nc   <- nc_open(input[['gpp']])
      time <- format_time(nc)
      nc_close(nc)
      
      
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
      
      data.frame(value = value, 
                 units = 'Rs:GPP')  %>% 
        cbind(time, info) 
      
    }, error = function(e){mutate(info, problem = TRUE)})
    
    })  %>% 
    bind_rows()
} 



# Create a data frame of the latitude and longitude coordinates from the observed data. 
# Arguments 
#   dir: the input direcotry that contains the rds files of the observations with the coordinates. 
# Returns: A dataframe of the lat and lon coordinates and observation source. 
format_LatLon <- function(dir){
  
  # Find the lat and lon coordinates. 
  files   <- list.files(path = dir, pattern = '_LatLon.rds', full.names = TRUE)
  sources <- gsub(x = basename(files), pattern = '_LatLon.rds|RsGPP_', replacement = '')
  
  # Import and format the lat and lon coordinates into a single data frame.
  mapply(FUN = function(file, source){
    
    df <- readRDS(file)
    
    assertthat::assert_that(all(c('Latitude', 'Longitude') %in% names(df)))
    
    df %>% 
      select(Latitude, 
             Longitude) %>%  
      mutate(source = source)
    
  }, file = files, source = sources, SIMPLIFY = FALSE) %>% 
    dplyr::bind_rows() %>%  
    na.omit 
  
}


# Extract the data for specific lat and lon coordiantes from MIP data that correspond to observed data. 
# Arguemnts
#   dataframe: A dataframe containing the CMIP data process and the land fraction and cell area netcdfs (because of the way that this function is set up 
#              it will only calculate the weighted mean for over the land areas, will need to modify to allow for fexlibility to process area over oceans)
#   coord: A dataframe of the lat and lon coordinates to extract from the MIP data, this data frame should be mae by the process_LatLon function 
#   intermed_dir: The path to the directory as to where to store the intermediate netcdf files. 
#   cdo_exe: The path to the CDO EXE 
# Returns: A data frame of gpp, raRoot, rhSoil, and land area values at specfic coordinates.  
extract_LatLon <- function(dataframe, coord, intermed_dir, cdo_exe){
  
  req_columns <- c('gpp', 'raRoot', 'rhSoil', 'areacella', 'sftlf')
  assertthat::assert_that(all(req_columns %in% names(dataframe)), msg = 'Missing required columns.')
  assertthat::assert_that(file.exists(cdo_exe), msg = 'Path to CDO exe does not exist.')
  assertthat::assert_that(dir.exists(intermed_dir), msg = 'intermed_dir does not exist.')
  
  
  bind_rows(apply(dataframe, 1, function(input){
    
    tryCatch({
      
      # Define the base name to use for the intermediate files that are saved during this process. 
      base_name <- paste(input[["model"]], input[["experiment"]], input[["ensemble"]], input[["grid"]], input[["time"]], sep  = '_')
      
      # Messages
      print('------------------------------------------------') 
      print(base_name)  
      
      # Extract time information by selecting one of the CMIP data files to extract 
      # time information from arbitrarily. 
      time <- format_time(nc_open(input[['gpp']])) 
    
      # Calculate the land area (remove the ocean area form the total cell area.)
      area_nc <- generate_land_area(input, base_name, intermed_dir, cdo_exe)
      
      # Create the intermediate netcdfs. 
      gpp_point       <- file.path(intermed_dir, paste0(base_name, '_gpp-LatLon.nc'))
      raRoot_point    <- file.path(intermed_dir, paste0(base_name, '_raRoot-LatLon.nc'))
      rhSoil_point    <- file.path(intermed_dir, paste0(base_name, '_rhSoil-LatLon.nc'))
      area_point      <- file.path(intermed_dir, paste0(base_name, '_area-LatLon.nc'))
      
      apply(coord, 1, function(coord_input){
        
        Long <- round(as.numeric(coord_input[['Longitude']]), digits = 4)
        Latt <- round(as.numeric(coord_input[['Latitude']]), digits = 4) 
        
        LatLon <- paste0('remapnn,lon=',Long,'_lat=',Latt)
        
        system2(cdo_exe, args = c(LatLon, input[['gpp']], gpp_point), stdout = TRUE, stderr = TRUE)
        system2(cdo_exe, args = c(LatLon, input[['raRoot']], raRoot_point), stdout = TRUE, stderr = TRUE)
        system2(cdo_exe, args = c(LatLon, input[['rhSoil']], rhSoil_point), stdout = TRUE, stderr = TRUE)
        system2(cdo_exe, args = c(LatLon, area_nc, area_point), stdout = TRUE, stderr = TRUE)
        
        # Create a data table of the variable values at a single coordinate location for 
        # the observed data. 
        coord_value_list <- mapply(FUN = function(nc_file, var){
          
          nc    <- nc_open(nc_file)
          
          data.table(value = ncvar_get(nc, var), 
                     units = ncatt_get(nc, var)$unit, 
                     variable = var) %>% 
            cbind(time)
          
        },
        nc_file = c(gpp_point, raRoot_point,  rhSoil_point, area_point),
        var = c('gpp', 'raRoot', 'rhSoil', 'areacella'),
        SIMPLIFY = FALSE)  
          # Format the results into a single long format data frame with 
          # coordinate identifier information.
        
        coord_value_list %>% 
          bind_rows %>% 
          mutate(Longitude = coord_input[['Longitude']], 
                Latitude = coord_input[['Latitude']], 
                source =  coord_input[['source']]) 
        
        
      }) -> 
        list_all_coords
      
      

        # Concatenate all of the extracted cooridnate values into a single data frame 
        # that has CMIP information. 
        bind_rows(list_all_coords) %>% 
        mutate(model = input[["model"]],
               experiment = input[["experiment"]], 
               ensemble = input[["ensemble"]], 
               grid = input[["grid"]]) -> 
          out 
        
      
      write.csv(x = out, file = file.path(intermed_dir, paste0(base_name, '-LatLon.csv')), row.names = FALSE)
        out
    }, error = function(e){data.table(model = input[["model"]], 
                                      experiment = input[["experiment"]], 
                                      ensemble = input[["ensemble"]],
                                      time = input[["time"]], 
                                      problem = TRUE)}) # End of the try catch 
    
  }) )
  
}



# Calculate the weighted ibal mean for the land variables using cdo. 
# Arguments 
#   dataframe: A dataframe containing the CMIP data process and the land fraction and cell area netcdfs (because of the way that this function is set up 
#              it will only calculate the weighted mean for over the land areas, will need to modify to allow for fexlibility to process area over oceans)
#   intermed_dir: The path to the directory as to where to store the intermediate netcdf files. 
#   cdo_exe: The path to the CDO EXE 
#   cleanup: A TRUE/FASLE argument that controls if the intermeidate netcdfs are cleaned  up or not, the default is set to leave intermediates in place.
# Returns: A data frame of the area weighted global average.  
# cdo_weighted_global_mean <- function(dataframe, intermed_dir, cdo_exe, cleanup = FALSE){
#   
#   # Check the inputs. 
#   assertthat::assert_that(dir.exists(intermed_dir)) 
#   assertthat::assert_that(file.exists(cdo_exe))
#   assertthat::assert_that(all(c('file', 'type', 'domain', 'variable', 'model', 'experiment', 'ensemble', 
#                                 'grid', 'time', 'areacella', 'sftlf') %in% names(dataframe)))
#   
#   split(dataframe, interaction(dataframe$model, dataframe$experiment, dataframe$ensemble, dataframe$variable, dataframe$time, drop = TRUE)) %>% 
#     lapply(X =., function(input = X){
#       
#       
#       # Define the base name to use for the intermediate files that are saved during this process. 
#       info <- distinct(input[, which(names(input) %in% c("variable", "domain", "model", "experiment", "ensemble", "grid", "time"))])
#       assertthat::assert_that(nrow(info) == 1)
#       
#       basename <- paste(info, collapse = '_')
#       print('--------')
#       print(basename)
#       
#       area_weights <- generate_land_area(input, basename, intermed_dir, cdo_exe)
#   
#       DataGridArea_nc <- file.path(intermed_dir, paste0(basename, '_DataGridArea.nc'))
#       WeightedMean_nc <- file.path(intermed_dir, paste0(basename, '_WeightedMean.nc'))
#       
#       #system2(cdo_exe, args = c('copy', input[['file']], Concat_nc), stdout = TRUE, stderr = TRUE)
#       system2(cdo_exe, args = c(paste0("setgridarea,", area_weights), input[['file']], DataGridArea_nc), stdout = TRUE, stderr = TRUE)
#       system2(cdo_exe, args = c(paste0("fldmean -setgridarea,", area_weights), input[['file']], WeightedMean_nc), stdout = TRUE, stderr = TRUE )
#       
#       # Extract the area inforamtion 
#       area <- sum(ncvar_get(nc_open(area_weights), 'areacella'))
#       area_units <- ncatt_get(nc_open(area_weights), 'areacella')$units 
#       
#       
#       # Extract data and format output.
#       assertthat::assert_that(file.exists(WeightedMean_nc))
#       nc <- nc_open(WeightedMean_nc)
#       
#       format_time(nc) %>% 
#         cbind(value = ncvar_get(nc, info[['variable']]), 
#               value_units = ncatt_get(nc, info[['variable']])$units, 
#               info) %>% 
#         mutate(area = area, 
#                area_units = area_units) 
#       
#       
#     }) %>%  
#     bind_rows() -> 
#     output 
#   
#   
#   if(cleanup){
#     file.remove(list.files(intermed_dir, '.nc', full.names = TRUE)) 
#   }
#   
#   output
#   
# } 


