# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  June 2021
# Version 0.1
#
#' stop_quietly exit program without error
#'
#' @rdname stop_quietly
#' @return stop execution of package
stop_quietly <- function() {
  
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
  
}

#
#' transY The default is to log transform the x argument:
#'
#' @param x vector
#' @param inverse if TRUE inverse
#' @rdname transY
#' @return A data.frame merged covariates
transY <- function(x, inverse=FALSE) {
  if (!inverse) {
    return( log(x) )
  } else {
    ##  Otherwise we backtransform it by exponentiating it:
    return( exp(x) )
  }
}


#
#' log_info save log into the file
#' @param prank type of log
#' @param stext Text to log
#' @param verbose If FALSE then the progress will be shown
#' @param log If FALSE then the progress will be shown
#' @rdname log_info
#' @return save log into the file
log_info <- function(prank, stext, verbose=FALSE, log=FALSE){
  
  if (verbose){
    print(stext)
  }

  if (log){
    if (file.exists(getOption("pj.output.dir"))){
      log_con <- file(file.path(getOption("pj.output.dir"), "logs.txt"), open="a")
      cat(paste0( format(Sys.time(), "%d %X"), 
                  " :: ", 
                  paste0(prank, " :: ", stext)), 
          file = log_con, sep="\n")
      close(log_con)
    }
  }
}



# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  June 2021
# Version 0.1
#
#' create_dir creating dir
#' @param x name of the folder to be created
#' @param verbose If FALSE then the progress will be shown    
#' @rdname create_dir
create_dir <- function(x, verbose){
  
  x.covariates <- file.path(x,"covariates")
  
  if(!file.exists( x.covariates )) {
    
    msg <- paste0("Log :: Creating directory ",x.covariates,".")
    
    dir.create(x.covariates, 
               recursive = TRUE,
               showWarnings = FALSE)
    
    if (verbose) message(msg)
  }

}


# Authors: Maksym Bondarenko mb4@soton.ac.uk
# Date :  June 2021
# Version 0.1
#
#' tmDiff return a time difference in human  reading format
#' @param start time when started
#' @param end time when endted
#' @param frm format of return      
#' @rdname tmDiff
tmDiff <- function(start, end, frm="hms") {
  ##  Function which takes time objects and calculates the difference between 
  ##  the start and end time point. Returned is the formatted time.
  dsec <- as.numeric(difftime(end, start, units = c("secs")))
  hours <- floor(dsec / 3600)
  
  if (frm == "hms" ){
    minutes <- floor((dsec - 3600 * hours) / 60)
    seconds <- dsec - 3600*hours - 60*minutes
    
    out=paste0(
      sapply(c(hours, minutes, seconds), function(x) {
        formatC(x, width = 2, format = "d", flag = "0")
      }), collapse = ":")
    
    return(out)
  }else{
    return(hours)
  }
}



#' creat_raster_stack Create a raster stack from all covariates from popfit and census_mask
#' and water_raster:
#' @param covariates covariates list
#' @param popfit_final list of names used in the RF
#' @param census_mask raster of census mask
#' @param water_raster raster of water mask
#' @importFrom utils stack
#' @importFrom raster raster
#' @rdname creat_raster_stack
#' @return raster stack
creat_raster_stack <- function(covariates, 
                               popfit_final, 
                               census_mask, 
                               water_raster) {
  ##  Create an empty list to hold the rasters:
  list_ras <- list()
  
  ##  For every raster name in the list of names used in the RF:
  for (i in 1:length(names(popfit_final$forest$xlevels))){
    ##  Retrieve the name:
    var_name <- names(popfit_final$forest$xlevels)[i]
    r <- raster( covariates[[var_name]]$path )
    names(r) <- var_name
    list_ras[[i]] <-  r
  }  
  ##  Append the census mask and the water mask to that list:
  names(census_mask) <- "census_mask"
  list_ras[[length(list_ras)+1]] <- census_mask
  names(water_raster) <- "water_raster"
  list_ras[[length(list_ras)+1]] <- water_raster
  
  ##  Stack all the rasters we just retrieved:
  ras_stack <- stack(list_ras)
  
  ##  Return the raster stack object:
  return(ras_stack)
}


#' This function allows for precision decimal formatting.
#' @param x number 
#' @param k format
#' @rdname specify_decimal
#' @return number
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)


#' Checking extent of two rasters
#' @param x path to raster  
#' @param y path to raster  
#' @rdname check_raster_extend
#' @return logical
check_raster_extend <- function( x, y ){
  
  r1 <- raster::raster(x)
  r2 <- raster::raster(y)
  xmin.r1 <- specify_decimal( raster::bbox(r1)[1,1],5 )
  xmax.r1 <- specify_decimal( raster::bbox(r1)[1,2],5 )
  ymin.r1 <- specify_decimal( raster::bbox(r1)[2,1],5 )
  ymax.r1 <- specify_decimal( raster::bbox(r1)[2,2],5 )
  
  xmin.r2 <- specify_decimal( raster::bbox(r2)[1,1],5 )
  xmax.r2 <- specify_decimal( raster::bbox(r2)[1,2],5 )
  ymin.r2 <- specify_decimal( raster::bbox(r2)[2,1],5 )
  ymax.r2 <- specify_decimal( raster::bbox(r2)[2,2],5 )
  
  if ( (xmin.r1) != (xmin.r2) | (xmax.r1) != (xmax.r2) | (ymin.r1) != (ymin.r2) | (ymax.r1) != (ymax.r2)  ) {
    return(FALSE)
  }else{
    return(TRUE)
  }
  
}


#' Changing extent of two rasters.
#' @param srcfile path to raster 
#' @param dstfile path to raster 
#' @param verbose logical. Should report extra information on progress? 
#' @param overwrite logical to overwrite or not the output file  
#' @rdname check_raster_extend
#' @return logical
change_raster_extend <- function(srcfile, 
                                 dstfile, 
                                 verbose=FALSE, 
                                 overwrite=FALSE){
  
  
  r1 <- raster::raster(srcfile)
  ##  Changing extent of two rasters using gdal_warp.
  xmin <- raster::bbox(r1)[1,1]
  xmax <- raster::bbox(r1)[1,2]
  ymin <- raster::bbox(r1)[2,1]
  ymax <- raster::bbox(r1)[2,2]
  
  rFileName <- basename(dstfile) 
  rPath <- dirname(dstfile)
  
  te <- paste0(' ',xmin,' ',ymin,' ',xmax,' ',ymax)
  
  rToPath <- file.path(rPath, paste0("tmp_",rFileName))
  gdalwarp(dstfile,
           te=te,
           co=c("COMPRESS=LZW","BLOCKXSIZE=512","BLOCKYSIZE=512", "TILED=YES", "BIGTIFF=YES"),
           s_srs=crs(r1),
           rToPath,
           output_Raster=TRUE,
           overwrite=TRUE,
           verbose=verbose)
  
  
  if(file.exists(dstfile)){ unlink(dstfile , recursive = TRUE, force = FALSE)}
  
  file.rename(from=rToPath, to=dstfile)
}



# creat_raster_stack <- function(covariates, 
#                                popfit_final, 
#                                census_mask, 
#                                water_raster) {
#   ##  Create an empty list to hold the rasters:
#   list_ras <- list()
#   
#   ##  For every raster name in the list of names used in the RF:
#   for (i in 1:length(names(popfit_final$forest$xlevels))){
#     ##  Retrieve the name:
#     var_name <- names(popfit_final$forest$xlevels)[i]  
#     ##  Assign that raster path to the varname:
#     base::assign(var_name, raster( covariates[[var_name]]$path ), envir = .GlobalEnv)
#     ##  Put the raster path in the list:
#     list_ras[[i]] <-  get(var_name, envir = .GlobalEnv)
#   }  
#   ##  Append the census mask and the water mask to that list:
#   list_ras[[length(list_ras)+1]] <- census_mask
#   list_ras[[length(list_ras)+1]] <- water_raster
#   
#   ##  Stack all the rasters we just retrieved:
#   ras_stack <- stack(list_ras)
#   
#   ##  Return the raster stack object:
#   return(ras_stack)
# }

#' Create project directory based on tag of the countries.
#' @param input.countries input list of the countries 
#' @param output_dir path to project dir 
#' @rdname create_dirs_for_prj
#' @return list of project directories
create_dirs_for_prj <- function(input.countries, output_dir){
  
  
  countries_tag_output <- paste(input.countries, collapse="_")
  
  subDir.country <- file.path(output_dir, countries_tag_output, "tmp")
  
  if(!file.exists(subDir.country)){ 
    message("Info :: Creating dir ", subDir.country)
    dir.create(subDir.country, recursive = TRUE, showWarnings = FALSE) 
  }
  
  subDir.country.output.zst <- file.path(output_dir, countries_tag_output, "zonal_stats")
  
  if(!file.exists(subDir.country.output.zst)){ 
    message("Info :: Creating dir ", subDir.country.output.zst)
    dir.create(subDir.country.output.zst, recursive = TRUE, showWarnings = FALSE) 
  }
  
  subDir.countrys.merged <- file.path(output_dir, countries_tag_output, "merged")
  
  if (length(input.countries) > 1){
    
    if(!file.exists(subDir.countrys.merged)){ 
      message("Info :: Creating dir ", subDir.countrys.merged)
      dir.create(subDir.countrys.merged, recursive = TRUE, showWarnings = FALSE) 
    }  
  }
  
  
  ##  Return a list of objects which represent the output path, the data path, 
  ##  and the country(ies) tag:
  return(list(countries_tag = countries_tag_output,
              output = file.path(output_dir, countries_tag_output),
              tmp = subDir.country,
              data_cvr = subDir.country.output.zst,
              data_merged = subDir.countrys.merged
  ))  
  
}

#' Checking if covariates extend matches mastergrid.
#' @param covariates list of covariates 
#' @param fix_cov logical
#' @param verbose logical. Should report extra information on progress?
#' @param log logical. Should report on progress be saved in log file?
#' @rdname check_cov
#' @return logical if fix_raster is FALSE
check_cov <- function(covariates, 
                      fix_cov,
                      verbose=FALSE, 
                      log=FALSE){

    for ( i in names(covariates) ) {
      
      mdir <-  covariates[[i]]$mastergrid$dataset_folder
      mfile <-  covariates[[i]]$mastergrid$dataset_filename
      mpath <- file.path(mdir, mfile)
      
      
      for (j in names(covariates[[i]]) ){
        
        if (j == "mastergrid") next
        
        fdir <-  covariates[[i]][[j]]$dataset_folder
        ffile <-  covariates[[i]][[j]]$dataset_filename
        fpath <- file.path(fdir,ffile)
        
        
        if (!check_raster_extend(mpath, fpath)){
          
          if (fix_cov){
            log_info("Warning", paste0("Will try to fix ", ffile ," extend to match mastergrid extend."), verbose=verbose, log=log)
            change_raster_extend(mpath,fpath)
            return(FALSE)
          }else{
            
            log_info("Error", paste0("Covariate ", ffile ," extend is not match mastergrid extend."), verbose=verbose, log=log)
            
            if (!verbose){
              message(paste0("Covariate ", ffile ," extend is not match mastergrid extend."))  
            }

            return(TRUE)
          } 
        }
        
      }
    }
  
  return(FALSE)
 
}